
module Data.Array.Repa.Flow.Par.Base
        ( Flow   (..)
        , Distro (..)
        , flow
        , Unflow (..))
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Bulk.Gang
import Data.Array.Repa.Flow.Par.Distro
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import qualified Data.Vector.Mutable            as VM
import qualified Data.Vector                    as V
import qualified Data.Array.Repa.Flow.Seq       as Seq
import qualified Data.Array.Repa.Flow.Seq.Base  as Seq
import System.IO.Unsafe
import GHC.Exts


data Flow rep bal a
        = forall state. Flow
        { -- | The Gang that the flow is attached to.
          flowGang      :: Gang

          -- | Flow distribution, keeps track of how well-balanced
          --   our workload is between the threads.
        , flowDistro    :: Distro bal

          -- | Start the flow.
          --   This returns a state value that needs to be passed to get
          --   functions.
        , flowStart     :: IO state

          -- | Get the sequential flow fragment that runs on the given thread.
        , flowFrag      :: state -> Int# -> Seq.Flow rep a }


-------------------------------------------------------------------------------
-- | Convert an unboxed vector
--   to a delayed, balanced, parallel flow.
flow    :: (Elt a, U.Unbox a) 
        => Gang -> U.Vector a -> Flow Seq.FD BB a
flow !gang !vec
 = let  !frags          = gangSize gang
        !(I# len)       = U.length vec

        !distro         = balanced frags len
        !fragStart      = distroBalancedFragStart  distro
        !fragLength     = distroBalancedFragLength distro

        frag _ tid
                = Seq.flow 
                $ U.unsafeSlice 
                        (I# (fragStart tid))
                        (I# (fragLength tid))
                        vec
        {-# INLINE frag #-}

   in   Flow    { flowGang      = gang
                , flowDistro    = balanced frags len 
                , flowStart     = return ()
                , flowFrag      = frag }
{-# INLINE [1] flow #-}
        

-------------------------------------------------------------------------------
class Unflow bal where
 unflow :: (Elt a, U.Unbox a) 
        => Flow Seq.FD bal a -> U.Vector a


-- | Unflowing a balanced computation allows results to be written directly
--   to the final vector without intermediate copying.
instance Unflow BB where
 unflow !(Flow gang distro startPar frag)
  = unsafePerformIO
  $ do  
        let !len        = distroBalancedLength distro
        let !getStart   = distroBalancedFragStart distro

        -- Allocate a mutable vector to hold the final results.
        !mvec           <- UM.unsafeNew $ I# len

        -- Start the parallel flow
        !statePar       <- startPar

        -- The action that runs on each thread.
        let action tid
             = do 
                  -- The starting point for this threads results into 
                  -- the final vector.
                  let !ixStart    = getStart tid

                  -- The 'slurp' function below calls on this to write
                  -- results into the destination vector.
                  let write (I# ix)  val
                        = UM.unsafeWrite mvec (I# (ixStart +# ix)) val

                  case frag statePar tid of
                   Seq.Flow startSeq _size _report get1 get8
                    -> do stateSeq <- startSeq
                          _        <- Seq.slurp 0# Nothing write
                                        (get1 stateSeq) (get8 stateSeq)
                          return ()

        -- Run the actions to write results into the target vector.
        gangIO gang action

        U.unsafeFreeze mvec
 {-# INLINE [1] unflow #-}


-- | Unflowing an unbalanced computation requires intermediate copying
--   to gather the intermediate results.
instance Unflow BN where
 unflow !(Flow gang distro start frag)
  = unsafePerformIO
  $ do  
        let !frags      = distroUnbalancedFrags distro

        -- Allocate a boxed mutable vector to hold the result from each thread.
        --  Each thread will write its own unboxed vector.
        !mchunks        <- VM.unsafeNew (I# frags)

        -- Start the parallel flow
        !state          <- start

        -- The action that runs on each thread.
        let action tid
             = do uvec  <- Seq.drain (frag state tid)
                  VM.unsafeWrite mchunks (I# tid) uvec
                  return ()

        -- Run the actions to compute each chunk.
        gangIO gang action

        -- TODO: rubbish concat needs to die
        --       to a scan to work out target index 
        --       then each thread should copy its results into the final vector
        chunks          <- V.unsafeFreeze mchunks
        return  $ U.concat $ V.toList chunks
 {-# INLINE [1] unflow #-}

