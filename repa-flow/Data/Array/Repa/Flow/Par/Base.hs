
module Data.Array.Repa.Flow.Par.Base
        ( module Data.Array.Repa.Flow.Base
        , Flow   (..)
        , Distro (..)
        , flow
        , Unflow (..))
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Bulk.Gang
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Flow.Base
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector                    as V
import qualified Data.Array.Repa.Flow.Seq       as Seq
import qualified Data.Array.Repa.Flow.Seq.Base  as Seq
import System.IO.Unsafe
import GHC.Exts

here = "Data.Array.Repa.Flow.Par.Base"


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
flow    :: Elt a 
        => Gang 
        -> (Int# -> a)          -- ^ Get the element at the given index.
        -> Int#                 -- ^ Total length of flow.
        -> Flow Seq.FD BB a

flow !gang !load !len
 = let  !frags          = gangSize gang
        !distro         = balanced frags len
        !fragStart      = distroBalancedFragStart  distro
        !fragLength     = distroBalancedFragLength distro

        frag _ tid
         = let  load' ix = load (ix +# fragStart tid)
                len'     = fragLength tid
           in   Seq.flow load' len'
        {-# INLINE frag #-}

   in   Flow    { flowGang      = gang
                , flowDistro    = balanced frags len 
                , flowStart     = return ()
                , flowFrag      = frag }
{-# INLINE [1] flow #-}
        

-------------------------------------------------------------------------------
class Unflow dist where
 unflow :: (Elt a, U.Unbox a) 
        => Flow Seq.FD dist a -> U.Vector a


-- | Unflowing a balanced computation allows results to be written directly
--   to the final vector without intermediate copying.
instance Unflow BB where
 unflow !(Flow gang distro startPar frag)
  = unsafePerformIO
  $ do  
        let !len        = distroBalancedLength distro
        let !getStart   = distroBalancedFragStart distro

        -- Allocate a mutable vector to hold the final results.
        !mvec           <- unew $ I# len

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
                        = uwrite here mvec (I# (ixStart +# ix)) val

                  case frag statePar tid of
                   Seq.Flow startSeq _size _report get1 get8
                    -> do stateSeq <- startSeq
                          _        <- Seq.slurp 0# Nothing write
                                        (get1 stateSeq) (get8 stateSeq)
                          return ()

        -- Run the actions to write results into the target vector.
        gangIO gang action

        ufreeze mvec
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
        !mchunks        <- vnew (I# frags)

        -- Start the parallel flow
        !state          <- start

        -- The action that runs on each thread.
        let action tid
             = do uvec  <- Seq.drain (frag state tid)
                  vwrite here mchunks (I# tid) uvec
                  return ()

        -- Run the actions to compute each chunk.
        gangIO gang action

        -- TODO: rubbish concat needs to die
        --       to a scan to work out target index 
        --       then each thread should copy its results into the final vector
        --       API should take an update function, 
        --       only use unboxed vectors internally.
        chunks          <- vfreeze mchunks
        return  $ U.concat $ V.toList chunks
 {-# INLINE [1] unflow #-}

