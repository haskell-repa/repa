
module Data.Array.Repa.Flow.Par.Flow
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
import qualified Data.Array.Repa.Flow.Seq.Flow  as Seq
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
        let here        = "par.unflow"
        let !len        = distroBalancedLength distro
        let !getStart   = distroBalancedFragStart distro

        -- Allocate a mutable vector to hold the final results.
        !mvec           <- unew $ I# len

        -- Start the parallel flow
        !statePar       <- startPar

        -- The action that runs on each thread.
        let {-# INLINE action #-}
            action tid
             = case frag statePar tid of
                Seq.Flow startSeq _size _flowReport get1 get8
                 -> do  -- The starting point for this threads results into 
                        -- the final vector.
                        let !ixStart    = getStart tid

--                        putStrLn $ "unflow[BB][tid = " ++ show (I# tid) ++ "]: start"
                        stateSeq  <- startSeq
--                        reportSeq <- flowReport stateSeq

--                        putStrLn $ "unflow[BB][tid = " ++ show (I# tid) ++ "]: " ++ show reportSeq

                        -- The 'slurp' function below calls on this to write
                        -- results into the destination vector.
                        let write ix val
                                = uwrite here mvec (I# (ixStart +# ix)) val

                        _         <- Seq.slurp 
                                        0# 
                                        Nothing 
                                        write
                                        (get1 stateSeq)
                                        (get8 stateSeq)
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
        let here        = "par.unflow"
        let !frags      = distroUnbalancedFrags distro

        -- Allocate a boxed mutable vector to hold the result from each thread.
        --  Each thread will write its own unboxed vector.
        !mchunks        <- vnew (I# frags)

        -- Start the parallel flow
        !statePar       <- start

        -- The action that runs on each thread.
        let {-# INLINE action #-}
            action tid
             = case frag statePar tid of
                flowSeq@(Seq.Flow _startFlow _ _reportFlow _ _)
                 -> do  
--                        putStrLn $ "unflow[BN][tid = " ++ show (I# tid) ++ "]: start"
--                        stateSeq   <- startFlow
--                        reportSeq  <- reportFlow stateSeq
--                        putStrLn $ "unflow[BN][tid = " ++ show (I# tid) ++ "]: " ++ show reportSeq
                        
                        let new ix        = unew (I# ix)
                        let write mvec ix = uwrite here mvec (I# ix)
                        (mvec, len)       <- Seq.drain new write flowSeq
                        !vec              <- ufreeze mvec
                        let !vec'         =  uslice 0 len vec

                        vwrite here mchunks (I# tid) vec'
                        return ()


        -- Run the actions to compute each chunk.
        gangIO gang action

        -- TODO: rubbish concat needs to die
        --       to a scan to work out target index 
        --       then each thread should copy its results into the final vector
        --       API should take an update function, 
        --       only use unboxed vectors internally.
        let {-# NOINLINE unflow_concat #-}
            unflow_concat mchunks'
             = do chunks          <- vfreeze mchunks'
                  return  $ U.concat $ V.toList chunks

        unflow_concat mchunks

 {-# INLINE [1] unflow #-}

