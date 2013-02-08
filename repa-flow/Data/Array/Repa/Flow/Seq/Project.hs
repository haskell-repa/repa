
module Data.Array.Repa.Flow.Seq.Project
        (gather)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import GHC.Exts


-- | Takes a function to get elements and a flow of indices, 
--   and produces a flow of elements corresponding to each index.
gather :: (Int# -> a) -> Flow mode Int -> Flow mode a
gather !get (Flow start size report get1 get8)
 = Flow start size report' get1' get8'
 where
        report' state
         = do   r       <- report state
                return  $ R.Gather r
        {-# NOINLINE report' #-}

        get1' !state push1
         =  get1 state $ \r
         -> case r of
                Yield1 (I# ix) hint
                 -> push1 $ Yield1 (get ix) hint

                Done  -> push1 Done
        {-# INLINE get1' #-}

        get8' state push8
         =  get8 state $ \r
         -> case r of
                Yield8 (I# ix0) (I# ix1) (I# ix2) (I# ix3)
                       (I# ix4) (I# ix5) (I# ix6) (I# ix7)
                 -> push8 $ Yield8 (get ix0) (get ix1) (get ix2) (get ix3)
                                   (get ix4) (get ix5) (get ix6) (get ix7)
                Pull1
                 -> push8 $ Pull1
        {-# INLINE get8' #-}

{-# INLINE [1] gather #-}
