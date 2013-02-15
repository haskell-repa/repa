
module Data.Array.Repa.Flow.Seq.Operator.Map
        ( map
        , comap)
where
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.CoFlow
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude hiding (map, zip, zipWith)


------------------------------------------------------------------------------
-- | Apply a function to every element of a flow.
map :: (a -> b) -> Flow mode a -> Flow mode b
map f (Flow start size report get1 get8)
 = Flow start size report' get1' get8'
 where  
        report' state
         = do   r       <- report state
                return  $ R.Map r
        {-# NOINLINE report' #-}

        get1' state push1
         =  get1 state $ \r 
         -> case r of
                Yield1 x hint   -> push1 $ Yield1 (f x) hint
                Done            -> push1 $ Done
        {-# INLINE get1' #-}

        get8' state push8
         =  get8 state $ \r
         -> case r of
                Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                 -> push8 $ Yield8      (f x0) (f x1) (f x2) (f x3)
                                        (f x4) (f x5) (f x6) (f x7)

                Pull1           -> push8 $ Pull1
        {-# INLINE get8' #-}

{-# INLINE [1] map #-}


-------------------------------------------------------------------------------
-- | Apply a function to every element of a coflow.
comap :: (a -> b) -> CoFlow mode b -> CoFlow mode a
comap f (CoFlow cfstate eject feed1 feed8)
 = CoFlow cfstate eject feed1' feed8'
 where
        feed1' state (Snack1 x)
         = feed1 state (Snack1 (f x))

        feed8' state (Snack8 x0 x1 x2 x3 x4 x5 x6 x7)
         = feed8 state (Snack8  (f x0) (f x1) (f x2) (f x3)
                                (f x4) (f x5) (f x6) (f x7))
