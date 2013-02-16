
module Data.Array.Repa.Flow.Seq.Operator.Zip
        ( zip_ii
        , zipWith_ii
        , zipLeft_i
        , zipLeftWith_i)
where
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.Operator.Map
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude hiding (map, zip, zipWith)
import GHC.Exts


-------------------------------------------------------------------------------
-- | Combine two flows into a flow of tuples, pulling one element at a time.
zip_ii :: Flow mode a -> Flow mode b -> Flow mode (a, b)
zip_ii (Flow !fstateA !sizeA reportA getA1 _)
       (Flow !fstateB !sizeB reportB getB1 _)
 = Flow fstate' size' report' get1' get8'
 where
        fstate'
         = joinFlowStates fstateA fstateB
        {-# INLINE fstate' #-}

        size' (stateA, stateB)
         = do   szA     <- sizeA stateA
                szB     <- sizeB stateB
                return  $  sizeMin szA szB
        {-# INLINE size' #-}


        report' (stateA, stateB)
         = do   rA      <- reportA stateA
                rB      <- reportB stateB
                return  $ R.Zip rA rB
        {-# NOINLINE report' #-}


        get1' (stateA, stateB) push1
         =  getA1 stateA $ \mxA 
         -> getB1 stateB $ \mxB
         -> case (mxA, mxB) of
                (Yield1 xA hintA, Yield1 xB hintB) 
                  -> push1 $ Yield1 (xA, xB) (hintA && hintB)
                _ -> push1 $ Done
        {-# INLINE get1' #-}


        -- We can't provide an 8-way zip because one of the flows
        -- might want to dynamically give us only 1 element at a time.
        get8' _ push8
         = push8 Pull1
        {-# INLINE get8' #-}
{-# INLINE [1] zip_ii #-}


-------------------------------------------------------------------------------
-- | Combine two flows with a function, pulling one element at a time.
zipWith_ii
        :: (a -> b -> c) 
        -> Flow mode a -> Flow mode b 
        -> Flow mode c

zipWith_ii f flowA flowB
        = map_i (uncurry f) $ zip_ii flowA flowB
{-# INLINE [1] zipWith_ii #-}


--------------------------------------------------------------------------------
-- | Pair elements of a flow with elements gained from some function.
--
--   Unlike plain 'zip', the fact that we can use the function to retrieve
--   an arbitary number of elements per step means we can pull up to 
--   8 elements at a time from the resulting flow.
--
zipLeft_i
        :: Flow FD a 
        -> (Int# -> b) 
        -> Flow FD (a, b)

zipLeft_i (Flow fstateA sizeA reportA getA1 getA8) getB
 = Flow fstate' size' report' get1' get8'
 where  
        here    = "seq.zipLeft"

        fstate'
         = case fstateA of
                FlowStateDelayed startA
                 -> FlowStateDelayed
                 $  do  stateA  <- startA
                        refIx   <- unew 1
                        iwrite here refIx 0# 0#
                        return (stateA, refIx)
        {-# INLINE fstate' #-}


        size' (!stateA, _)
         =      sizeA stateA
        {-# INLINE size' #-}


        report' (stateA, _)
         = do   r       <- reportA stateA
                return  $ R.ZipLeft r
        {-# NOINLINE report' #-}

        
        get1' (!stateA, !refIx) push1
         =  getA1 stateA $ \r 
         -> case r of
                Yield1 x1 hint
                 -> do  !(I# ix)        <- uread here refIx 0
                        iwrite here refIx 0# (ix +# 1#)
                        push1 $ Yield1 (x1, getB ix) hint

                Done  -> push1 Done
        {-# INLINE get1' #-}


        get8' (!stateA, !refIx) push8
         = getA8 stateA $ \r
         -> case r of
                Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                 -> do  !(I# ix)        <- uread here refIx 0
                        iwrite here refIx 0# (ix +# 8#)
                        push8 $ Yield8  (x0, getB (ix +# 0#))
                                        (x1, getB (ix +# 1#))
                                        (x2, getB (ix +# 2#))
                                        (x3, getB (ix +# 3#))
                                        (x4, getB (ix +# 4#))
                                        (x5, getB (ix +# 5#))
                                        (x6, getB (ix +# 6#))
                                        (x7, getB (ix +# 7#))

                Pull1 -> push8 Pull1
        {-# INLINE get8' #-}
{-# INLINE [1] zipLeft_i #-}


-------------------------------------------------------------------------------
-- | Combine a flow and elements gained from some function.
zipLeftWith_i
        :: (a -> b -> c) 
        -> Flow FD a 
        -> (Int# -> b) 
        -> Flow FD c

zipLeftWith_i f flowA getB
        = map_i (uncurry f) $ zipLeft_i flowA getB
{-# INLINE [1] zipLeftWith_i #-}

