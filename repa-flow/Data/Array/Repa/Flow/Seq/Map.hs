
module Data.Array.Repa.Flow.Seq.Map
        ( map
        , zip,          zipLeft
        , zipWith,      zipLeftWith)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import qualified Data.Vector.Unboxed.Mutable            as UM
import Prelude hiding (map, zip, zipWith)
import GHC.Exts

------------------------------------------------------------------------------
-- | Apply a function to every element of a flow.
map :: (a -> b) -> Flow r a -> Flow r b
map f (Flow start size report get1 get8)
 = Flow start size report' get1' get8'
 where  
        report' state
         = do   r       <- report state
                return  $ R.Map r

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

                Pull1
                 -> push8 $ Pull1
        {-# INLINE get8' #-}

{-# INLINE [1] map #-}


-------------------------------------------------------------------------------
-- | Combine two flows into a flow of tuples, pulling one element at a time.
zip :: Flow r a -> Flow r b -> Flow r (a, b)            -- TODO: compute upper bound
zip    (Flow !startA !sizeA reportA getA1 _)
       (Flow !startB !sizeB reportB getB1 _)
 = Flow start' size' report' get1' get8'
 where
        start'
         = do   stateA  <- startA
                stateB  <- startB
                return  (stateA, stateB)
        {-# INLINE start' #-}

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

{-# INLINE [1] zip #-}


-------------------------------------------------------------------------------
-- | Combine two flows with a function, pulling one element at a time.
zipWith :: (a -> b -> c) -> Flow r a -> Flow r b -> Flow r c    -- TODO: compute upper bound
zipWith f flowA flowB
        = map (uncurry f) $ zip flowA flowB
{-# INLINE [1] zipWith #-}


--------------------------------------------------------------------------------
-- | Pair elements of a flow with elements gained from some function.
--
--   Unlike plain 'zip', the fact that we can use the function to retrieve
--   an arbitary number of elements per step means we can pull up to 
--   8 elements at a time from the resulting flow.
--
zipLeft :: Flow r a -> (Int# -> b) -> Flow r (a, b)
zipLeft (Flow startA sizeA reportA getA1 getA8) getB
 = Flow start' size' report' get1' get8'
 where  
        start'
         = do   stateA  <- startA
                refIx   <- UM.unsafeNew 1
                UM.unsafeWrite refIx 0 (0 :: Int)
                return (stateA, refIx)

        size' (!stateA, _)
         =      sizeA stateA

        report' (stateA, _)
         = do   r       <- reportA stateA
                return  $ R.ZipLeft r
        {-# NOINLINE report' #-}
        
        get1' (!stateA, !refIx) push1
         =  getA1 stateA $ \r 
         -> case r of
                Yield1 x1 hint
                 -> do  !(I# ix)        <- UM.unsafeRead refIx 0
                        UM.unsafeWrite refIx 0 (I# (ix +# 1#))
                        push1 $ Yield1 (x1, getB ix) hint

                Done -> push1 $ Done


        get8' (!stateA, !refIx) push8
         = getA8 stateA $ \r
         -> case r of
                Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                 -> do  !(I# ix)        <- UM.unsafeRead refIx 0
                        UM.unsafeWrite refIx 0 (I# (ix +# 8#))
                        push8 $ Yield8  (x0, getB (ix +# 0#))
                                        (x1, getB (ix +# 1#))
                                        (x2, getB (ix +# 2#))
                                        (x3, getB (ix +# 3#))
                                        (x4, getB (ix +# 4#))
                                        (x5, getB (ix +# 5#))
                                        (x6, getB (ix +# 6#))
                                        (x7, getB (ix +# 7#))

                Pull1 -> push8 Pull1

{-# INLINE [1] zipLeft #-}


-------------------------------------------------------------------------------
-- | Combine a flow and elements gained from some function.
zipLeftWith :: (a -> b -> c) -> Flow r a -> (Int# -> b) -> Flow r c
zipLeftWith f flowA getB
        = map (uncurry f) $ zipLeft flowA getB
{-# INLINE [1] zipLeftWith #-}

