
module Data.Array.Repa.Flow.Map
        ( map
        , zip,          zipLeft
        , zipWith,      zipLeftWith)
where
import Data.Array.Repa.Flow.Base
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude hiding (map, zip, zipWith)
import Control.Monad
import GHC.Exts

------------------------------------------------------------------------------
-- | Apply a function to every element of a flow.
map :: (a -> b) -> Flow a -> Flow b
map f (Flow getSize get1 get8)
 = Flow getSize get1' get8'
 where  
        get1' push1
         =  get1 $ \r 
         -> case r of
                Yield1 x hint   -> push1 $ Yield1 (f x) hint
                Done            -> push1 $ Done
        {-# INLINE get1' #-}

        get8' push8
         =  get8 $ \r
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
zip :: Flow a -> Flow b -> Flow (a, b)
zip    (Flow !getSizeA getA1 _)
       (Flow !getSizeB getB1 _)
 = Flow getSize' get1' get8'
 where
        getSize'
         = do   sizeA   <- getSizeA
                sizeB   <- getSizeB
                return  $  sizeMin sizeA sizeB
        {-# INLINE getSize' #-}

        get1' push1
         =  getA1 $ \mxA 
         -> getB1 $ \mxB
         -> case (mxA, mxB) of
                (Yield1 xA hintA, Yield1 xB hintB) 
                  -> push1 $ Yield1 (xA, xB) (hintA && hintB)
                _ -> push1 $ Done
        {-# INLINE get1' #-}

        -- We can't provide an 8-way zip because one of the flows
        -- might want to dynamically give us only 1 element at a time.
        get8' push8
         = push8 Pull1
        {-# INLINE get8' #-}

{-# INLINE [1] zip #-}


-------------------------------------------------------------------------------
-- | Combine two flows with a function, pulling one element at a time.
zipWith :: (a -> b -> c) -> Flow a -> Flow b -> Flow c
zipWith f flowA flowB
        = map (uncurry f) $ zip flowA flowB
{-# INLINE [1] zipWith #-}


--------------------------------------------------------------------------------
-- | Pair elements of a flow with elements gained from some function.
--
--   Unlike plain 'zip', the fact that we can use the function to retrieve
--   an arbitary number of elements per step means we can pull up to 
--   8 elements at a time from the resulting flow.
zipLeft :: Flow a -> (Int# -> b) -> IO (Flow (a, b))
zipLeft (Flow !getSizeA getA1 getA8) getB
 = do
        refIx   <- UM.unsafeNew 1
        UM.unsafeWrite refIx 0 (0 :: Int)

        let 
         get1' push1
          =  getA1 $ \r 
          -> case r of
                Yield1 x1 hint
                 -> do  !(I# ix)        <- UM.unsafeRead refIx 0
                        UM.unsafeWrite refIx 0 (I# (ix +# 1#))
                        push1 $ Yield1 (x1, getB ix) hint

                Done -> push1 $ Done

         get8' push8
          = getA8 $ \r
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

        return $ Flow getSizeA get1' get8'
{-# INLINE [1] zipLeft #-}


-------------------------------------------------------------------------------
-- | Combine a flow and elements gained from some function.
zipLeftWith :: (a -> b -> c) -> Flow a -> (Int# -> b) -> IO (Flow c)
zipLeftWith f flowA getB
        = liftM (map (uncurry f)) $ zipLeft flowA getB
{-# INLINE [1] zipLeftWith #-}
