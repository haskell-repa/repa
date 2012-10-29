
module Data.Array.Repa.Flow.Map
        ( map
        , zip, zipWith)
where
import Data.Array.Repa.Flow.Base
import Prelude hiding (map, zip, zipWith)

------------------------------------------------------------------------------
-- | Apply a function to every element of a flow.
map :: (a -> b) -> Flow a -> Flow b
map f (Flow getSize get1 get8)
 = Flow getSize get1' get8'
 where  
        get1' push1
         =  get1 $ \r 
         -> case r of
                Just x  -> push1 $ Just (f x)
                Nothing -> push1 $ Nothing
        {-# INLINE get1' #-}

        get8' push8
         =  get8 $ \r
         -> case r of
                Left (x0, x1, x2, x3, x4, x5, x6, x7)
                 -> push8 $ Left (f x0, f x1, f x2, f x3, f x4, f x5, f x6, f x7)

                Right len
                 -> push8 $ Right len
        {-# INLINE get8' #-}

{-# INLINE [1] map #-}


-------------------------------------------------------------------------------
-- | Combine two flows into a flow of tuples.
zip :: Flow a -> Flow b -> Flow (a, b)
zip    (Flow !getSizeA getA1 getA8)
       (Flow !getSizeB getB1 getB8)
 = Flow getSize' get1' get8'
 where
        getSize' _
         = do   sizeA   <- getSizeA ()
                sizeB   <- getSizeB ()
                return  $  sizeMin sizeA sizeB
        {-# INLINE getSize' #-}

        get1' push1
         =  getA1 $ \mxA 
         -> getB1 $ \mxB
         -> case (mxA, mxB) of
                (Just xA, Just xB) -> push1 $ Just (xA, xB)
                _                  -> push1 $ Nothing
        {-# INLINE get1' #-}

        get8' push8
         =  getA8 $ \mxA
         -> getB8 $ \mxB
         -> case (mxA, mxB) of
                ( Left (a0, a1, a2, a3, a4, a5, a6, a7)
                 ,Left (b0, b1, b2, b3, b4, b5, b6, b7))
                 -> push8 $ Left  ( (a0, b0), (a1, b1), (a2, b2), (a3, b3)
                                  , (a4, b4), (a5, b5), (a6, b6), (a7, b7))

                ( Right len, _) 
                 -> push8 $ Right len

                ( _, Right len) 
                 -> push8 $ Right len
        {-# INLINE get8' #-}

{-# INLINE [1] zip #-}


-------------------------------------------------------------------------------
-- | Combine two flows with a function.
zipWith :: (a -> b -> c) -> Flow a -> Flow b -> Flow c
zipWith f flowA flowB
        = map (uncurry f) $ zip flowA flowB
{-# INLINE [1] zipWith #-}
