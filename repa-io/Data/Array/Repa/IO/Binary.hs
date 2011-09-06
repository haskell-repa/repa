{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Array.Repa.IO.Binary
        ()
where
import Foreign.Storable                 as S
import Data.Binary                      as B
import Data.Binary.Get                  as B
import Data.Array.Repa                  as R
import Prelude                          as P
import Data.Vector.Unboxed              as V


-- | Binary instance for Repa arrays.
--   When reading from the stream, if there are not a whole number of 
--   elements then `error`.
instance (Binary a, Storable a, Elt a) => Binary (Array DIM1 a) where
                   
 {-# INLINE get #-}     
 get 
  = do -- Determine the size of a single element.
        --  This size can be determined from the Storable dictionary we've
        --  been passed, but we need to go via a fake array to witness the
        --  element type.
        let (fake   :: Array DIM1 a) = R.fromList (Z:.0) []
        let (bytes1 :: Int)          = S.sizeOf (fake R.! (Z:.0))

        -- Determine how many elements the stream will give us.
        bytesTotal      <- remaining 
        let lenTotal    =  bytesTotal `div` (fromIntegral bytes1)
        let lenTotal'   = fromIntegral lenTotal
        
        vec             <- if bytes1 * lenTotal' /= (fromIntegral bytesTotal)
                                then error "Data.Array.Repa.IO.Binary.get: not a whole number of elements in stream"
                                else V.generateM (fromIntegral lenTotal') (\_ -> (get :: Get a))        

        let arr         = fromVector (Z:. lenTotal') vec
        return $ (arr `asTypeOf` fake)

 {-# INLINE put #-}
 put arr
  =     V.mapM_ put $ toVector arr

