
module Data.Array.Repa.Series.Base
        ( Series (..)
        , index
        , eatUnboxed
        , eatUnboxed2)
where
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Vector, Unbox)
import GHC.Exts


-- | A stream of values.
--
--   The rate parameter @k@ represents the abstract length of the stream.
data Series k a
        = Series 
        { seriesLength  :: Int#
        , seriesVector  :: !(Vector a) }  


-- | Index into a series.
index :: Unbox a => Series k a -> Int# -> a
index s ix
        = U.unsafeIndex (seriesVector s) (I# ix)
{-# INLINE [1] index #-}


-- | Process a vector as a series.
--
--   The rate variable @k@ represents the length of the series.
eatUnboxed 
        :: Unbox a 
        => Vector a 
        -> (forall k. Series k a -> b)                  -- ^ worker function
        -> b

eatUnboxed vec f
 = let  !(I# len)  = U.length vec

        s          = Series
                   { seriesLength = len
                   , seriesVector = vec }
   in   f s
{-# INLINE [1] eatUnboxed #-}


-- | Convert two vectors of the same length to series, and run the
--   provided consumer. If the seriess don't have the same length
--   then Nothing.
eatUnboxed2 
        :: (Unbox a, Unbox b)
        => Vector a
        -> Vector b
        -> (forall k. Series k a -> Series k b -> c)    -- ^ worker function
        -> Maybe c

eatUnboxed2 vec1 vec2 f
 | U.length vec1 == U.length vec2
 = let  !(I# len) = U.length vec1
        !s1       = Series len vec1
        !s2       = Series len vec2
   in   Just (f s1 s2)

 | otherwise
 = Nothing
{-# INLINE [1] eatUnboxed2 #-}

