
module Data.Array.Repa.Series.Stream
        ( Stream (..)
        , streamUnboxed
        , streamUnboxed2

        , streamNext)
where
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Vector, Unbox)
import GHC.Exts


-- | A stream of values.
--
--   The rate parameter @k@ represents the abstract length of the stream.
data Stream k a
        = Stream 
        { streamLength  :: Int#
        , streamVector  :: Int# -> a }


-- | Take the next element from a stream.
--   
--   TODO: we should probably only access the stream linearly, 
--   how do we support filtering operations without the plugin?
streamNext :: Unbox a => Stream k a -> Int# -> a
streamNext s ix
        = streamVector s ix
{-# INLINE [1] streamNext #-}


-- | Convert a vector to a stream.
--
--   The rate variable @k@ represents the length of the stream.
streamUnboxed 
        :: Unbox a 
        => Vector a 
        -> (forall k. Stream k a -> b)                  -- ^ worker function
        -> b

streamUnboxed vec f
 = let  !(I# len)  = U.length vec

        get ix     = U.unsafeIndex vec (I# ix)
        {-# INLINE get #-}

        s          = Stream
                   { streamLength = len
                   , streamVector = get }
   in   f s
{-# INLINE [1] streamUnboxed #-}


-- | Convert two vectors of the same length to streams, and run the provided
--   stream operator. If the streams don't have the same length then Nothing.
streamUnboxed2 
        :: (Unbox a, Unbox b)
        => Vector a
        -> Vector b
        -> (forall k. Stream k a -> Stream k b -> c)    -- ^ worker function
        -> Maybe c

streamUnboxed2 vec1 vec2 f
 | U.length vec1 == U.length vec2
 = let  !(I# len)  = U.length vec1

        get vec ix = U.unsafeIndex vec (I# ix)
        {-# INLINE get #-}

        s1      = Stream len (get vec1)
        s2      = Stream len (get vec2)

   in   Just (f s1 s2)

 | otherwise
 = Nothing
{-# INLINE [1] streamUnboxed2 #-}



