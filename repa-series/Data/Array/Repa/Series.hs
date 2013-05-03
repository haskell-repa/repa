{-# LANGUAGE MagicHash #-}
module Data.Array.Repa.Series
        ( Stream  (..)
        , streamUnboxed
        , streamNext
        , fold)
where
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Vector, Unbox)
import GHC.Exts

data Stream k a
        = Stream 
        { streamLength  :: Int#
        , streamVector  :: !(Vector a) }


streamUnboxed 
        :: Unbox a 
        => Vector a 
        -> (forall k. Stream k a -> b) 
        -> b

streamUnboxed vec f
 = let  !(I# len)  = U.length vec
        s          = Stream
                   { streamLength = len
                   , streamVector = vec }
   in   f s
{-# NOINLINE streamUnboxed #-}


streamNext :: Unbox a => Stream k a -> Int# -> a
streamNext s ix
        = U.unsafeIndex (streamVector s) (I# ix)
{-# INLINE streamNext #-}


fold :: forall k a b. Unbox b => (a -> b -> a) -> a -> Stream k b -> a
fold f z !source
 = go 0# z
 where  go !ix !acc
         | ix >=# streamLength source
         = acc

         | otherwise
         = let  x = streamNext source ix
           in   go (ix +# 1#) (f acc x)
{-# NOINLINE fold #-}


