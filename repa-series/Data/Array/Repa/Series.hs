
module Data.Array.Repa.Series
        ( Stream 
        , streamUnboxed
        , fold)
where
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Vector, Unbox)


data Stream a
        = Stream 
        { streamLength  :: Int
        , streamNext    :: Int -> a }

data Sink a
        = Sink
        { sinkPush      :: a -> IO () }


streamUnboxed :: Unbox a => Vector a -> Stream a
streamUnboxed vec
 = Stream
 { streamLength = U.length vec
 , streamNext   = \ix -> U.unsafeIndex vec ix }
{-# NOINLINE streamUnboxed #-}


fold :: (a -> b -> a) -> a -> Stream b -> a
fold f z source
 = go 0 z
 where  go !ix !acc
         | ix >= streamLength source
         = acc

         | otherwise
         = let  x = streamNext source ix
           in   go (ix + 1) (f acc x)
{-# NOINLINE fold #-}


