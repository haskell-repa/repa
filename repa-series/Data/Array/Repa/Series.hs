
module Data.Array.Repa.Series
        ( Stream )
where
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Vector)


data Stream a
        = Stream 
        { streamLength  :: Int
        , streamNext    :: Int -> IO a }

data Sink a
        = Sink
        { sinkPush      :: a -> IO () }


sourceVector :: Unbox a => Vector a -> Source a
sourceVector vec
 = Source
 { sourceLength = U.length vec
 , sourcePull   = \ix -> U.unsafeIndex vec ix }
{-# NOINLINE sourceVector #-}


fold :: (a -> b -> a) -> b -> Source a -> IO b
fold f z source
 = go 0 z
 where  go !ix !acc
         | ix >= sourceLength source 
         = return acc

         | otherwise
         = do   x       <- sourceNext source ix
                go (ix + 1) (f x acc)
{-# NOINLINE fold #-}


