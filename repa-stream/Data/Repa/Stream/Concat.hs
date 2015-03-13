
module Data.Repa.Stream.Concat
        (catMaybesS)
where
import Data.Vector.Fusion.Stream.Monadic        (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size as S
#include "repa-stream.h"


-- | Return the `Just` elements from a stream, dropping the `Nothing`s.
catMaybesS
        :: Monad m
        => Stream m (Maybe a)
        -> Stream m a

catMaybesS (Stream istep si0 sz)
 = Stream ostep si0 (S.toMax sz)
 where
        ostep si
         =  istep si >>= \m
         -> case m of
                Yield Nothing  si'      -> return $ Skip  si'
                Yield (Just x) si'      -> return $ Yield x si'
                Skip si'                -> return $ Skip si'
                Done                    -> return $ Done
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM catMaybesS #-}

