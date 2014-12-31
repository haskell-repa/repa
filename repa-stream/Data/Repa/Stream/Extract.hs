{-# LANGUAGE CPP #-}
module Data.Repa.Stream.Extract
        (extractS)
where
import Data.IORef
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Generic             as G
import qualified Data.Vector.Generic.Mutable     as GM
import qualified Data.Vector.Unboxed             as U
import qualified Data.Vector.Unboxed.Mutable     as UM
import qualified Data.Vector.Fusion.Stream.Size  as S

#include "vector.h"

-- | Extract.
--   TODO: extract intersperse from some other vector.
extractS
        :: Monad m
        => (Int -> a)
        -> Stream m (Int, Int)  -- Stream of segment starts and lengths.
        -> Stream m a           -- Stream of vector data.

extractS get (Stream istep si0 _)
 = Stream ostep (si0, Nothing) S.Unknown
 where
        -- Start a new segment.
        ostep (si, Nothing)
         =  istep si >>= \m
         -> case m of
                Yield (iStart, iLen) si' 
                          -> return $ Skip (si', Just (iStart, iStart + iLen))
                Skip  si' -> return $ Skip (si', Nothing)
                Done      -> return $ Done

        -- Emit data from a segment.
        ostep (si, Just (iPos, iTop))
         | iPos >= iTop   =  return $ Skip  (si, Nothing)
         | otherwise      =  return $ Yield (get iPos) 
                                            (si, Just (iPos + 1, iTop))
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM extractS #-}
