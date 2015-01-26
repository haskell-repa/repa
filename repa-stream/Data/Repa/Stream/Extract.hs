{-# LANGUAGE CPP #-}
module Data.Repa.Stream.Extract
        (extractS)
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"

-- | Extract segments from some source array and concatenate them.
-- 
-- @
--    let arr = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
--    in  extractS (index arr) [(0, 1), (3, 3), (2, 6)]
--    
--     => [10, 13, 14, 15, 12, 13, 14, 15, 16, 17]
-- @
extractS
        :: Monad m
        => (Int -> a)           -- ^ Function to get elements from the source.
        -> Stream m (Int, Int)  -- ^ Segment starts and lengths.
        -> Stream m a           -- ^ Result elements.

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
