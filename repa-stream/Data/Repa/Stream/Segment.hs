{-# LANGUAGE CPP #-}
module Data.Repa.Stream.Segment
        ( findSegmentsS
        , startLengthsOfSegsS)
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size  as S

#include "vector.h"

-------------------------------------------------------------------------------
-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end.
findSegmentsS
        :: Monad m
        => (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end   of segment.
        -> i                    -- ^ Index of final element in stream.
        -> Stream m (i, a)      -- ^ Stream of indices and elements.
        -> Stream m (i, i)      -- ^ Stream of segment start and end indices.

findSegmentsS pStart pEnd iEnd (Stream istep s sz)
 = Stream ostep (s, True, Nothing) (S.toMax sz)
 where
        -- We've hit the end of the stream
        ostep (_, False, _)
         = return Done

        -- We're not in a segment, so look for the next starting element.
        ostep (si, f, n@Nothing)
         = do m <- istep si
              case m of
                Yield (i, x) si'
                 | pStart x  
                 -> if pEnd x 
                        -- Segment started and ended on the same element.
                        then return $ Yield (i, i) (si', f, Nothing)

                        -- Segment has started on this element.
                        else return $ Skip (si', f, Just i)

                 -- Still looking for the starting element.
                 | otherwise -> return $ Skip (si', f, n)

                -- We didn't get an element this time.
                Skip  si'    -> return $ Skip (si', f, n)

                -- Found end of imput.
                Done         -> return $ Done

        -- We're in a segment,    so look for ending element.
        ostep (si, f, j@(Just iStart))
         = do m <- istep si
              case m of
                Yield (i, x) si'
                 -- Segment ended here.
                 | pEnd  x   -> return $ Yield (iStart, i)    
                                               (si', f, Nothing)

                 -- Still looking for the ending element.
                 | otherwise -> return $ Skip  (si', f, j)

                -- We didn't get an element this time.
                Skip si'     -> return $ Skip  (si', f, j)

                -- Found end of input during a segment.
                Done         -> return $ Yield (iStart, iEnd) 
                                               (si, False, Nothing)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM findSegmentsS #-}


-------------------------------------------------------------------------------
-- | Given a stream of starting and ending indices for some segments,
--   convert it to a stream of starting indices and segment lengths.
--
--   * The ending indices must be after the starting indices, 
--     otherwise the result will contain negative lengths.
--
startLengthsOfSegsS
        :: Monad m
        => Stream m (Int, Int)  -- ^ Start and end indices.
        -> Stream m (Int, Int)  -- ^ Start indices and lengths of segments.

startLengthsOfSegsS (Stream istep s sz)
 = Stream ostep (s, True, Nothing) sz
 where
        ostep (_, False, _)
         = return Done

        ostep (si, f, n@Nothing)
         = do m <- istep si
              case m of
               Yield x si'  -> return $ Skip (si', f, Just x)
               Skip  si'    -> return $ Skip (si', f, n)
               Done         -> return $ Done

        ostep (si, f, j@(Just (iStart, iEnd)))
         = do m <- istep si
              case m of
               Yield  x si' -> return $ Yield (iStart, iEnd - iStart + 1) 
                                              (si', f,     Just x)

               Skip   si'   -> return $ Skip  (si', f, j)

               Done         -> return $ Yield (iStart, iEnd - iStart + 1) 
                                              (si,  False, Nothing)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM startLengthsOfSegsS #-}

