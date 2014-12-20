{-# LANGUAGE CPP #-}
module Data.Repa.Stream.Segment
        ( findSegmentsS
        , startLengthsOfSegsS
        , unsafeRatchetS
        , extractS)
where
import Data.IORef
import Data.Vector.Fusion.Stream.Monadic                (Stream(..), Step(..))
import qualified Data.Vector.Generic                    as G
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import qualified Data.Vector.Fusion.Stream.Size         as S

#include "vector.h"

---------------------------------------------------------------------------------------------------
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
                 | pStart x  -> if pEnd x 
                                        then return $ Yield (i, i) (si', f, Nothing)
                                        else return $ Skip (si', f, Just i)
                 | otherwise -> return $ Skip (si', f, n)

                Skip  si'    -> return $ Skip (si', f, n)
                Done         -> return $ Done

        -- We're in a segment,    so look for ending element.
        ostep (si, f, j@(Just iStart))
         = do m <- istep si
              case m of
                Yield (i, x) si'
                 | pEnd  x   -> return $ Yield (iStart, i)    (si', f, Nothing)
                 | otherwise -> return $ Skip  (si', f, j)

                Skip si'     -> return $ Skip  (si', f, j)
                Done         -> return $ Yield (iStart, iEnd) (si, False, Nothing)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM findSegmentsS #-}


---------------------------------------------------------------------------------------------------
-- | Given a stream of starting and ending indices for some segments,
--   convert it to a stream of starting indices and segment lengths.
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
               Yield  x si' -> return $ Yield (iStart, iEnd - iStart + 1) (si', f,     Just x)
               Skip   si'   -> return $ Skip  (si', f, j)
               Done         -> return $ Yield (iStart, iEnd - iStart)     (si,  False, Nothing)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM startLengthsOfSegsS #-}


---------------------------------------------------------------------------------------------------
-- | Interleaved `enumFromTo`. 
--
--   Given a vector of starting values, and a vector of stopping values, 
--   produce an stream where we increase each of the starting values to 
--   the stopping values in a round-robin order. 
--
--   @unsafeRatchetS [10,20,30,40] [15,26,33,47]
--  =  [10,20,30,40       -- 4
--     ,11,21,31,41       -- 4
--     ,12,22,32,42       -- 4
--     ,13,23   ,43       -- 3
--     ,14,24   ,44       -- 3
--        ,25   ,45       -- 2
--              ,46]@     -- 1
--
--   The function takes the starting values in a mutable vector and 
--   updates it during computation. Computation proceeds by making passes
--   through the mutable vector and updating the starting values until
--   they match the stopping values. 
--
--   UNSAFE: Both input vectors must have the same length, 
--           but this is not checked.
--
unsafeRatchetS 
        :: UM.IOVector Int              -- ^ Starting values.
        ->  U.Vector   Int              -- ^ Ending values
        -> IORef (UM.IOVector Int)      -- ^ Vector holding segment lengths.
        -> Stream IO   Int

unsafeRatchetS mvStarts vMax rmvLens
 = Stream ostep (0, Nothing, 0, 0) S.Unknown
 where
        !iSegMax = GM.length mvStarts - 1

        ostep (iSeg, mvmLens, oSeg, oLen)
         | iSeg <= iSegMax
         = do   iVal      <- GM.unsafeRead mvStarts iSeg
                let iNext = vMax `G.unsafeIndex` iSeg
                if  iVal >= iNext
                 then   return $ Skip       (iSeg + 1, mvmLens, oSeg, oLen)
                 else do
                        GM.unsafeWrite mvStarts iSeg (iVal + 1)
                        return $ Yield iVal (iSeg + 1, mvmLens, oSeg, oLen + 1)

         -- We're at the end of an output segment, 
         -- so write the output length into the lengths vector.
         | oLen > 0
         = do   -- Get the current output vector.
                vmLens  <- case mvmLens of
                                Nothing     -> readIORef rmvLens
                                Just vmLens -> return $ vmLens

                -- If the output vector is full then we need to grow it.
                let !oSegLen = UM.length vmLens
                if oSeg >= oSegLen
                 then do
                        vmLens' <- UM.unsafeGrow vmLens (UM.length vmLens)
                        writeIORef rmvLens vmLens'
                        UM.unsafeWrite vmLens' oSeg oLen
                        return $ Skip (0, Just vmLens', oSeg + 1, 0)

                 else do
                        UM.write vmLens  oSeg oLen
                        return $ Skip (0, Just vmLens,  oSeg + 1, 0)

         | otherwise
         = do   vmLens  <- case mvmLens of
                                Nothing     -> readIORef rmvLens
                                Just vmLens -> return $ vmLens

                let vmLens' = UM.unsafeSlice 0 oSeg vmLens
                writeIORef rmvLens vmLens'
                return Done
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM unsafeRatchetS #-}


---------------------------------------------------------------------------------------------------
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
                Skip  si'  -> return $ Skip (si', Nothing)
                Done       -> return $ Done

        -- Emit data from a segment.
        ostep (si, Just (iPos, iTop))
         | iPos >= iTop    =  return $ Skip  (si, Nothing)
         | otherwise       =  return $ Yield (get iPos) (si, Just (iPos + 1, iTop))
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM extractS #-}



