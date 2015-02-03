
module Data.Repa.Stream.Dice
        ( diceSepS )
where
import Data.Vector.Fusion.Stream.Monadic        (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size as S
import Debug.Trace
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Given predicates that detect the begining and end of interesting segments
--   of information, scan through a vector looking for when these begin
--   and end.
--
diceSepS
        :: Monad m
        => (a -> Bool)  -- ^ Detect the end of a column.
        -> (a -> Bool)  -- ^ Detect the end of a row.
        -> Stream m a
        -> Stream m (Maybe (Int, Int), Maybe (Int, Int)) 
                        -- ^ Segment starts   and lengths

diceSepS pEndCol pEndRow (Stream istep s0 sz)
 = Stream ostep (s0, True, 0, 0, 0, False, Nothing) 
                (case sz of
                        S.Exact n       -> S.Max (n + 1)
                        S.Max   n       -> S.Max (n + 1)
                        S.Unknown       -> S.Unknown)
 where
        ostep (_, False, _, _, _, _, _)
         = return Done

        -- We're not in an inner segment, so look for the next starting element.
        ostep (si, f, iSrc, iRowStart, iSeps, inRow, mCol@Nothing)
         =  iSrc `seq` iRowStart `seq` iSeps `seq` inRow `seq`
            istep si >>= \m 
         -> case m of
             Yield x si'
              -- Line ended outside a word.
              | pEndRow x
              -> let nRow   = if inRow then iSeps + 1 else 0
                 in return $ Yield      
                     ( Just (iSrc, 0)
                     , Just (iRowStart, nRow))
                     (si', f, iSrc + 1, iRowStart + nRow, 0, False, mCol)

              -- Inner segment started and ended on the same element.
              | pEndCol x
              -> return $ Yield
                    (Just (iSrc, 0), Nothing)
                    (si', f, iSrc + 1, iRowStart, iSeps + 1, True,  mCol)

              -- Segment has started on this element.
              | otherwise
              -> return $ Skip
                    (si', f, iSrc + 1, iRowStart, iSeps,     True,  Just (iSrc, 1))

             -- We didn't get an element this time.
             Skip si' 
              -> return $ Skip
                    (si', f, iSrc,     iRowStart, iSeps,     inRow, mCol)

             -- Found end of input outside a segment.
             Done
              | inRow
              -> return $ Yield
                    (Just (0, 0), Just (iRowStart, iSeps + 1))
                    (si,  False, iSrc, iRowStart, iSeps, False, mCol)

              | otherwise
              -> return $ Yield
                    (Just (0, 0), Nothing)
                    (si,  False, iSrc, iRowStart, iSeps, False, mCol)


        -- We're in an inner segment, looking for the ending element.
        ostep   ( si, f, iSrc, iRowStart, iSeps, inRow
                , mCol@(Just (iColStart, iColLen)))

         = iSrc `seq` iRowStart `seq` iSeps `seq` inRow `seq`
           istep si >>= \m
         -> case m of
             Yield x si'
              -- Both inner and outer ended at this point,
              --  and now we're looking for a new segment start.
              | pEndRow x
              -> return $ Yield 
                    ( Just (iColStart, iColLen)
                    , Just (iRowStart, iSeps + 1))
                    ( si', f,   iSrc + 1, iRowStart + iSeps + 1, 0, False, Nothing)

              -- Inner segment ended at this point,
              -- but we're still in the outer segment.
              | pEndCol x
              -> return  $ Yield 
                    ( Just  (iColStart, iColLen)
                    , Nothing)
                    ( si', f,   iSrc + 1, iRowStart, iSeps + 1, inRow, Nothing)

              -- Another element of the inner segment.
              | otherwise
              -> return  $ Skip
                    ( si', f,   iSrc + 1, iRowStart, iSeps,     inRow
                    , Just (iColStart, iColLen + 1))

             -- We didn't get an element this time.
             Skip si'
              -> return $ Skip
                   ( si', f,    iSrc,     iRowStart, iSeps,     inRow, mCol)

             -- Found end of input during a segment.
             Done 
              -> return $ Yield
                   ( Just (iColStart, iColLen)
                   , Nothing)
                   ( si,  True, iSrc,     iRowStart, iSeps,     inRow, Nothing)
{-# INLINE_STREAM diceSepS #-}
