
module Data.Repa.Stream.Dice
        ( diceWithS )
where
import Data.Vector.Fusion.Stream.Monadic        (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size as S
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Given predicates that detect the begining and end of interesting segments
--   of informaiton, scan through a vector looking for when these begin
--   and end.
--
diceWithS   
        :: Monad m
        => (a -> Bool)  -- ^ Detect the start of an inner and outer segment.
        -> (a -> Bool)  -- ^ Detect the end   of an inner segment.
        -> (a -> Bool)  -- ^ Detect the end   of an inner and outer segment.
        -> Stream m a
        -> Stream m (Maybe (Int, Int), Maybe (Int, Int)) 
                        -- ^ Segment starts   and lengths

diceWithS pStart pEndInner pEndBoth (Stream istep s0 sz)
 = Stream ostep (s0, True, 0, 0, 0, Nothing) (S.toMax sz)

 where
        -- TODO: check end of input condition with no inner segments.
        ostep (_, False, _, _, _, _)
         = return Done

        -- We're not in an inner segment, so look for the next starting element.
        ostep (si, f, iSrc, iOuterStart, iOuterLen, mInner@Nothing)
         = do   m <- istep si
                
                case m of
                 Yield x si'
                  -- Outer segment started and ended on the same element.
                  | pStart x, pEndBoth x
                  -> return $ Yield      
                        (Nothing, Just (iOuterStart, 0))
                        (si', f, iSrc + 1, iOuterStart, 0, mInner)

                  -- Inner segment started and ended on the same element.
                  | pStart x, pEndInner x
                  -> return $ Yield
                        (Just  (iSrc, 0), Nothing)
                        (si', f, iSrc + 1, iOuterStart, iOuterLen + 1, mInner)

                  -- Segment has started on this element.
                  | pStart x
                  -> return $ Skip
                        (si', f, iSrc + 1, iOuterStart, iOuterLen + 1, Just (iSrc, 1))

                  -- Still looking for the starting element.
                  | otherwise
                  -> return $ Skip
                        (si', f, iSrc + 1, iOuterStart, iOuterLen,     mInner)

                 -- We didn't get an element this time.
                 Skip si' 
                  -> return $ Skip
                        (si', f, iSrc,     iOuterStart, iOuterLen,     mInner)

                 -- Found end of input outside a segment.
                 Done     
                  -> return $ Skip
                        (si,  False, iSrc, iOuterStart, iOuterLen,     mInner)


        -- We're in an inner segment, looking for the ending element.
        ostep   ( si, f, iSrc, iOuterStart, iOuterLen
                , mInner@(Just (iInnerStart, iInnerLen)))

         = do   m <- istep si

                case m of
                 Yield x si'
                  -- Both inner and outer ended at this point,
                  --  and now we're looking for a new segment start.
                  | pEndBoth x
                  -> return $ Yield 
                        ( Just (iInnerStart, iInnerLen)
                        , Just (iOuterStart, iOuterLen))
                        ( si', f, iSrc + 1, iOuterStart + 1, 0,         Nothing)

                  -- Inner segment ended at this point,
                  -- but we're still in the outer segment.
                  | pEndInner x
                  -> return  $ Yield 
                        ( Just  (iInnerStart, iInnerLen)
                        , Nothing)
                        ( si', f, iSrc + 1, iOuterStart, iOuterLen + 1, Nothing)

                  -- Another element of the inner segment.
                  | otherwise
                  -> return  $ Skip
                        ( si', f, iSrc + 1, iOuterStart, iOuterLen
                        , Just (iInnerStart, iInnerLen + 1))

                 -- We didn't get an element this time.
                 Skip si'
                  -> return $ Skip
                        ( si', f,    iSrc, iOuterStart, iOuterLen,      mInner)

                 -- Found end of input during a segment.
                 Done 
                  -> return $ Yield
                        ( Just (iInnerStart, iInnerLen)
                        , Just (iOuterStart, iOuterLen))
                        ( si,  True, iSrc, iOuterStart, iOuterLen,      Nothing)


        
