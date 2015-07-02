
module Data.Repa.Stream.Pad
        (padForwardS)
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import Data.Repa.Scalar.Option
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Given a stream of keys and values, and a successor function for keys, 
--   if the stream is has keys missing in the sequence then insert 
--   the missing key, copying forward the the previous value.
--
padForwardS
        :: (Monad m, Ord k)
        => (k -> k)             -- ^ Successor functinon for keys.
        -> Stream m (k, v)      -- ^ Input stream.
        -> Stream m (k, v)

padForwardS ksucc (Stream istep si0 _)
 = Stream ostep (si0, None2, None2) S.Unknown
 where
        -- Load the first element.
        ostep (si, sPrev@None2, sBound)
         =  istep si >>= \m
         -> case m of
                Yield (k0, v0) si'
                 -> return $ Yield (k0, v0) (si', Some2 k0 v0, sBound)

                Skip si' 
                 -> return $ Skip           (si', sPrev,       sBound)

                Done     
                 -> return $ Done

        -- Load the next element element.
        ostep (si, sPrev@(Some2 kPrev _vPrev), sTarget@None2)
         =  istep si >>= \m
         -> case m of
                Yield (kStep, vStep) si'
                 -- The next element from the input is more than the expected
                 -- one then there is a gap in the input.
                 |  kExpect <- ksucc kPrev
                 ,  kStep   >  kExpect
                 -> return $ Skip                 (si', sPrev, Some2 kStep vStep)

                 -- Otherwise there is no gap.
                 |  otherwise
                 -> return $ Yield (kStep, vStep) (si', Some2 kStep vStep, None2)

                Skip si' -> return $ Skip  (si', sPrev, sTarget)
                Done     -> return $ Done

        -- Fill in missing elements.
        ostep (si, _sPrev@(Some2 kPrev vPrev), sTarget@(Some2 kTarget vTarget))
         = let  kNext    = ksucc kPrev
           in   if kNext >= kTarget
                     -- We've reached the target, so the gap is filled.
                     then return $ Yield (kTarget, vTarget) (si, Some2 kTarget vTarget, None2)

                     -- We're still filling the gap.
                     else return $ Yield (kNext,   vPrev)   (si, Some2 kNext   vPrev, sTarget)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM padForwardS #-}

