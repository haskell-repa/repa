
module Data.Repa.Stream.Merge
        (mergeS)
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import Data.Repa.Scalar.Option
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Merge two key-value streams.
--
--   The streams are assumed to be pre-sorted on the keys.
--
mergeS  :: (Monad m, Ord k)
        => (k -> a -> b -> c) -- ^ Combine two values with the same key.
        -> (k -> a -> c)      -- ^ Handle a left  value without a right value.
        -> (k -> b -> c)      -- ^ Handle a right value without a left value.
        -> Stream m (k, a)    -- ^ Stream of keys and left values.
        -> Stream m (k, b)    -- ^ Stream of keys and right values.
        -> Stream m (k, c)    -- ^ Stream of keys and results.

mergeS fBoth fLeft fRight (Stream istepA sA0 _) (Stream istepB sB0 _)
 = Stream ostep (sA0, sB0, None2, True, None2, True) S.Unknown
 where
        -- Merge where both streams match.
        ostep (sA, sB, kxA@(Some2 kA xA), hasA
                     , kxB@(Some2 kB xB), hasB)

         = return $ Yield (if | kA == kB  -> (kA, fBoth  kA xA xB)
                              | kB <  kA  -> (kB, fRight kB xB)
                              | otherwise -> (kA, fLeft  kA xA))

                          (if | kA == kB  -> (sA, sB, None2, hasA, None2, hasB)
                              | kB <  kA  -> (sA, sB, kxA,   hasA, None2, hasB)
                              | otherwise -> (sA, sB, None2, hasA, kxB,   hasB))

        -- Drain left stream.
        ostep (sA, sB, Some2 kA xA, hasA, kxB@None2, hasB@False)
         = return $ Yield (kA, fLeft  kA xA)
                          (sA, sB, None2, hasA, kxB, hasB)

        -- Drain right stream.
        ostep (sA, sB, kxA@None2, hasA@False, Some2 kB xB, hasB)
         = return $ Yield (kB, fRight kB xB)
                          (sA, sB, kxA, hasA, None2, hasB)

        -- Advance left stream.
        ostep (sA, sB, kxA@None2, hasA@True, kxB, hasB)
         =  istepA sA >>= \mA
         -> case mA of
                Yield (kA, xA) sA'
                 -> return $ Skip (sA', sB, Some2 kA xA, True, kxB, hasB)

                Skip  sA'
                 -> return $ Skip (sA', sB, kxA, hasA,  kxB, hasB)

                Done  
                 -> return $ Skip (sA,  sB, kxA, False, kxB, hasB)

        -- Advance the right stream.
        ostep (sA, sB, kxA, hasA, kxB@None2, hasB@True)
         =  istepB sB >>= \mB
         -> case mB of
                Yield (kB, xB) sB'
                 -> return $ Skip (sA, sB', kxA, hasA, Some2 kB xB, True)

                Skip  sB'
                 -> return $ Skip (sA, sB', kxA, hasA, kxB, hasB)

                Done 
                 -> return $ Skip (sA, sB,  kxA, hasA, kxB, False)

        -- Done
        ostep (_sA, _sB, None2, False, None2, False)
         = return $ Done
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM mergeS #-}

