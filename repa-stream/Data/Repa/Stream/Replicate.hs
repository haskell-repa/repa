
module Data.Repa.Stream.Replicate
        (replicatesS)
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Segmented replicate.
--   
--   Given a stream of counts and values, produce a result stream where
--   each value is replciated the associated number of times.
--
replicatesS 
        :: Monad m
        => Stream m (Int, a)
        -> Stream m a

replicatesS (Stream istepA sA0 _)
 = Stream ostep (sA0, 0, Nothing) S.Unknown
 where
        -- Pull the next element from the source stream.
        ostep (sA, 0, mv)
         =  istepA sA >>= \rA
         -> case rA of
                Done                 -> return Done
                Skip sA'             -> return $ Skip (sA', 0,   mv)
                Yield (len, val) sA' -> return $ Skip (sA', len, Just val)

        -- Should never be entered, as we'll always have a value
        -- when the count is non-zero.
        ostep (_, _, Nothing)       
         = return Done

        ostep (sA, len, mv@(Just val))
         = return $ Yield val (sA, len - 1, mv)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM replicatesS #-}

