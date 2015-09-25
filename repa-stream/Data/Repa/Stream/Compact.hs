
module Data.Repa.Stream.Compact
        ( compactS
        , compactInS )
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Combination of `fold` and `filter`. 
--   
--   We walk over the stream front to back, maintaining an accumulator.
--   At each point we can chose to emit an element (or not)
--
compactS
        :: Monad m
        => (s -> a -> (s, Maybe b))     -- ^ Worker function.
        -> s                            -- ^ Starting state
        -> Stream m a                   -- ^ Input elements.
        -> Stream m b

compactS f s0 (Stream istep si0 sz)
 = Stream ostep (si0, s0) (S.toMax sz)
 where
        ostep (si, s)
         =  istep si >>= \m
         -> case m of
                Yield x si'         
                 -> case f s x of
                        (s', Nothing)   -> return $ Skip    (si', s')
                        (s', Just y)    -> return $ Yield y (si', s')
                Skip si'                -> return $ Skip    (si', s)
                Done                    -> return $ Done
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM compactS #-}


-- | Like `compact` but use the first value of the stream as the 
--   initial state, and add the final state to the end of the output.
compactInS
        :: Monad m
        => (a -> a -> (a, Maybe a))     -- ^ Worker function.
        -> Stream m a                   -- ^ Input elements.
        -> Stream m a

compactInS f (Stream istep si0 sz)
 = Stream ostep (si0, Nothing) (S.toMax sz)
 where
        ostep (si, ms@Nothing)
         =  istep si >>= \m
         -> case m of
                Yield x si'             -> return $ Skip (si', Just x)
                Skip    si'             -> return $ Skip (si', ms)
                Done                    -> return $ Done

        ostep (si, ms@(Just s))
         =  istep si >>= \m
         -> case m of
                Yield x si'     
                 -> case f s x of
                        (s', Nothing)   -> return $ Skip    (si', Just s')
                        (s', Just y)    -> return $ Yield y (si', Just s')
                Skip si'                -> return $ Skip    (si', ms)
                Done                    -> return $ Yield s (si,  Nothing)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM compactInS #-}
