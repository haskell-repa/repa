
module Data.Repa.Flow.Debug
        ( Nicer (..)
        , next
        , nnext)
where
import Data.Repa.Nice
import Data.Repa.Flow                   hiding (next)
import qualified Data.Repa.Array        as A
import Control.Monad

-- | Given a source index and a length, pull enough chunks from the source
--   to build a list of the requested length, and discard the remaining 
--   elements in the final chunk.
--  
--   * This function is intended for interactive debugging.
--     If you want to retain the rest of the final chunk then use `head_i`.
--
next    :: A.Window r DIM1 a
        => Int          -- ^ Source index.
        -> Int          -- ^ Number of elements to show.
        -> Sources r a
        -> IO (Maybe [a])
next ix len s
        = liftM (liftM fst) $ head_i ix len s
{-# INLINE next #-}


-- | Like `next`, but convert the result to a nice representation.
nnext   :: (A.Window r DIM1 a, Nicer a)
        => Int          -- ^ Source index.
        -> Int          -- ^ Number of elements to show.
        -> Sources r a
        -> IO (Maybe [Nice a])
nnext ix len s
        = liftM (liftM (map nice . fst)) $ head_i ix len s
{-# INLINE nnext #-}


