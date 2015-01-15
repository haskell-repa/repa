{-# LANGUAGE OverlappingInstances #-}
module Data.Repa.Flow.Debug
        ( Nicer (..)
        , Tabulate (..)
        , more
        , moren
        , moret)
where
import Data.Repa.Nice.Tabulate
import Data.Repa.Nice
import Data.Repa.Flow                   hiding (next)
import qualified Data.Repa.Array        as A
import Control.Monad
import Data.Monoid
import Data.Char
import Data.List                        as L
import Prelude                          as P
import Data.Maybe

-- | Given a source index and a length, pull enough chunks from the source
--   to build a list of the requested length, and discard the remaining 
--   elements in the final chunk.
--  
--   * This function is intended for interactive debugging.
--     If you want to retain the rest of the final chunk then use `head_i`.
--
more    :: A.Window r DIM1 a
        => Int          -- ^ Source index.
        -> Int          -- ^ Number of elements to show.
        -> Sources r a
        -> IO (Maybe [a])
more ix len s
        = liftM (liftM fst) $ head_i ix len s
{-# INLINE more #-}


-- | Like `more`, but convert the result to a nice representation.
moren   :: (A.Window r DIM1 a, Nicer a)
        => Int          -- ^ Source index.
        -> Int          -- ^ Number of elements to show.
        -> Sources r a
        -> IO (Maybe [Nice a])
moren ix len s
        = liftM (liftM (map nice . fst)) $ head_i ix len s
{-# INLINE moren #-}


-- | Like `more`, but print results in tabular form to the console.
moret   :: (A.Window r DIM1 a, Nicer a, Tabulate a)
        => Int          -- ^ Source index.
        -> Int          -- ^ Number of elements to show.
        -> Sources r a
        -> IO ()

moret ix len s
 = do   Just (vals, _)       <- head_i ix len s
        putStrLn $ tabulate vals
{-# NOINLINE moret #-}



