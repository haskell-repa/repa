{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}
module Data.Repa.Flow.Default.Debug
        (-- * More
          more,         more'

        -- * More (tabular)
        , moret,        moret'

        -- * More (raw)
        , morer,        morer'

        -- * Nicer
        , Nicer         (..)
        , Presentable   (..))
where
import Data.Repa.Nice.Present
import Data.Repa.Nice.Tabulate
import Data.Repa.Nice
import Data.Repa.Flow.Default           hiding (next)
import qualified Data.Repa.Array        as A
import Control.Monad
import Data.List                        as L
import Data.Text                        as T
import Prelude                          as P
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Given a source index and a length, pull enough chunks from the source
--   to build a list of the requested length, and discard the remaining 
--   elements in the final chunk.
--  
--   * This function is intended for interactive debugging.
--     If you want to retain the rest of the final chunk then use `head_i`.
--
more    :: (Windowable l a, A.Index l ~ Int, Nicer a)
        => Int          -- ^ Index  of source in bundle.
        -> Sources l a  -- ^ Bundle of sources.
        -> IO (Maybe [Nice a])
more i ss = more' i 20 ss
{-# INLINE more #-}


-- | Like `more` but also specify now many elements you want.
more'   :: (Windowable l a, A.Index l ~ Int, Nicer a)
        => Int -> Int -> Sources l a -> IO (Maybe [Nice a])
more' ix len s
        = liftM (liftM (L.map nice . fst)) $ head_i ix len s
{-# INLINE_FLOW more' #-}


-------------------------------------------------------------------------------
-- | Like `more`, but print results in a tabular form to the console.
moret   :: ( A.Windowable l a, A.Index l ~ Int
           , Nicer [a], Presentable (Nice [a]))
        => Int          -- ^ Index of source in bundle.
        -> Sources l a  -- ^ Bundle of sources.
        -> IO ()

moret i ss = moret' i 20 ss
{-# INLINE moret #-}


-- | Like `more'`, but print results in tabular form to the console.
moret'  :: ( A.Windowable l a, A.Index l ~ Int
           , Nicer [a], Presentable (Nice [a]))
        => Int -> Int -> Sources l a -> IO ()

moret' ix len s
 = do   Just (vals, _) <- head_i ix len s
        putStrLn $ T.unpack $ tabulate $ nice vals
{-# INLINE_FLOW moret' #-}


-------------------------------------------------------------------------------
-- | Like `more`, but show elements in their raw format.
morer   :: (A.Windowable l a, A.Index l ~ Int)
        => Int          -- ^ Index  of source in bundle.
        -> Sources l a  -- ^ Bundle of sources.
        -> IO (Maybe [a])

morer i ss = morer' i 20 ss
{-# INLINE morer #-}


-- | Like `more'`, but show elements in their raw format.
morer'   :: (A.Windowable l a, A.Index l ~ Int)
        => Int -> Int -> Sources l a -> IO (Maybe [a])
morer' ix len s
        = liftM (liftM fst) $ head_i ix len s
{-# INLINE_FLOW morer' #-}



