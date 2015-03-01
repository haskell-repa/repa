
module Data.Repa.Flow.Generic.Debug
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
import Data.Repa.Flow.Generic           hiding (next)
import Control.Monad
import Data.List                        as L
import Data.Text                        as T
import Prelude                          as P
#include "repa-flow.h"


-------------------------------------------------------------------------------
-- | Given a source index and a length, pull enough chunks from the source
--   to build a list of the requested length, and discard the remaining 
--   elements in the final chunk.
--  
--   * This function is intended for interactive debugging.
--     If you want to retain the rest of the final chunk then use `head_i`.
--
more    :: (States i IO, Nicer a)
        => i                    -- ^ Index  of source in bundle.
        -> Sources i IO a     -- ^ Bundle of sources.
        -> IO [Nice a]
more i ss = more' i 20 ss
{-# INLINE more #-}


-- | Like `more` but also specify now many elements you want.
more'   :: (States i IO, Nicer a)
        => i -> Int -> Sources i IO a -> IO [Nice a]
more' ix len s
        = liftM (L.map nice . fst) $ head_i len s ix
{-# INLINE_FLOW more' #-}


-------------------------------------------------------------------------------
-- | Like `more`, but print results in a tabular form to the console.
moret   :: (States i IO, Nicer [a], Presentable (Nice [a]))
        => i                    -- ^ Index of source in bundle.
        -> Sources i IO a     -- ^ Bundle of sources.
        -> IO ()

moret i ss = moret' i 20 ss
{-# INLINE moret #-}


-- | Like `more'`, but print results in tabular form to the console.
moret'  :: (States i IO, Nicer [a], Presentable (Nice [a]))
        => i -> Int -> Sources i IO a -> IO ()

moret' i len s
 = do   (vals, _) <- head_i len s i
        putStrLn $ T.unpack $ tabulate $ nice vals
{-# INLINE_FLOW moret' #-}


-------------------------------------------------------------------------------
-- | Like `more`, but show elements in their raw format.
morer   :: States i IO
        => i                    -- ^ Index  of source in bundle.
        -> Sources i IO a       -- ^ Bundle of sources.
        -> IO [a]

morer i ss = morer' i 20 ss
{-# INLINE morer #-}


-- | Like `more'`, but show elements in their raw format.
morer'  :: States i IO
        => i -> Int -> Sources i IO a -> IO [a]
morer' i len s
        = liftM fst $ head_i len s i
{-# INLINE_FLOW morer' #-}



