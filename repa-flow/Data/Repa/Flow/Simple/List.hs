
module Data.Repa.Flow.Simple.List
        ( fromList
        , toList
        , takeList)
where
import Control.Monad
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.States                    (States)
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Flow.States          as S

-- | Given an arity and a list of elements, yield a source that produces
--   all the elements.
fromList :: States () m
         => [a] -> m (Source m a)
fromList xx = liftM wrap $ G.fromList () xx
{-# INLINE fromList #-}


-- | Drain a source into a list.
toList   :: States () m
         => Source m a -> m [a]
toList s =  G.toList1 (unwrap s) S.UIx
{-# INLINE toList #-}


-- | Drain the given number of elements from a single source into a list.
takeList :: States () m
         => Int -> Source m a -> m [a]
takeList len s = G.takeList1 len (unwrap s) S.UIx
{-# INLINE takeList #-}
