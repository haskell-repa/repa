
module Data.Repa.Flow.Simple.List
        ( fromList
        , toList
        , takeList)
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.States                    (States)
import qualified Data.Repa.Flow.Generic         as G
#include "repa-stream.h"


-- | Given an arity and a list of elements, yield a source that produces
--   all the elements.
fromList :: States () m
         => [a] -> m (Source m a)
fromList xx = G.fromList () xx
{-# INLINE fromList #-}


-- | Drain a source into a list.
toList   :: States () m
         => Source m a -> m [a]
toList s =  G.toList1 () s
{-# INLINE toList #-}


-- | Drain the given number of elements from a single source into a list.
takeList :: States () m
         => Int -> Source m a -> m [a]
takeList len s = G.takeList1 len () s 
{-# INLINE takeList #-}
