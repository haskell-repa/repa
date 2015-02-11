
module Data.Repa.Flow.Generic.List
        ( fromList
        , toList1
        , takeList1

        , pushList
        , pushList1)
where
import Data.Repa.Flow.Generic.Base
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Given an arity and a list of elements, yield sources that each produce
--   all the elements.
fromList :: States i m
         => i -> [a] -> m (Sources i m a)
fromList n xx0
 = do
        refs    <- newRefs n xx0

        let pull_fromList i eat eject
             = do xx  <- readRefs refs i
                  case xx of
                   []     -> eject
                   x : xs -> do writeRefs refs i xs
                                eat x
            {-# INLINE pull_fromList #-}

        return  $ Sources n pull_fromList
{-# INLINE_FLOW fromList #-}


-- | Drain a single source into a list.
toList1   :: States  i m
          => i -> Sources i m a  -> m [a]
toList1 i (Sources n pullX)
 = do   
        refs    <- newRefs n []

        let loop_toList !acc     = pullX i eat_toList eject_toList
             where eat_toList x  = loop_toList (x : acc)
                   eject_toList  = writeRefs refs i (reverse acc)
            {-# INLINE loop_toList #-}

        loop_toList []
        xx      <- readRefs refs i
        return xx
{-# INLINE_FLOW toList1 #-}


-- | Drain the given number of elements from a single source into a list.
takeList1 :: States i m
          => Int -> i -> Sources i m a  -> m [a]
takeList1 len i (Sources n pullX)
 = do   
        refs    <- newRefs n []

        let loop_toList !ix !acc
             | ix >= len         = writeRefs refs i (reverse acc)
             | otherwise         = pullX i eat_toList eject_toList
             where eat_toList x  = loop_toList (ix + 1) (x : acc)
                   eject_toList  = writeRefs refs i (reverse acc)
            {-# INLINE loop_toList #-}

        loop_toList 0 []
        xx  <- readRefs refs i
        return xx
{-# INLINE_FLOW takeList1 #-}


-------------------------------------------------------------------------------
-- | Push elements into the associated streams of a bundle of sinks.
pushList  :: Monad m => [(i, a)] -> Sinks i m a -> m ()
pushList xx (Sinks _nSinks eat _eject)
 = loop_pushList xx
 where  
        loop_pushList []
         = return ()

        loop_pushList ((i, x) : ixs)
         = do   eat i x
                loop_pushList ixs
{-# INLINE_FLOW pushList #-}


-- | Push the elements of a list into the given stream of a 
--   bundle of sinks.
pushList1 :: Monad m => i -> [a] -> Sinks i m a -> m ()
pushList1 i xx (Sinks _nSinks eat _eject)
 = loop_pushList1 xx
 where  
        loop_pushList1 []   
         = return ()

        loop_pushList1 (x : xs)
         = do   eat i x
                loop_pushList1 xs
{-# INLINE_FLOW pushList1 #-}




