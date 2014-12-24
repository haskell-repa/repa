
module Data.Repa.Flow.Generic.List
        ( fromList
        , toList1
        , takeList1)
where
import Data.Repa.Flow.Generic.Base


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
{-# INLINE [2] fromList #-}


-- | Drain a single source into a list.
toList1   :: States  i m
          => Sources i m a -> Ix i -> m [a]
toList1 (Sources n pullX) i
 = do   
        refs    <- newRefs n []

        let loop_toList !acc     = pullX i eat_toList eject_toList
             where eat_toList x  = loop_toList (x : acc)
                   eject_toList  = writeRefs refs i (reverse acc)

        loop_toList []
        xx      <- readRefs refs i
        return xx
{-# INLINE [2] toList1 #-}


-- | Drain the given number of elements from a single source into a list.
takeList1 :: States i m
          => Int -> Sources i m a -> Ix i -> m [a]
takeList1 len (Sources n pullX) i
 = do   
        refs    <- newRefs n []

        let loop_toList !ix !acc
             | ix >= len         = writeRefs refs i (reverse acc)
             | otherwise         = pullX i eat_toList eject_toList
             where eat_toList x  = loop_toList (ix + 1) (x : acc)
                   eject_toList  = writeRefs refs i (reverse acc)

        loop_toList 0 []
        xx  <- readRefs refs i
        return xx
{-# INLINE [2] takeList1 #-}

