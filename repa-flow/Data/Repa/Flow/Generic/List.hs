
module Data.Repa.Flow.Generic.List
        (fromList, toList1) -- , toList, takeList)
where
import Data.Repa.Flow.Generic.Base


-- | Given an arity and a list of elements, yield sources that each produce
--   all the elements.
fromList :: (States i m (Box [a]), Monad m) 
         => i -> [a] -> m (Sources i m a)
fromList n xx0
 = do
        refs    <- newRefs n (Box xx0)

        let pull_fromList i eat eject
             = do Box xx  <- readRefs refs i
                  case xx of
                   []     -> eject
                   x : xs -> do writeRefs refs i (Box xs)
                                eat x
            {-# INLINE pull_fromList #-}

        return  $ Sources n pull_fromList
{-# INLINE [2] fromList #-}


-- | Drain a single source into a list.
toList1  :: (States i m (Box [a]), Monad m) 
         => Sources i m a -> Ix i -> m [a]
toList1 (Sources n pullX) i
 = do   
        refs    <- newRefs n (Box [])

        let loop_toList !acc
             = pullX i eat_toList eject_toList

             where eat_toList x
                     = loop_toList (x : acc)

                   eject_toList 
                     = writeRefs refs i (Box $ reverse acc)

        loop_toList []
        Box xx  <- readRefs refs i
        return xx
{-# INLINE [2] toList1 #-}

{-
-- | Drain the given number of elements into a list.
takeList :: Int -> Source IO a -> IO [a]
takeList n (Source pullX)
 = do   
        ref     <- newIORef []

        let loop_toList ix
             | ix >= n   = return ()
             | otherwise = do pullX eat_toList eject_toList
                              loop_toList (ix + 1)
             where 
                   eat_toList x
                     = modifyIORef ref (\acc -> x : acc)

                   eject_toList 
                     = return ()

        loop_toList 0
        list    <- readIORef ref
        return  $ reverse list


-}
