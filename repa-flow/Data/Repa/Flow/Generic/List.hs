
module Data.Repa.Flow.Generic.List
        (fromList, toList, takeList)


-- | Produce a flow from a list.
fromList :: [a] -> IO (Source IO a)
fromList xx0
 = do
        ref     <- newIORef xx0

        let pullX eat eject
             = do xx      <- readIORef ref
                  case xx of
                   []     -> eject
                   x : xs -> do writeIORef ref xs
                                eat x
            {-# INLINE pullX #-}

        return $ Source pullX
{-# INLINE [2] fromList #-}


-- | Drain a flow into a list.
toList  :: Source IO a -> IO [a]
toList (Source pullX)
 = do   
        ref     <- newIORef []

        let loop_toList acc
             =  pullX eat_toList eject_toList

             where eat_toList x
                     = loop_toList (x : acc)

                   eject_toList 
                     = writeIORef ref (reverse acc)

        loop_toList []
        readIORef ref

{-# INLINE [2] toList #-}


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


