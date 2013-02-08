

-- Eater
data E a        = E (a -> IO ())




-- 
emap  :: (a -> b) -> E b -> E a
emap f (E eb)
        = E (\xa -> eb (f xa))

-- 
emap2 :: (a -> b -> c) -> E c -> E (a, b)
emap2 f (E ec)
        = E (\(xa, xb) -> ec (f xa xb))

--
efilter :: (a -> Bool) -> E a  -> E a
efilter p (E ea)
        = E (\xa -> if p xa then ea xa
                            else return ())

--
edup  :: E a -> E a -> E a
edup (E ea1) (E ea2)
        = E (\x -> ea1 x >> ea2 x)

-- 
eunzip  :: E a -> E b -> E (a, b)
eunzip (E ea) (E eb)
        = E (\(a, b)
                -> do   ea a
                        eb b)


esums  :: Num a => E a -> (E Int, E a)
esums (E ea)
 = do   refAcc  <- newIORef 0
        refLen  <- newIORef 0

        eatSegLen
         = ...

        eatElem
         = ...


eatPrint :: Show a => E a
eatPrint = E (\xa -> print xa)


feed1 :: [a] -> E a -> IO ()
feed1 [] _               = return ()
feed1 (x : xs) e@(E ea)  = ea x >> feed1 xs e


-- 
feed2 :: [a] -> [b] -> E a -> E b -> IO ()
feed2 xs []


