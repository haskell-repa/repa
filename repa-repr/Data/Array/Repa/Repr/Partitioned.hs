data P r1 r2

data instance Array sh (P r1 r2) e
        = APart sh                              -- size of whole arry
                (Range sh) (Array sh r1 e)      -- if in range use this array
                (Array sh r2 e)                 -- otherwise use this array

data Range sh
        = Range sh sh 
                (sh -> Bool)

inRange (Range _ _ pred) ix
        = pred ix

instance (Rep r1 e, Rep r2 e) => Rep (P r1 r2) e where
 index (APart sh range arr1 arr2) ix
   | inRange range ix   = index arr1 ix
   | otherwise          = index arr2 ix

 extent (APart sh _ _ _) 
        = sh
