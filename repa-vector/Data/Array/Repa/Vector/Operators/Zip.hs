
module Data.Array.Repa.Vector.Operators.Zip
        ( Zip(..)
        , zip3
        , zip4
        , zip5
        , zip6
        , zipWith
        , zipWith3
        , zipWith4
        , zipWith5
        , zipWith6)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Operators.Map
import GHC.Exts
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U

import Prelude
        hiding (map, zip, zip3, zipWith, zipWith3)


class Zip r1 r2 a b where
 type TZ r1 r2
 -- | Vector zip uses the least general possible representation for the result.
 --   
 --   The two vectors must have the same length, else undefined.
 zip    :: Vector r1 a 
        -> Vector r2 b
        -> Vector (TZ r1 r2) (a, b)


-- Unboxed --------------------------------------------------------------------
instance (Elt a, U.Unbox a
         ,Elt b, U.Unbox b) 
       => Zip U U a b where
 type TZ U U            = D
 zip !arr1 !arr2        = zip (delay arr1) (delay arr2)
 {-# INLINE [4] zip #-}


-- Delayed --------------------------------------------------------------------
instance Zip D D a b where
 type TZ D D    = D
 zip !arr1 !arr2
  = fromFunction (extent arr1) get
  where get ix = ( index arr1 ix
                 , index arr2 ix )
        {-# INLINE get #-}
 {-# INLINE [4] zip #-}


-- Flow -----------------------------------------------------------------------
instance Zip (O mode BB) (O mode BB) a b where
 type TZ (O mode BB) (O mode BB)
        = O mode BB
 zip (AFlow ff1) (AFlow ff2)
  = AFlow (F.zip ff1 ff2)
 {-# INLINE [4] zip #-}


-- Flow/Unboxed ---------------------------------------------------------------
instance U.Unbox b => Zip (O mode BB) U a b where
 type TZ (O mode BB) U
        = O mode BB
 zip (AFlow ff1) (AUnboxed _ _ get)
  = AFlow (F.zipLeft ff1 get)
 {-# INLINE [4] zip #-}


instance U.Unbox a => Zip U (O mode BB) a b where
 type TZ U (O mode BB) 
        = O mode BB
 zip (AUnboxed _ _ get) (AFlow ff2) 
  = AFlow (F.map (\(x, y) -> (y, x)) $ F.zipLeft ff2 get)
 {-# INLINE [4] zip #-}


-- Flow/Delayed ---------------------------------------------------------------
instance Zip (O mode BB) D a b where
 type TZ (O mode BB) D
        = O mode BB
 zip (AFlow ff1) arr2
  = let get ix  = linearIndex arr2 (I# ix)
    in  AFlow (F.zipLeft ff1 get)
 {-# INLINE [4] zip #-}


instance Zip D (O mode BB) a b where
 type TZ D (O mode BB) 
        = O mode BB
 zip arr1 (AFlow ff2) 
  = let get ix  = linearIndex arr1 (I# ix)
    in  AFlow (F.map (\(x, y) -> (y, x)) $ F.zipLeft ff2 get)
 {-# INLINE [4] zip #-}


-- Unboxed/Delayed ------------------------------------------------------------
instance (Elt a, U.Unbox a)
      => Zip U D a b where
 type TZ U D            = D
 zip !arr1 !arr2        = zip (delay arr1) arr2
 {-# INLINE [4] zip #-}


instance (Elt b, U.Unbox b)
      => Zip D U a b where
 type TZ D U    = D
 zip !arr1 !arr2        = zip arr1 (delay arr2)
 {-# INLINE [4] zip #-}


-- Higher-arity zips ----------------------------------------------------------
-- NOTE: The first vector / second vector comments are there so Haddock
--       formats the type signatures in a semi-readable way.
-- 
zip3   :: ( Zip r1 (TZ r2 r3) a (b, c)
           , Zip r2 r3         b c
           , Map (TZ r1 (TZ r2 r3)) (a, (b, c)))
        => Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector (TM (TZ r1 (TZ r2 r3))) (a, b, c)

zip3 !vec1 !vec2 !vec3
 = map merge
 $ zip vec1 (zip vec2 vec3)
 where  merge (x, (y, z)) = (x, y, z)
        {-# INLINE merge #-}
{-# INLINE [4] zip3 #-}


zip4    :: ( Zip r1 (TZ r2 (TZ r3 r4)) a (b, (c, d))
           , Zip r2 (TZ r3 r4)         b (c, d)
           , Zip r3 r4                 c d
           , Map (TZ r1 (TZ r2 (TZ r3 r4))) 
                 (a, (b, (c, d))))
        => Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector r4 d                  -- ^ fourth vector
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 r4))))
                  (a, b, c, d)

zip4 !vec1 !vec2 !vec3 !vec4
 = map merge
 $ zip vec1 (zip vec2 (zip vec3 vec4))
 where  merge (x1, (x2, (x3, x4))) = (x1, x2, x3, x4)
        {-# INLINE merge #-}
{-# INLINE [4] zip4 #-}


zip5    :: ( Zip r1 (TZ r2 (TZ r3 (TZ r4 r5))) a (b, (c, (d, e)))
           , Zip r2 (TZ r3 (TZ r4 r5))         b (c, (d, e))
           , Zip r3 (TZ r4 r5)                 c (d, e)
           , Zip r4 r5                         d e
           , Map (TZ r1 (TZ r2 (TZ r3 (TZ r4 r5))))
                 (a, (b, (c, (d, e)))))
        => Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector r4 d                  -- ^ fourth vector
        -> Vector r5 e                  -- ^ fifth vector
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 (TZ r4 r5)))))
                  (a, b, c, d, e)

zip5 !vec1 !vec2 !vec3 !vec4 !vec5
 = map merge
 $ zip vec1 (zip vec2 (zip vec3 (zip vec4 vec5)))
 where  merge (x1, (x2, (x3, (x4, x5)))) = (x1, x2, x3, x4, x5)
        {-# INLINE merge #-}
{-# INLINE [4] zip5 #-}


zip6    :: ( Zip r1 (TZ r2 (TZ r3 (TZ r4 (TZ r5 r6)))) a (b, (c, (d, (e, f))))
           , Zip r2 (TZ r3 (TZ r4 (TZ r5 r6)))         b (c, (d, (e, f)))
           , Zip r3 (TZ r4 (TZ r5 r6))                 c (d, (e, f))
           , Zip r4 (TZ r5 r6)                         d (e, f)
           , Zip r5 r6                                 e f
           , Map (TZ r1 (TZ r2 (TZ r3 (TZ r4 (TZ r5 r6)))))
                 (a, (b, (c, (d, (e, f))))))
        => Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector r4 d                  -- ^ fourth vector
        -> Vector r5 e                  -- ^ fifth vector
        -> Vector r6 f                  -- ^ sixth vector
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 (TZ r4 (TZ r5 r6))))))
                  (a, b, c, d, e, f)

zip6 !vec1 !vec2 !vec3 !vec4 !vec5 !vec6
 = map merge
 $ zip vec1 (zip vec2 (zip vec3 (zip vec4 (zip vec5 vec6))))
 where  merge (x1, (x2, (x3, (x4, (x5, x6))))) = (x1, x2, x3, x4, x5, x6)
        {-# INLINE merge #-}
{-# INLINE [4] zip6 #-}


-- zipWiths -------------------------------------------------------------------
zipWith ::  ( Zip r1 r2 a b
            , Map (TZ r1 r2) (a, b))
         => (a -> b -> c)               -- ^ function.
         -> Vector r1 a                 -- ^ first vector.
         -> Vector r2 b                 -- ^ second vector.
         -> Vector (TM (TZ r1 r2)) c

zipWith f !vec1 !vec2
 = map merge
 $ zip vec1 vec2
 where  merge (x, y)    = f x y
        {-# INLINE  merge #-}
{-# INLINE [4] zipWith #-}


zipWith3 ::  ( Zip r1 (TZ r2 r3) a (b, c)
             , Zip r2 r3         b c
             , Map (TZ r1 (TZ r2 r3)) (a, (b, c)))
          => (a -> b -> c -> d)         -- ^ function
          -> Vector r1 a                -- ^ first vector
          -> Vector r2 b                -- ^ second vector
          -> Vector r3 c                -- ^ third vector
          -> Vector (TM (TZ r1 (TZ r2 r3))) d

zipWith3 f !vec1 !vec2 !vec3
 = map merge
 $ zip vec1 (zip vec2 vec3)
 where  merge (x1, (x2, x3)) = f x1 x2 x3
        {-# INLINE merge #-}
{-# INLINE [4] zipWith3 #-}


zipWith4 ::  ( Zip r1 (TZ r2 (TZ r3 r4)) a (b, (c, d))
             , Zip r2 (TZ r3 r4)         b (c, d)
             , Zip r3 r4                 c d
             , Map (TZ r1 (TZ r2 (TZ r3 r4))) 
                   (a, (b, (c, d))))
        => (a -> b -> c -> d -> e)      -- ^ function
        -> Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector r4 d                  -- ^ fourth vector
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 r4)))) e

zipWith4 f !vec1 !vec2 !vec3 !vec4
 = map merge
 $ zip vec1 (zip vec2 (zip vec3 vec4))
 where  merge (x1, (x2, (x3, x4))) = f x1 x2 x3 x4
        {-# INLINE merge #-}
{-# INLINE [4] zipWith4 #-}


zipWith5 ::  ( Zip r1 (TZ r2 (TZ r3 (TZ r4 r5))) a (b, (c, (d, e)))
             , Zip r2 (TZ r3 (TZ r4 r5))         b (c, (d, e))
             , Zip r3 (TZ r4 r5)                 c (d, e)
             , Zip r4 r5                         d e
             , Map (TZ r1 (TZ r2 (TZ r3 (TZ r4 r5))))
                   (a, (b, (c, (d, e)))))
        => (a -> b -> c -> d -> e -> f) -- ^ function
        -> Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector r4 d                  -- ^ fourth vector
        -> Vector r5 e                  -- ^ fifth vector
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 (TZ r4 r5))))) f

zipWith5 f !vec1 !vec2 !vec3 !vec4 !vec5 
 = map merge
 $ zip vec1 (zip vec2 (zip vec3 (zip vec4 vec5)))
 where  merge (x1, (x2, (x3, (x4, x5)))) = f x1 x2 x3 x4 x5
        {-# INLINE merge #-}
{-# INLINE [4] zipWith5 #-}


zipWith6  :: ( Zip r1 (TZ r2 (TZ r3 (TZ r4 (TZ r5 r6)))) a (b, (c, (d, (e, f))))
             , Zip r2 (TZ r3 (TZ r4 (TZ r5 r6)))         b (c, (d, (e, f)))
             , Zip r3 (TZ r4 (TZ r5 r6))                 c (d, (e, f))
             , Zip r4 (TZ r5 r6)                         d (e, f)
             , Zip r5 r6                                 e f
             , Map (TZ r1 (TZ r2 (TZ r3 (TZ r4 (TZ r5 r6)))))
                   (a, (b, (c, (d, (e, f))))))
        => (a -> b -> c -> d -> e -> f -> g)    -- ^ function
        -> Vector r1 a                          -- ^ first vector
        -> Vector r2 b                          -- ^ second vector
        -> Vector r3 c                          -- ^ third vector
        -> Vector r4 d                          -- ^ fourth vector
        -> Vector r5 e                          -- ^ fifth vector
        -> Vector r6 f
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 (TZ r4 (TZ r5 r6)))))) g

zipWith6 f !vec1 !vec2 !vec3 !vec4 !vec5 !vec6
 = map merge
 $ zip vec1 (zip vec2 (zip vec3 (zip vec4 (zip vec5 vec6))))
 where  merge (x1, (x2, (x3, (x4, (x5, x6))))) = f x1 x2 x3 x4 x5 x6
        {-# INLINE merge #-}
{-# INLINE [4] zipWith6 #-}
