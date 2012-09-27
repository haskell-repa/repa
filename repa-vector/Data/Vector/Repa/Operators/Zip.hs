{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Vector.Repa.Operators.Zip
        ( Zip(..)
        , vzip3
        , vzip4
        , vzipWith
        , vzipWith3
        , vzipWith4)
where
import Data.Array.Repa
import Data.Vector.Repa.Repr.Chain
import Data.Vector.Repa.Repr.Sliced
import Data.Vector.Repa.Base
import qualified Data.Array.Repa                as R
import qualified Data.Vector.Unboxed            as U
import Prelude                                  hiding (zip3, zipWith)


-- Unboxed --------------------------------------------------------------------
instance (U.Unbox a, U.Unbox b) 
       => Zip U U a b where
 type TZ U U            = D
 vzip !arr1 !arr2       = vzip (delay arr1) (delay arr2)
 {-# INLINE [4] vzip #-}


-- Delayed --------------------------------------------------------------------
instance Zip D D a b where
 type TZ D D    = D
 vzip !arr1 !arr2
  = fromFunction (extent arr1) get
  where get ix = ( arr1 `unsafeIndex` ix
                 , arr2 `unsafeIndex` ix)
        {-# INLINE get #-}
 {-# INLINE [4] vzip #-}


-- Chained --------------------------------------------------------------------
instance Zip N N a b where
 type TZ N N    = N
 vzip (AChained sh1 frags mkFrag1 vec1) 
      (AChained _   _     mkFrag2 vec2)
  =    AChained sh1 frags mkFrag (R.zipWith (,) vec1 vec2)

  where mkFrag c 
         | Frag start1   end1 s10 mkStep1 <- mkFrag1 c
         , Frag _start2 _end2 s20 mkStep2 <- mkFrag2 c
         = let  
                mkStep ix (s1, s2)
                 | Yield s1' x1  <- mkStep1 ix s1
                 , Yield s2' x2  <- mkStep2 ix s2
                 = Yield (s1', s2') (x1, x2)

                 | otherwise
                 = error "vzip: source vectors have different lengths"
                {-# INLINE mkStep #-}

           in   Frag start1 end1 (s10, s20) mkStep
        {-# INLINE mkFrag #-}


-- Sliced ---------------------------------------------------------------------
instance Zip r1 r2 a b
      => Zip (S r1) (S r2) a b where
 type TZ (S r1) (S r2) = S (TZ r1 r2)

 vzip  (ASliced start shape arr1)
        (ASliced _     _     arr2)
  = ASliced start shape (vzip arr1 arr2)
 {-# INLINE [4] vzip #-}


-- Unboxed/Delayed ------------------------------------------------------------
instance U.Unbox a
      => Zip U D a b where
 type TZ U D            = D
 vzip !arr1 !arr2       = vzip (delay arr1) arr2
 {-# INLINE [4] vzip #-}


instance U.Unbox b
      => Zip D U a b where
 type TZ D U    = D
 vzip !arr1 !arr2       = vzip arr1 (delay arr2)
 {-# INLINE [4] vzip #-}


-- Unboxed/Chained ------------------------------------------------------------
instance U.Unbox a => Zip U N a b where
 type TZ U N            = N
 vzip !arr1 !arr2       = vzip (chain arr1) arr2
 {-# INLINE [4] vzip #-}


instance U.Unbox b => Zip N U a b where
 type TZ N U            = N
 vzip !arr1 !arr2       = vzip arr1 (chain arr2)
 {-# INLINE [4] vzip #-}


-- Delayed/Chained ------------------------------------------------------------
instance Zip D N a b where
 type TZ D N            = N
 vzip !arr1 !arr2       = vzip (chain arr1) arr2
 {-# INLINE [4] vzip #-}


instance Zip N D a b where
 type TZ N D            = N
 vzip !arr1 !arr2       = vzip arr1 (chain arr2)
 {-# INLINE [4] vzip #-}


-- Higher-arity zips ----------------------------------------------------------
-- NOTE: The first vector / second vector comments are there so Haddock
--       formats the type signatures in a semi-readable way.
-- 
vzip3   :: ( Zip r1 (TZ r2 r3) a (b, c)
           , Zip r2 r3         b c
           , Map (TZ r1 (TZ r2 r3)) (a, (b, c)))
        => Vector r1 a                  -- ^ first vector
        -> Vector r2 b                  -- ^ second vector
        -> Vector r3 c                  -- ^ third vector
        -> Vector (TM (TZ r1 (TZ r2 r3))) (a, b, c)

vzip3 !vec1 !vec2 !vec3
 = vmap merge
 $ vzip vec1 (vzip vec2 vec3)
 where  merge (x, (y, z)) = (x, y, z)
        {-# INLINE merge #-}
{-# INLINE [4] vzip3 #-}


vzip4   :: ( Zip r1 (TZ r2 (TZ r3 r4)) a (b, (c, d))
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

vzip4 !vec1 !vec2 !vec3 !vec4
 = vmap merge
 $ vzip vec1 (vzip vec2 (vzip vec3 vec4))
 where  merge (x1, (x2, (x3, x4))) = (x1, x2, x3, x4)
        {-# INLINE merge #-}
{-# INLINE [4] vzip4 #-}


-- zipWiths -------------------------------------------------------------------
vzipWith :: ( Zip r1 r2 a b
            , Map (TZ r1 r2) (a, b))
         => (a -> b -> c)               -- ^ function.
         -> Vector r1 a                 -- ^ first vector.
         -> Vector r2 b                 -- ^ second vector.
         -> Vector (TM (TZ r1 r2)) c

vzipWith f !vec1 !vec2
 = vmap merge
 $ vzip vec1 vec2
 where  merge (x, y)    = f x y
        {-# INLINE  merge #-}
{-# INLINE [4] vzipWith #-}


vzipWith3 :: ( Zip r1 (TZ r2 r3) a (b, c)
             , Zip r2 r3         b c
             , Map (TZ r1 (TZ r2 r3)) (a, (b, c)))
          => (a -> b -> c -> d)         -- ^ function
          -> Vector r1 a                -- ^ first vector
          -> Vector r2 b                -- ^ second vector
          -> Vector r3 c                -- ^ third vector
          -> Vector (TM (TZ r1 (TZ r2 r3))) d

vzipWith3 f !vec1 !vec2 !vec3
 = vmap merge
 $ vzip vec1 (vzip vec2 vec3)
 where  merge (x1, (x2, x3)) = f x1 x2 x3
        {-# INLINE merge #-}
{-# INLINE [4] vzipWith3 #-}


vzipWith4 :: ( Zip r1 (TZ r2 (TZ r3 r4)) a (b, (c, d))
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

vzipWith4 f !vec1 !vec2 !vec3 !vec4
 = vmap merge
 $ vzip vec1 (vzip vec2 (vzip vec3 vec4))
 where  merge (x1, (x2, (x3, x4))) = f x1 x2 x3 x4
        {-# INLINE merge #-}
{-# INLINE [4] vzipWith4 #-}

