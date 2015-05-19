{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Singleton-typed natural numbers and arithmetic.
-- 
--   Used for indexing into hetrogenous list types.
--
module Data.Repa.Scalar.Singleton.Nat
        ( N   (..)
        , Nat (..)
        , Add (..)
        , Mul (..)

        -- * Literals
        , nat0, nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9)
where


-- | Peano natural numbers.
data N  = Z | S N


deriving instance Show N


-- | Peano natural number singletons.
data Nat (n :: N) where
        Zero    :: Nat Z
        Succ    :: Nat n -> Nat (S n)

deriving instance Show (Nat n)


---------------------------------------------------------------------------------------------------
class Add x y where
 type AddR x y :: N
 -- | Addition of singleton typed natural numbers.
 add  :: Nat x -> Nat y -> Nat (AddR x y)

instance Add Z x where
 type AddR Z y   = y
 add Zero y      = y
 {-# INLINE add #-}

instance Add x (S y) => Add (S x) y where
 type AddR (S x) y = AddR x (S y)
 add (Succ x) y    = add x  (Succ y)
 {-# INLINE add #-}


---------------------------------------------------------------------------------------------------
class Mul x y where
 type MulR x y :: N
 -- | Multiplication of singleton typed natural numbers.
 mul  :: Nat x -> Nat y -> Nat (MulR x y)

instance Mul Z x where
 type MulR  Z       y = Z
 mul        Zero    _ = Zero

instance (Mul x y, Add (MulR x y) y) 
       => Mul (S x) y where
 type MulR (S x)    y = AddR (MulR x y) y
 mul       (Succ x) y = add  (mul  x y) y


---------------------------------------------------------------------------------------------------
nat0    :: Nat Z
nat0    =  Zero
{-# INLINE nat0 #-}

nat1    :: Nat (S Z)
nat1    =  Succ Zero
{-# INLINE nat1 #-}

nat2    :: Nat (S (S Z))
nat2    =  Succ $ Succ Zero
{-# INLINE nat2 #-}

nat3    :: Nat (S (S (S Z)))
nat3    =  Succ $ Succ $ Succ Zero
{-# INLINE nat3 #-}

nat4    :: Nat (S (S (S (S Z))))
nat4    =  Succ $ Succ $ Succ $ Succ Zero
{-# INLINE nat4 #-}

nat5    :: Nat (S (S (S (S (S Z)))))
nat5    =  Succ $ Succ $ Succ $ Succ $ Succ Zero
{-# INLINE nat5 #-}

nat6    :: Nat (S (S (S (S (S (S Z))))))
nat6    =  Succ $ Succ $ Succ $ Succ $ Succ $ Succ Zero
{-# INLINE nat6 #-}

nat7    :: Nat (S (S (S (S (S (S (S Z)))))))
nat7    =  Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ Zero
{-# INLINE nat7 #-}

nat8    :: Nat (S (S (S (S (S (S (S (S Z))))))))
nat8    =  Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ Zero
{-# INLINE nat8 #-}

nat9    :: Nat (S (S (S (S (S (S (S (S (S Z)))))))))
nat9    =  Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ Zero
{-# INLINE nat9 #-}

