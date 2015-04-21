
module Data.Repa.Convert.Mask 
        ( -- * Generic masking
          Drop (..)
        , Keep (..)
        , Maskable (..)        

          -- * Boilerplate for specific types
          -- | In future it would be better to generate these cases
          --   on the fly using Template Haskell...
        , mask2_1, mask2_2
        , mask3_1, mask3_2, mask3_3
        , mask4_1, mask4_2, mask4_3, mask4_4
        , mask5_1, mask5_2, mask5_3, mask5_4, mask5_5
        , mask6_1, mask6_2, mask6_3, mask6_4, mask6_5, mask6_6
        , mask7_1, mask7_2, mask7_3, mask7_4, mask7_5, mask7_6, mask7_7
        , mask8_1, mask8_2, mask8_3, mask8_4, mask8_5, mask8_6, mask8_7, mask8_8)
where
import Data.Repa.Product


---------------------------------------------------------------------------------------------------
-- | Singleton to indicate a field that should be dropped.
data Drop = Drop


-- | Singleton to indicate a field that should be kept.
data Keep = Keep


-- | Class of data types that can have parts masked out.
class Maskable mask full where
 type Masked mask full
 -- | Mask out some component of a type.
 --
 -- @  
 -- mask (Keep :*: Drop  :*: Keep :*: ()) 
 --      (1    :*: \"foo\" :*: \'a\'  :*: ())   =   (1 :*: \'a\' :*: ())
 --
 -- mask (Drop :*: Drop  :*: Drop :*: ()) 
 --      (1    :*: \"foo\" :*: \'a\'  :*: ())   =   ()
 -- @
 --
 mask :: mask -> full -> Masked mask full


instance Maskable Keep t2 where
 type Masked Keep t2 = t2
 mask Keep x2 = x2

instance Maskable Drop t2 where
 type Masked Drop t2 = ()
 mask Drop x2 = ()

instance Maskable () () where
 type Masked ()   () = ()
 mask () ()   = ()


instance Maskable mask t2 
      => Maskable (Keep :*: mask) (t1 :*: t2) where
 type Masked (Keep :*: m2) (t1 :*: t2) 
        = t1 :*: Masked m2 t2
 mask (_ :*: m2) (x1 :*: x2) = x1 :*: mask m2 x2


instance Maskable mask t2
      => Maskable (Drop :*: mask) (t1 :*: t2)  where
 type Masked (Drop :*: m2) (t1 :*: t2)
        = Masked m2 t2
 mask (_ :*: m2) (x1 :*: x2) = mask m2 x2


---------------------------------------------------------------------------------------------------
-- Boilerlpate for masking specific fields in a product.
-- There isn't a better way to represent this in the type system,
-- so we just list out all the cases. In future it would be better
-- to generate the code on the fly with template-haskell.

-- 2
mask2_1 :: (a :*: b) -> b
mask2_2 :: (a :*: b) -> a

mask2_1    (_ :*: b) = b
mask2_2    (a :*: _) = a

{-# INLINE mask2_1 #-}
{-# INLINE mask2_2 #-}


-- 3
mask3_1 :: (a :*: b :*: c) -> b :*: c
mask3_2 :: (a :*: b :*: c) -> a :*: c
mask3_3 :: (a :*: b :*: c) -> a :*: b

mask3_1    (_ :*: b :*: c) =  b :*: c
mask3_2    (a :*: _ :*: c) =  a :*: c
mask3_3    (a :*: b :*: _) =  a :*: b

{-# INLINE mask3_1 #-}
{-# INLINE mask3_2 #-}
{-# INLINE mask3_3 #-}


-- 4
mask4_1 :: (a :*: b :*: c :*: d) -> b :*: c :*: d
mask4_2 :: (a :*: b :*: c :*: d) -> a :*: c :*: d
mask4_3 :: (a :*: b :*: c :*: d) -> a :*: b :*: d
mask4_4 :: (a :*: b :*: c :*: d) -> a :*: b :*: c

mask4_1    (_ :*: b :*: c :*: d) =  b :*: c :*: d
mask4_2    (a :*: _ :*: c :*: d) =  a :*: c :*: d
mask4_3    (a :*: b :*: _ :*: d) =  a :*: b :*: d
mask4_4    (a :*: b :*: c :*: _) =  a :*: b :*: c

{-# INLINE mask4_1 #-}
{-# INLINE mask4_2 #-}
{-# INLINE mask4_3 #-}
{-# INLINE mask4_4 #-}


-- 5
mask5_1 :: (a :*: b :*: c :*: d :*: e) -> b :*: c :*: d :*: e
mask5_2 :: (a :*: b :*: c :*: d :*: e) -> a :*: c :*: d :*: e
mask5_3 :: (a :*: b :*: c :*: d :*: e) -> a :*: b :*: d :*: e
mask5_4 :: (a :*: b :*: c :*: d :*: e) -> a :*: b :*: c :*: e
mask5_5 :: (a :*: b :*: c :*: d :*: e) -> a :*: b :*: c :*: d

mask5_1    (_ :*: b :*: c :*: d :*: e) =  b :*: c :*: d :*: e
mask5_2    (a :*: _ :*: c :*: d :*: e) =  a :*: c :*: d :*: e
mask5_3    (a :*: b :*: _ :*: d :*: e) =  a :*: b :*: d :*: e
mask5_4    (a :*: b :*: c :*: _ :*: e) =  a :*: b :*: c :*: e
mask5_5    (a :*: b :*: c :*: d :*: _) =  a :*: b :*: c :*: d

{-# INLINE mask5_1 #-}
{-# INLINE mask5_2 #-}
{-# INLINE mask5_3 #-}
{-# INLINE mask5_4 #-}
{-# INLINE mask5_5 #-}


-- 6
mask6_1 :: (a :*: b :*: c :*: d :*: e :*: f) -> b :*: c :*: d :*: e :*: f
mask6_2 :: (a :*: b :*: c :*: d :*: e :*: f) -> a :*: c :*: d :*: e :*: f
mask6_3 :: (a :*: b :*: c :*: d :*: e :*: f) -> a :*: b :*: d :*: e :*: f
mask6_4 :: (a :*: b :*: c :*: d :*: e :*: f) -> a :*: b :*: c :*: e :*: f
mask6_5 :: (a :*: b :*: c :*: d :*: e :*: f) -> a :*: b :*: c :*: d :*: f
mask6_6 :: (a :*: b :*: c :*: d :*: e :*: f) -> a :*: b :*: c :*: d :*: e

mask6_1    (_ :*: b :*: c :*: d :*: e :*: f) =  b :*: c :*: d :*: e :*: f
mask6_2    (a :*: _ :*: c :*: d :*: e :*: f) =  a :*: c :*: d :*: e :*: f
mask6_3    (a :*: b :*: _ :*: d :*: e :*: f) =  a :*: b :*: d :*: e :*: f
mask6_4    (a :*: b :*: c :*: _ :*: e :*: f) =  a :*: b :*: c :*: e :*: f
mask6_5    (a :*: b :*: c :*: d :*: _ :*: f) =  a :*: b :*: c :*: d :*: f
mask6_6    (a :*: b :*: c :*: d :*: e :*: _) =  a :*: b :*: c :*: d :*: e

{-# INLINE mask6_1 #-}
{-# INLINE mask6_2 #-}
{-# INLINE mask6_3 #-}
{-# INLINE mask6_4 #-}
{-# INLINE mask6_5 #-}
{-# INLINE mask6_6 #-}


-- 7
mask7_1 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> b :*: c :*: d :*: e :*: f :*: g
mask7_2 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> a :*: c :*: d :*: e :*: f :*: g
mask7_3 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> a :*: b :*: d :*: e :*: f :*: g
mask7_4 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> a :*: b :*: c :*: e :*: f :*: g
mask7_5 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> a :*: b :*: c :*: d :*: f :*: g
mask7_6 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> a :*: b :*: c :*: d :*: e :*: g
mask7_7 :: (a :*: b :*: c :*: d :*: e :*: f :*: g) -> a :*: b :*: c :*: d :*: e :*: f

mask7_1    (_ :*: b :*: c :*: d :*: e :*: f :*: g) =  b :*: c :*: d :*: e :*: f :*: g 
mask7_2    (a :*: _ :*: c :*: d :*: e :*: f :*: g) =  a :*: c :*: d :*: e :*: f :*: g
mask7_3    (a :*: b :*: _ :*: d :*: e :*: f :*: g) =  a :*: b :*: d :*: e :*: f :*: g
mask7_4    (a :*: b :*: c :*: _ :*: e :*: f :*: g) =  a :*: b :*: c :*: e :*: f :*: g
mask7_5    (a :*: b :*: c :*: d :*: _ :*: f :*: g) =  a :*: b :*: c :*: d :*: f :*: g
mask7_6    (a :*: b :*: c :*: d :*: e :*: _ :*: g) =  a :*: b :*: c :*: d :*: e :*: g
mask7_7    (a :*: b :*: c :*: d :*: e :*: f :*: _) =  a :*: b :*: c :*: d :*: e :*: f

{-# INLINE mask7_1 #-}
{-# INLINE mask7_2 #-}
{-# INLINE mask7_3 #-}
{-# INLINE mask7_4 #-}
{-# INLINE mask7_5 #-}
{-# INLINE mask7_6 #-}
{-# INLINE mask7_7 #-}


-- 8
mask8_1 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> b :*: c :*: d :*: e :*: f :*: g :*: h
mask8_2 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: c :*: d :*: e :*: f :*: g :*: h
mask8_3 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: b :*: d :*: e :*: f :*: g :*: h
mask8_4 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: b :*: c :*: e :*: f :*: g :*: h
mask8_5 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: b :*: c :*: d :*: f :*: g :*: h
mask8_6 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: b :*: c :*: d :*: e :*: g :*: h
mask8_7 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: b :*: c :*: d :*: e :*: f :*: h
mask8_8 :: (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> a :*: b :*: c :*: d :*: e :*: f :*: g

mask8_1    (_ :*: b :*: c :*: d :*: e :*: f :*: g :*: h) =  b :*: c :*: d :*: e :*: f :*: g :*: h 
mask8_2    (a :*: _ :*: c :*: d :*: e :*: f :*: g :*: h) =  a :*: c :*: d :*: e :*: f :*: g :*: h
mask8_3    (a :*: b :*: _ :*: d :*: e :*: f :*: g :*: h) =  a :*: b :*: d :*: e :*: f :*: g :*: h
mask8_4    (a :*: b :*: c :*: _ :*: e :*: f :*: g :*: h) =  a :*: b :*: c :*: e :*: f :*: g :*: h
mask8_5    (a :*: b :*: c :*: d :*: _ :*: f :*: g :*: h) =  a :*: b :*: c :*: d :*: f :*: g :*: h
mask8_6    (a :*: b :*: c :*: d :*: e :*: _ :*: g :*: h) =  a :*: b :*: c :*: d :*: e :*: g :*: h
mask8_7    (a :*: b :*: c :*: d :*: e :*: f :*: _ :*: h) =  a :*: b :*: c :*: d :*: e :*: f :*: h
mask8_8    (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) =  a :*: b :*: c :*: d :*: e :*: f :*: g

{-# INLINE mask8_1 #-}
{-# INLINE mask8_2 #-}
{-# INLINE mask8_3 #-}
{-# INLINE mask8_4 #-}
{-# INLINE mask8_5 #-}
{-# INLINE mask8_6 #-}
{-# INLINE mask8_7 #-}
{-# INLINE mask8_8 #-}

