-- | Values that can be stored in Repa Arrays.
{-# LANGUAGE MagicHash, UnboxedTuples, TypeSynonymInstances, FlexibleInstances #-}
module Data.Array.Repa.Internals.Elt
	(Elt (..))
where
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Word
import GHC.Int
import Data.Vector.Unboxed
import Data.Array.Repa.Index
	
class (Show a, Unbox a)	=> Elt a where

	-- | We use this to prevent bindings from being floated inappropriatey.
	--   Doing a `seq` sometimes isn't enough, because the GHC simpplifier can 
	--   erase these, and/or still move around the bindings.
	touch :: a -> IO ()

	zero  :: a
	one   :: a

-- Note that the touch# function is special because we can pass it boxed or unboxed
-- values. The argument type has kind ?, not just * or #.

instance Elt Bool where
 {-# INLINE touch #-}
 touch b
  = IO (\state -> case touch# b state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = False

 {-# INLINE one #-}
 one  = True

-- Tuple ----------------------------------------------------------------------
instance (Elt a, Elt b) => Elt (a, b) where
 {-# INLINE touch #-}
 touch (a, b) 
  = do	touch a
	touch b
	
 {-# INLINE zero #-}
 zero = (zero, zero)

 {-# INLINE one #-}
 one =  (one, one)


-- Floating -------------------------------------------------------------------
instance Elt Float where
 {-# INLINE touch #-}
 touch (F# f) 
  = IO (\state -> case touch# f state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Double where
 {-# INLINE touch #-}
 touch (D# d)
  = IO (\state -> case touch# d state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


-- Integral -------------------------------------------------------------------
instance Elt Int where
 {-# INLINE touch #-}
 touch (I# i) 
  = IO (\state -> case touch# i state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Word8 where
 {-# INLINE touch #-}
 touch (W8# w) 
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Word16 where
 {-# INLINE touch #-}
 touch (W16# w) 
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Int8 where
 {-# INLINE touch #-}
 touch (I8# w) 
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Int16 where
 {-# INLINE touch #-}
 touch (I16# w) 
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1
	
