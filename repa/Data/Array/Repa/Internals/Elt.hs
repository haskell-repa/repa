-- | Values that can be stored in Repa Arrays.
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Data.Array.Repa.Internals.Elt
	(Elt (..))
where
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Word
import Data.Vector.Unboxed
	
class Unbox a	=> Elt a where

	-- | We use this to prevent bindings from being floated inappropriatey.
	--   Doing a `seq` sometimes isn't enough, because the GHC simpplifier can 
	--   erase these, and/or still move around the bindings.
	touch :: a -> IO ()


-- Note that the touch# function is special because we can pass it boxed or unboxed
-- values. The argument type has kind ?, not just * or #.

instance Elt Bool where
 {-# INLINE touch #-}
 touch b
  = IO (\state -> case touch# b state of
			state' -> (# state', () #))


instance Elt Float where
 {-# INLINE touch #-}
 touch (F# f) 
  = IO (\state -> case touch# f state of
			state' -> (# state', () #))


instance Elt Double where
 {-# INLINE touch #-}
 touch (D# d)
  = IO (\state -> case touch# d state of
			state' -> (# state', () #))


instance Elt Int where
 {-# INLINE touch #-}
 touch (I# i) 
  = IO (\state -> case touch# i state of
			state' -> (# state', () #))


instance Elt Word8 where
 {-# INLINE touch #-}
 touch (W8# w) 
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))
	
