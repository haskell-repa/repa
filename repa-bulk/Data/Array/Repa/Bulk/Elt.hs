-- | Values that can be stored in Repa Arrays.
module Data.Array.Repa.Bulk.Elt
	(Elt (..))
where
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Word
import GHC.Int


-- Note that the touch# function is special because we can pass it boxed or unboxed
-- values. The argument type has kind ?, not just * or #.

-- | Element types that can be used with the blockwise filling functions.
--  
--   This class is mainly used to define the `touch` method. This is used internally
--   in the imeplementation of Repa to prevent let-binding from being floated
--   inappropriately by the GHC simplifier.  Doing a `seq` sometimes isn't enough,
--   because the GHC simplifier can erase these, and still move around the bindings.
--
class Elt a where

	-- | Place a demand on a value at a particular point in an IO computation.
	touch :: a -> IO ()

	-- | Generic zero value, helpful for debugging.
	zero  :: a

	-- | Generic one value, helpful for debugging.
	one   :: a


-- Bool -----------------------------------------------------------------------
instance Elt Bool where
 {-# INLINE touch #-}
 touch b
  = IO (\state -> case touch# b state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = False

 {-# INLINE one #-}
 one  = True


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


-- Int ------------------------------------------------------------------------
instance Elt Int where
 {-# INLINE touch #-}
 touch (I# i)
  = IO (\state -> case touch# i state of
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


instance Elt Int32 where
 {-# INLINE touch #-}
 touch (I32# w)
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Int64 where
 {-# INLINE touch #-}
 touch (I64# w)
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


-- Word -----------------------------------------------------------------------
instance Elt Word where
 {-# INLINE touch #-}
 touch (W# i)
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


instance Elt Word32 where
 {-# INLINE touch #-}
 touch (W32# w)
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


instance Elt Word64 where
 {-# INLINE touch #-}
 touch (W64# w)
  = IO (\state -> case touch# w state of
			state' -> (# state', () #))

 {-# INLINE zero #-}
 zero = 0

 {-# INLINE one #-}
 one = 1


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


instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
 {-# INLINE touch #-}
 touch (a, b, c)
  = do	touch a
	touch b
	touch c

 {-# INLINE zero #-}
 zero = (zero, zero, zero)

 {-# INLINE one #-}
 one =  (one, one, one)


instance (Elt a, Elt b, Elt c, Elt d) => Elt (a, b, c, d) where
 {-# INLINE touch #-}
 touch (a, b, c, d)
  = do	touch a
	touch b
	touch c
	touch d

 {-# INLINE zero #-}
 zero = (zero, zero, zero, zero)

 {-# INLINE one #-}
 one =  (one, one, one, one)


instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Elt (a, b, c, d, e) where
 {-# INLINE touch #-}
 touch (a, b, c, d, e)
  = do	touch a
	touch b
	touch c
	touch d
	touch e

 {-# INLINE zero #-}
 zero = (zero, zero, zero, zero, zero)

 {-# INLINE one #-}
 one =  (one, one, one, one, one)


instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => Elt (a, b, c, d, e, f) where
 {-# INLINE touch #-}
 touch (a, b, c, d, e, f)
  = do	touch a
	touch b
	touch c
	touch d
	touch e
	touch f

 {-# INLINE zero #-}
 zero = (zero, zero, zero, zero, zero, zero)

 {-# INLINE one #-}
 one =  (one, one, one, one, one, one)


