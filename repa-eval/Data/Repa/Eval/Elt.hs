
-- | Values that can be stored in Repa Arrays.
module Data.Repa.Eval.Elt
        (Elt (..))
where
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Word
import GHC.Int
import GHC.Generics


-- Note that the touch# function is special because we can pass it boxed or unboxed
-- values. The argument type has kind ?, not just * or #.

-- | Element types that can be used with the blockwise filling functions.
--
--   This class is mainly used to define the `touch` method. This is used internally
--   in the imeplementation of Repa to prevent let-binding from being floated
--   inappropriately by the GHC simplifier.  Doing a `seq` sometimes isn't enough,
--   because the GHC simplifier can erase these, and still move around the bindings.
--
--   This class supports the generic deriving mechanism, 
--   use @deriving instance Elt (TYPE)@
--
class Elt a where

        -- | Place a demand on a value at a particular point in an IO computation.
        touch :: a -> IO ()

        default touch :: (Generic a, GElt (Rep a)) => a -> IO ()
        touch = gtouch . from
        {-# INLINE touch #-}

        -- | Generic zero value, helpful for debugging.
        zero  :: a

        default zero :: (Generic a, GElt (Rep a)) => a
        zero = to gzero
        {-# INLINE zero #-}

        -- | Generic one value, helpful for debugging.
        one   :: a

        default one :: (Generic a, GElt (Rep a)) => a
        one = to gone
        {-# INLINE one #-}

class GElt f where
        -- | Generic version of touch
        gtouch :: f a -> IO ()

        -- | Generic version of zero
        gzero  :: f a

        -- | Generic version of gone
        gone   :: f a


-- Generic Definition ----------------------------------------------------------

instance GElt U1 where
  gtouch _ = return ()
  {-# INLINE gtouch #-}

  gzero = U1
  {-# INLINE gzero #-}

  gone = U1
  {-# INLINE gone #-}

instance (GElt a, GElt b) => GElt (a :*: b) where
  gtouch (x :*: y) = gtouch x >> gtouch y
  {-# INLINE gtouch #-}

  gzero = gzero :*: gzero
  {-# INLINE gzero #-}

  gone  = gone :*: gone
  {-# INLINE gone #-}

instance (GElt a, GElt b) => GElt (a :+: b) where
  gtouch (L1 x) = gtouch x
  gtouch (R1 x) = gtouch x
  {-# INLINE gtouch #-}

  gzero = L1 gzero
  {-# INLINE gzero #-}

  gone  = R1 gone
  {-# INLINE gone #-}

instance (GElt a) => GElt (M1 i c a) where
  gtouch (M1 x) = gtouch x
  {-# INLINE gtouch #-}

  gzero = M1 gzero
  {-# INLINE gzero #-}

  gone  = M1 gone
  {-# INLINE gone #-}

instance (Elt a) => GElt (K1 i a) where
  gtouch (K1 x) = touch x
  {-# INLINE gtouch #-}

  gzero = K1 zero
  {-# INLINE gzero #-}

  gone = K1 one
  {-# INLINE gone #-}


-- Bool -----------------------------------------------------------------------
instance Elt Bool where
 touch b
  = IO (\state -> case touch# b state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = False
 {-# INLINE zero #-}

 one  = True
 {-# INLINE one #-}


-- Char -----------------------------------------------------------------------
instance Elt Char where
 touch c
  = IO (\state -> case touch# c state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = '0'
 {-# INLINE zero #-}

 one  = '1'
 {-# INLINE one #-}


-- Floating -------------------------------------------------------------------
instance Elt Float where
 touch (F# f)
  = IO (\state -> case touch# f state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Double where
 touch (D# d)
  = IO (\state -> case touch# d state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


-- Int ------------------------------------------------------------------------
instance Elt Int where
 touch (I# i)
  = IO (\state -> case touch# i state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Int8 where
 touch (I8# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Int16 where
 touch (I16# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Int32 where
 touch (I32# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Int64 where
 touch (I64# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


-- Word -----------------------------------------------------------------------
instance Elt Word where
 touch (W# i)
  = IO (\state -> case touch# i state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Word8 where
 touch (W8# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Word16 where
 touch (W16# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Word32 where
 touch (W32# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


instance Elt Word64 where
 touch (W64# w)
  = IO (\state -> case touch# w state of
                        state' -> (# state', () #))
 {-# INLINE touch #-}

 zero = 0
 {-# INLINE zero #-}

 one = 1
 {-# INLINE one #-}


-- Tuple ----------------------------------------------------------------------
instance (Elt a, Elt b) => Elt (a, b) where
 touch (a, b)
  = do  touch a
        touch b
 {-# INLINE touch #-}

 zero = (zero, zero)
 {-# INLINE zero #-}

 one =  (one, one)
 {-# INLINE one #-}


instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
 touch (a, b, c)
  = do  touch a
        touch b
        touch c
 {-# INLINE touch #-}

 zero = (zero, zero, zero)
 {-# INLINE zero #-}

 one =  (one, one, one)
 {-# INLINE one #-}


instance (Elt a, Elt b, Elt c, Elt d) => Elt (a, b, c, d) where
 touch (a, b, c, d)
  = do  touch a
        touch b
        touch c
        touch d
 {-# INLINE touch #-}

 zero = (zero, zero, zero, zero)
 {-# INLINE zero #-}

 one =  (one, one, one, one)
 {-# INLINE one #-}


instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Elt (a, b, c, d, e) where
 touch (a, b, c, d, e)
  = do  touch a
        touch b
        touch c
        touch d
        touch e
 {-# INLINE touch #-}

 zero = (zero, zero, zero, zero, zero)
 {-# INLINE zero #-}

 one =  (one, one, one, one, one)
 {-# INLINE one #-}


instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => Elt (a, b, c, d, e, f) where
 touch (a, b, c, d, e, f)
  = do  touch a
        touch b
        touch c
        touch d
        touch e
        touch f
 {-# INLINE touch #-}

 zero = (zero, zero, zero, zero, zero, zero)
 {-# INLINE zero #-}

 one =  (one, one, one, one, one, one)
 {-# INLINE one #-}


