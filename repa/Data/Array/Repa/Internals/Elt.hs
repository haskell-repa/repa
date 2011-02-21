
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Data.Array.Repa.Internals.Elt
	(Elt (..))
where
import GHC.Prim
import GHC.Exts
import GHC.Types
import Data.Word
import Data.Vector.Unboxed			as V
	
class V.Unbox a	=> Elt a where
	touch :: a -> IO ()
	touch x = return ()

instance Elt Float where
 {-# INLINE touch #-}
 touch (F# f) 
  = IO (\state -> case touch# f state of
			state' -> (# state', () #))


instance Elt Bool
instance Elt Int
instance Elt Double
instance Elt Word8

