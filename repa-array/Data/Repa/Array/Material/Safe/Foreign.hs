
module Data.Repa.Array.Material.Safe.Foreign
        ( F, Array (..)
        , fromForeignPtr, toForeignPtr
        , fromByteString, toByteString)
where
import Data.Repa.Array.Material.Unsafe.Foreign     (UF(..), Array(..))
import qualified  Data.Repa.Array.Material.Unsafe.Foreign  as UF
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Checked
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Bulk
import Foreign.ForeignPtr
import Data.Word
import Data.ByteString                          (ByteString)


-- | Arrays represented as foreign buffers in the C heap.
type F = K UF


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr :: Shape sh => sh -> ForeignPtr a -> Array F sh a
fromForeignPtr !sh !fptr
        = checked $ UF.fromForeignPtr sh fptr
{-# INLINE fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array F sh a -> ForeignPtr a
toForeignPtr arr
        = UF.toForeignPtr $ unchecked arr
{-# INLINE toForeignPtr #-}


-- | O(1). Convert a `ByteString` to an foreign `Vector`.
fromByteString :: ByteString -> Vector F Word8
fromByteString bs
        = checked $ UF.fromByteString bs
{-# INLINE fromByteString #-}


-- | O(1). Convert a foreign 'Vector' to a `ByteString`.
toByteString :: Vector F Word8 -> ByteString
toByteString vec
        = UF.toByteString $ unchecked vec
{-# INLINE toByteString #-}

