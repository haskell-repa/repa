
module Data.Array.Repa.Repr.ByteString
        ( B, Array (..)
        , fromByteString, toByteString)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Data.Word
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as BU
import Data.ByteString                  (ByteString)


-- | Strict ByteStrings arrays are represented as ForeignPtr buffers of Word8
data B
data instance Array B sh Word8
        = AByteString sh !ByteString
        
deriving instance Show sh
        => Show (Array B sh Word8)

-- Repr -----------------------------------------------------------------------
-- | Use elements from a bytestring
instance Repr B Word8 where
 {-# INLINE linearIndex #-}
 linearIndex (AByteString _ bs) ix
        = bs `B.index` ix

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex (AByteString _ bs) ix
        = bs `BU.unsafeIndex` ix

 {-# INLINE extent #-}
 extent (AByteString sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AByteString sh bs) x 
  = sh `deepSeq` bs `seq` x


-- Load -----------------------------------------------------------------------
-- | no-op.
instance Shape sh => Load B B sh Word8 where
 {-# INLINE load #-}
 load arr = arr

-- | O(n) construction of a `ByteString`
instance Shape sh => Load L B sh Word8 where
 {-# INLINE load #-}
 load (AList sh xx)
        = AByteString sh $ B.pack xx

-- | O(n) construction of list.
instance Shape sh => Load B L sh Word8 where
 {-# INLINE load #-}
 load (AByteString sh bs)
        = AList sh $ B.unpack bs


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ByteString` as an array.
fromByteString
        :: Shape sh
        => sh -> ByteString -> Array B sh Word8
{-# INLINE fromByteString #-}
fromByteString sh bs
        = AByteString sh bs


-- | Convert an array to a `ByteString`
-- 
--   This is O(1) if the source is already represented as ByteString (has representation `B`).
--
toByteString
        :: Load  r1 B sh Word8
        => Array r1 sh Word8 -> ByteString
{-# INLINE toByteString #-}
toByteString arr
 = case load arr of
        AByteString _ bs  -> bs
