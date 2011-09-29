
module Data.Array.Repa.Repr.ByteString
        ( B, Array (..)
        , fromByteString, toByteString)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed
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


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ByteString` as an array.
fromByteString
        :: Shape sh
        => sh -> ByteString -> Array B sh Word8
{-# INLINE fromByteString #-}
fromByteString sh bs
        = AByteString sh bs


-- | O(1). Unpack a `ByteString` from an array.
toByteString :: Array B sh Word8 -> ByteString
{-# INLINE toByteString #-}
toByteString (AByteString _ bs) = bs
