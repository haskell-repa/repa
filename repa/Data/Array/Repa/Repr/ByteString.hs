
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
        = AByteString !sh !ByteString
        
deriving instance Show sh
        => Show (Array B sh Word8)

deriving instance Read sh
        => Read (Array B sh Word8)


-- Repr -----------------------------------------------------------------------
-- | Read elements from a `ByteString`.
instance Repr B Word8 where
 linearIndex (AByteString _ bs) ix
        = bs `B.index` ix
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (AByteString _ bs) ix
        = bs `BU.unsafeIndex` ix
 {-# INLINE unsafeLinearIndex #-}

 extent (AByteString sh _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (AByteString sh bs) x 
  = sh `deepSeq` bs `seq` x
 {-# INLINE deepSeqArray #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ByteString` as an array.
fromByteString
        :: Shape sh
        => sh -> ByteString -> Array B sh Word8
fromByteString sh bs
        = AByteString sh bs
{-# INLINE fromByteString #-}


-- | O(1). Unpack a `ByteString` from an array.
toByteString :: Array B sh Word8 -> ByteString
toByteString (AByteString _ bs) = bs
{-# INLINE toByteString #-}
