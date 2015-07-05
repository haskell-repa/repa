
-- | Conversions for "Data.ByteString" things.
module Data.Repa.Convert.Format.Bytes
        (VarBytes (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Word
import GHC.Exts
import Prelude                                  hiding (fail)
import Data.ByteString                          (ByteString)
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Internal       as BS
import qualified Foreign.Marshal.Alloc          as F
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Storable               as F
import qualified Foreign.Ptr                    as F


-- | Variable length sequence of bytes, represented as a `Data.ByteString`.
--
data VarBytes                   = VarBytes      deriving (Eq, Show)
instance Format VarBytes        where
 type Value VarBytes            = ByteString
 fieldCount _                   = 1
 minSize    _                   = 0
 fixedSize  VarBytes            = Nothing
 packedSize VarBytes bs         = Just $ BS.length bs
 {-# INLINE fieldCount #-}
 {-# INLINE minSize #-}
 {-# INLINE fixedSize #-}
 {-# INLINE packedSize #-}


instance Packable VarBytes where

 packer VarBytes (BS.PS fptr start len) dst _fails k
  = F.withForeignPtr fptr 
  $ \ptr_
  -> let        
        -- Pointer to active bytes.
        !ptr = F.plusPtr ptr_ start

        -- Copy bytes from the bytestring to the destination buffer.
        packer_VarBytes !ix
         | ix >= len
         = let  !(Ptr dst') = F.plusPtr (Ptr dst) ix
            in  k dst'

         | otherwise
         = do   !(x :: Word8) <- F.peekByteOff ptr ix
                F.pokeByteOff (Ptr dst) ix x
                packer_VarBytes (ix + 1)
        {-# INLINE packer_VarBytes #-}

     in packer_VarBytes 0


 unpacker VarBytes start end stop _fail eat
  = checkLen 0
  where
        -- Length of the input buffer.
        !lenBuf = F.minusPtr (pw8 end) (pw8 start)

        -- Scan through the input to see how long the result will be.
        checkLen !ix
         | ix >= lenBuf
         = copy lenBuf

         | otherwise
         = do   !(x :: Word8) <- F.peekByteOff (pw8 start) ix
                if stop x
                 then copy      ix
                 else checkLen (ix + 1)
        {-# INLINE checkLen #-}

        -- Copy the desired bytes into a new buffer.
        copy !len
         =  F.mallocBytes len >>= \ptr
         -> let 
                unpacker_VarBytes !ix
                 | ix >= len
                 = do   fptr       <- F.newForeignPtr F.finalizerFree ptr
                        let bs  =  BS.PS fptr 0 len
                        let !(Ptr start') = F.plusPtr (pw8 start) len
                        eat start' bs

                 | otherwise
                 = do   x :: Word8 <- F.peekByteOff (pw8 start) ix
                        F.pokeByteOff ptr ix x
                        unpacker_VarBytes (ix + 1)
            in  unpacker_VarBytes 0
        {-# INLINE copy #-}
 {-# INLINE unpacker #-}                


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
