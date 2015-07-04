
module Data.Repa.Convert.Format.Text
        (VarText (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Text                                (Text)
import Data.Word
import GHC.Exts

import qualified Data.Text.Foreign              as T
import qualified Data.Text                      as T
import qualified Foreign.Storable               as F
import qualified Foreign.Ptr                    as F


---------------------------------------------------------------------------------------------------
-- | Variable length unicode text.
-- 
-- * When serialised, the \"string\" is not escaped, not does it have
--   surrounding quotes. The UTF8 data is just copied into the result verbatim.
--
data VarText    = VarText       deriving (Eq, Show)
instance Format VarText         where
 type Value VarText             = Text

 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 0
 {-# INLINE minSize #-}

 fixedSize  VarText             = Nothing
 {-# INLINE fixedSize #-}

 packedSize VarText xs          = Just $ T.length xs
 {-# INLINE packedSize #-}


instance Packable VarText where

 packer   VarText tt buf k
  = T.withCStringLen tt
  $ \(ptr, len)
  -> let        
        packer_VarText !ix
         | ix >= len
         = k (F.plusPtr buf ix)

         | otherwise
         = do   !(x :: Word8)   <- F.peekByteOff ptr ix
                F.pokeByteOff buf ix x
                packer_VarText (ix + 1)
        {-# INLINE packer_VarText #-}

     in packer_VarText 0
 {-# INLINE packer #-}

 unpacker VarText start end stop _fail eat
  = scanLen 0
  where
        -- Length of the input buffer.
        !lenBuf = F.minusPtr (pw8 end) (pw8 start)

        -- Scan through the input to see how long the field is.
        scanLen !ix
         | ix >= lenBuf
         = copyField lenBuf

         | otherwise
         = do   x       <- F.peekByteOff (pw8 start) ix
                if stop x
                 then copyField ix
                 else scanLen (ix + 1)
        {-# INLINE scanLen #-}

        -- Decode the UTF-8 bytes into a new buffer.
        copyField !lenField
         = do   tt      <- T.peekCStringLen (Ptr start, lenField)
                let !(Ptr start') = F.plusPtr (Ptr start) lenField
                eat start' tt
        {-# INLINE copyField #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
