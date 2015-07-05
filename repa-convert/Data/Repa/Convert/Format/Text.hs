
-- | Conversions for "Data.Text" things.
module Data.Repa.Convert.Format.Text
        ( VarText       (..)
        , VarTextString (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Format.String
import Data.Text                                (Text)
import Data.Word
import GHC.Exts
import qualified Data.Text.Foreign              as T
import qualified Data.Text                      as T
import qualified Foreign.Storable               as F
import qualified Foreign.Ptr                    as F


---------------------------------------------------------------------------------------------------
-- | Variable length unicode text, represented as a "Data.Text" thing.
--
data VarText                    = VarText       deriving (Eq, Show)
instance Format VarText         where
 type Value VarText             = Text
 fieldCount _                   = 1
 minSize    _                   = 0
 fixedSize  VarText             = Nothing
 packedSize VarText xs          = Just $ T.length xs
 {-# INLINE fieldCount #-}
 {-# INLINE minSize #-}
 {-# INLINE fixedSize #-}
 {-# INLINE packedSize #-}


instance Packable VarText where

 packer   VarText tt dst _fails eat
  = T.withCStringLen tt
  $ \(ptr, len)
  -> let        
        packer_VarText !ix
         | ix >= len
         = let  !(Ptr dst') = F.plusPtr (Ptr dst) ix
           in   eat dst'

         | otherwise
         = do   !(x :: Word8)   <- F.peekByteOff ptr ix
                F.pokeByteOff (Ptr dst) ix x
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
 {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------------------
-- | Variable length string in double quotes,
--   and standard backslash encoding of non-printable characters.
data VarTextString              = VarTextString deriving (Eq, Show)
instance Format VarTextString   where
 type Value VarTextString       = Text
 fieldCount _                   = 1
 minSize    _                   = 2
 fixedSize  VarTextString       = Nothing
 packedSize VarTextString xs    = Just $ T.length xs
 {-# INLINE fieldCount #-}
 {-# INLINE minSize #-}
 {-# INLINE fixedSize #-}
 {-# INLINE packedSize #-}


instance Packable VarTextString where
 
 -- TODO: don't go via lists.
 packer VarTextString tt buf k
  = packer VarText (T.pack $ show $ T.unpack tt) buf k
 {-# INLINE packer #-}

 -- TODO: don't go via lists.
 unpacker VarTextString start end stop _fail eat
  = unpacker VarCharString start end stop _fail 
  $ \start' val -> eat start' (T.pack val)
 {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------------------
pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}

