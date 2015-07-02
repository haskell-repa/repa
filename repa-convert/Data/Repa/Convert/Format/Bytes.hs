
module Data.Repa.Convert.Format.Bytes
        ( -- * Flat byte strings
          VarBytes (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Vector.Unboxed                      (Vector)
import qualified Foreign.Storable               as S
import qualified Foreign.Ptr                    as S
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Data.Word
import GHC.Exts

import Prelude hiding (fail)
#include "repa-convert.h"


---------------------------------------------------------------------------------------------------
-- | Variable length byte sequence.
--
-- * When serialised, the \"string\" is not escaped, nor does it have
--   surrounding quotes. The bytes are just copied into the result verbatim.
--   
data VarBytes   = VarBytes      deriving (Eq, Show)
instance Format VarBytes        where
 type Value VarBytes            = Vector Word8

 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 0
 {-# INLINE minSize #-}

 fixedSize  VarBytes            = Nothing
 {-# INLINE fixedSize #-}

 packedSize VarBytes xs         = Just $ U.length xs
 {-# NOINLINE packedSize #-}


instance Packable VarBytes where

 packer VarBytes bs buf k
  = go 0
  where
        !len    = U.length bs

        go !ix
         | ix >= len    = k (S.plusPtr buf ix)

         | otherwise
         = do   let x   = U.unsafeIndex bs ix
                S.pokeByteOff buf ix x
                go (ix + 1)
        {-# INLINE go #-}
 {-# INLINE packer #-}


 unpacker VarBytes start end stop _fail eat
  = checkLen 0
  where
        -- Length of the input buffer.
        !lenBuf = S.minusPtr (pw8 end) (pw8 start)

        -- Scan through the input to see how long the result will be.
        checkLen !ix
         | ix >= lenBuf
         = copy lenBuf

         | otherwise
         = do   x  <- S.peekByteOff (pw8 start) ix
                if stop x
                 then copy      ix
                 else checkLen (ix + 1)
        {-# INLINE checkLen #-}

        copy len
         = do   mvec    <- UM.unsafeNew len

                let go !ix
                        | ix >= len
                        = do    vec     <- U.unsafeFreeze mvec
                                let !(Ptr start') = S.plusPtr (pw8 start) len
                                eat start' vec

                        | otherwise
                        = do    x       <- S.peekByteOff (pw8 start) ix
                                UM.unsafeWrite mvec ix x
                                go (ix + 1)
                go 0
        {-# NOINLINE copy #-}
 {-# INLINE unpacker #-}                


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
