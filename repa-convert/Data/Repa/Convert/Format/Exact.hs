
module Data.Repa.Convert.Format.Exact
        (ExactString (..))
where
import Data.Repa.Convert.Format.Base
import Data.Word
import Data.Char
import qualified Foreign.Storable       as S
import qualified Foreign.Ptr            as S
#include "repa-convert.h"


data ExactString 
        = ExactString String
        deriving Show


instance Format ExactString where
 type Value ExactString          = ()
 fieldCount (ExactString _)      = 0
 minSize    (ExactString str)    = length str
 fixedSize  (ExactString str)    = return (length str)
 packedSize (ExactString str) () = return (length str)
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable ExactString where
 pack (ExactString str) _       
  =  Packer $ \buf k
  -> do let !len = length str
        mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x))
                $ zip [0 .. len - 1] str
        k (S.plusPtr buf len)
 {-# NOINLINE pack #-}

 unpack (ExactString str)
  =  Unpacker $ \start end _stop fails eat
  -> do
        let !len    = length str
        let !lenBuf = S.minusPtr end start
        if  lenBuf < len
         then fails
         else do
                let load_unpackChar o
                      = do x :: Word8 <- S.peekByteOff start o
                           return $ chr $ fromIntegral x
                    {-# INLINE load_unpackChar #-}

                xs      <- mapM load_unpackChar [0 .. len - 1]
                if (xs == str)
                 then eat (S.plusPtr start len) ()
                 else fails
 {-# NOINLINE unpack #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE_INNER w8  #-}

