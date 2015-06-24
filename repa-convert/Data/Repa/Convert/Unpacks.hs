{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Convert.Unpacks where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Format.Sep
import Data.Repa.Convert.Format.Lists
import Data.Repa.Scalar.Product
import Data.Word
import GHC.Exts


---------------------------------------------------------------------------------------------------
class Unpacks format where
 unpacks
        :: format
        -> Addr#                                -- ^ Start of buffer.
        -> Addr#                                -- ^ Pointer to first byte after end of buffer.
        -> (Word8 -> Bool)                      -- ^ Detect a field terminator.
        -> IO ()                                -- ^ Signal failure.
        -> (Addr# -> Value format -> IO ())     -- ^ Eat an unpacked value
        -> IO ()


instance ( Unpacks (Sep fs)
         , Value   (Sep fs) ~ Value fs)
        => Unpacks (Sep (VarAsc :*: fs)) where

 unpacks (SepCons _sm _f1 sfs) start end stop fails eat
  = do  (Ptr next, str)  <- unpackAsc (pw8 start) (pw8 end) stop
        unpacks sfs next end stop fails (\addr x -> eat addr (str :*: x))
 {-# INLINE unpacks #-}




---------------------------------------------------------------------------------------------------
pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
