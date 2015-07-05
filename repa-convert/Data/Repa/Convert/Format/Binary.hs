
-- | Atomic binary formats.
module Data.Repa.Convert.Format.Binary
        ( Format    (..)

        , Word8be   (..),       Int8be  (..)
        , Word16be  (..),       Int16be (..)
        , Word32be  (..),       Int32be (..)
        , Word64be  (..),       Int64be (..)

        , Float32be (..)
        , Float64be (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Bits                
import Data.Int                                 as V
import Data.Word                                as V
import qualified Foreign.Storable               as S
import qualified Foreign.Marshal.Alloc          as S
import qualified Foreign.Ptr                    as S
import qualified Control.Monad.Primitive        as Prim
import GHC.Exts
import Prelude hiding (fail)
#include "repa-convert.h"


------------------------------------------------------------------------------------------- Word8be
-- | Big-endian 8-bit unsigned word.
data Word8be     = Word8be              deriving (Eq, Show)
instance Format Word8be                 where
 type Value Word8be     = Word8
 fieldCount _           = 1
 minSize    _           = 1
 fixedSize  _           = Just 1
 packedSize _ _         = Just 1
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Word8be where
 packer   _ x dst _fails k
  = do  S.poke (Ptr dst) (fromIntegral x :: Word8)
        let !(Ptr dst') = S.plusPtr (Ptr dst) 1
        k dst'
 {-# INLINE pack #-}

 unpacker _ start _end _stop _fail eat
  = do  x <- S.peek (pw8 start)
        eat (plusAddr# start 1#) (fromIntegral x)
 {-# INLINE unpack #-}


w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}


------------------------------------------------------------------------------------------- Int8be
-- | Big-endian 8-bit signed integer.
data Int8be     = Int8be                deriving (Eq, Show)
instance Format Int8be                  where
 type Value Int8be      = V.Int8
 fieldCount _           = 1
 minSize    _           = 1
 fixedSize  _           = Just 1
 packedSize _ _         = Just 1
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Int8be where
 packer      Int8be x buf k
  = packer   Word8be (w8 x) buf k
 {-# INLINE packer   #-}

 unpacker    Int8be  start end stop fail eat    
  = unpacker Word8be start end stop fail 
  $ \addr v -> eat addr (i8 v)
 {-# INLINE unpacker #-}


i8  :: Integral a => a -> Int8
i8 = fromIntegral
{-# INLINE i8  #-}


------------------------------------------------------------------------------------------ Word16be
-- | Big-endian 32-bit unsigned word.
data Word16be    = Word16be             deriving (Eq, Show)
instance Format Word16be                where
 type Value Word16be    = V.Word16
 fieldCount _           = 1
 minSize    _           = 2
 fixedSize  _           = Just 2
 packedSize _ _         = Just 2
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Word16be where
 packer   Word16be x dst _fails k
  = do  S.poke        (Ptr dst)    (w8 ((w16 x .&. 0x0ff00) `shiftR` 8))
        S.pokeByteOff (Ptr dst) 1  (w8 ((w16 x .&. 0x000ff)))
        let !(Ptr dst') = S.plusPtr (Ptr dst) 2
        k dst'
 {-# INLINE packer #-}

 unpacker Word16be start _end _stop _fail eat
  = do  x0 :: Word8  <- S.peek        (pw8 start)
        x1 :: Word8  <- S.peekByteOff (pw8 start) 1
        eat (plusAddr# start 2#)
            (w16 ((w16 x0 `shiftL` 8) .|. w16 x1))
 {-# INLINE unpacker #-}


w16 :: Integral a => a -> Word16
w16 = fromIntegral
{-# INLINE w16 #-}


------------------------------------------------------------------------------------------- Int16be
--- | Big-endian 16-bit signed integer.
data Int16be    = Int16be               deriving (Eq, Show)
instance Format Int16be                 where
 type Value Int16be     = V.Int16
 fieldCount _           = 1
 minSize    _           = 2
 fixedSize  _           = Just 2
 packedSize _ _         = Just 2
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Int16be where
 packer      Int16be x buf k
  = packer   Word16be (w16 x) buf k
 {-# INLINE packer   #-}

 unpacker    Int16be  start end stop fail eat
  = unpacker Word16be start end stop fail
  $ \addr v -> eat addr (i16 v)
 {-# INLINE unpacker #-}


i16 :: Integral a => a -> Int16
i16 = fromIntegral
{-# INLINE i16 #-}


------------------------------------------------------------------------------------------ Word32be
-- | Big-endian 32-bit unsigned word.
data Word32be    = Word32be             deriving (Eq, Show)
instance Format Word32be                where
 type Value Word32be    = V.Word32
 fieldCount _           = 1
 minSize    _           = 4
 fixedSize  _           = Just 4
 packedSize _ _         = Just 4
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Word32be where
 packer Word32be x dst _fails k
  =  do S.poke        (Ptr dst)    (w8 ((w32 x .&. 0x0ff000000) `shiftR` 24))
        S.pokeByteOff (Ptr dst) 1  (w8 ((w32 x .&. 0x000ff0000) `shiftR` 16))
        S.pokeByteOff (Ptr dst) 2  (w8 ((w32 x .&. 0x00000ff00) `shiftR`  8))
        S.pokeByteOff (Ptr dst) 3  (w8 ((w32 x .&. 0x0000000ff)))
        let !(Ptr dst') = S.plusPtr (Ptr dst) 4
        k dst'
 {-# INLINE packer #-}

 unpacker Word32be start _end _fail _stop eat
  = do  x0 :: Word8  <- S.peek        (pw8 start) 
        x1 :: Word8  <- S.peekByteOff (pw8 start) 1
        x2 :: Word8  <- S.peekByteOff (pw8 start) 2
        x3 :: Word8  <- S.peekByteOff (pw8 start) 3
        eat (plusAddr# start 4#)
            (w32 (   (w32 x0 `shiftL` 24) 
                 .|. (w32 x1 `shiftL` 16)
                 .|. (w32 x2 `shiftL`  8)
                 .|. (w32 x3)))
 {-# INLINE unpack #-}


w32 :: Integral a => a -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}


------------------------------------------------------------------------------------------- Int32be
-- | Big-endian 32-bit signed integer.
data Int32be    = Int32be               deriving (Eq, Show)
instance Format Int32be                 where
 type Value Int32be     = V.Int32
 fieldCount _           = 1
 minSize    _           = 4
 fixedSize  _           = Just 4
 packedSize _ _         = Just 4
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Int32be where
 packer      Int32be x buf k
  = packer   Word32be (w32 x) buf k
 {-# INLINE packer #-}

 unpacker    Int32be  start end stop fail eat
  = unpacker Word32be start end stop fail
  $ \addr v -> eat addr (i32 v)
 {-# INLINE unpacker #-}


i32 :: Integral a => a -> Int32
i32 = fromIntegral
{-# INLINE i32 #-}


------------------------------------------------------------------------------------------ Word64be
-- | Big-endian 64-bit unsigned word.
data Word64be    = Word64be             deriving (Eq, Show)
instance Format Word64be                where
 type Value Word64be    = V.Word64
 fieldCount _           = 1
 minSize    _           = 8
 fixedSize  _           = Just 8
 packedSize _ _         = Just 8
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Word64be where
 packer Word64be x dst _fails k
  = do  S.poke        (Ptr dst)    (w8 ((w64 x .&. 0x0ff00000000000000) `shiftR` 56))
        S.pokeByteOff (Ptr dst) 1  (w8 ((w64 x .&. 0x000ff000000000000) `shiftR` 48))
        S.pokeByteOff (Ptr dst) 2  (w8 ((w64 x .&. 0x00000ff0000000000) `shiftR` 40))
        S.pokeByteOff (Ptr dst) 3  (w8 ((w64 x .&. 0x0000000ff00000000) `shiftR` 32))
        S.pokeByteOff (Ptr dst) 4  (w8 ((w64 x .&. 0x000000000ff000000) `shiftR` 24))
        S.pokeByteOff (Ptr dst) 5  (w8 ((w64 x .&. 0x00000000000ff0000) `shiftR` 16))
        S.pokeByteOff (Ptr dst) 6  (w8 ((w64 x .&. 0x0000000000000ff00) `shiftR`  8))
        S.pokeByteOff (Ptr dst) 7  (w8 ((w64 x .&. 0x000000000000000ff)            ))
        let !(Ptr dst') = S.plusPtr (Ptr dst) 8
        k dst'
 {-# INLINE packer #-}

 unpacker Word64be start _end _fail _stop eat
  = do  x0 :: Word8  <- S.peek        (pw8 start) 
        x1 :: Word8  <- S.peekByteOff (pw8 start) 1
        x2 :: Word8  <- S.peekByteOff (pw8 start) 2
        x3 :: Word8  <- S.peekByteOff (pw8 start) 3
        x4 :: Word8  <- S.peekByteOff (pw8 start) 4
        x5 :: Word8  <- S.peekByteOff (pw8 start) 5
        x6 :: Word8  <- S.peekByteOff (pw8 start) 6
        x7 :: Word8  <- S.peekByteOff (pw8 start) 7
        eat (plusAddr# start 8#)
            (w64 (   (w64 x0 `shiftL` 56) 
                 .|. (w64 x1 `shiftL` 48)
                 .|. (w64 x2 `shiftL` 40)
                 .|. (w64 x3 `shiftL` 32) 
                 .|. (w64 x4 `shiftL` 24) 
                 .|. (w64 x5 `shiftL` 16)
                 .|. (w64 x6 `shiftL`  8)
                 .|. (w64 x7           )))
 {-# INLINE unpacker #-}


w64 :: Integral a => a -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}


------------------------------------------------------------------------------------------- Int64be
-- | Big-endian 64-bit signed integer.
data Int64be    = Int64be               deriving (Eq, Show)
instance Format Int64be                 where
 type Value Int64be     = V.Int64
 fieldCount _           = 1
 minSize    _           = 8
 fixedSize  _           = Just 8
 packedSize _ _         = Just 8
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Int64be where
 packer      Int64be x buf k  
  = packer   Word64be (w64 x) buf k
 {-# INLINE packer   #-}

 unpacker    Int64be  start end stop fail eat
  = unpacker Word64be start end stop fail 
  $ \addr v -> eat addr (i64 v)
 {-# INLINE unpacker #-}


i64 :: Integral a => a -> Int64
i64 = fromIntegral
{-# INLINE i64 #-}


----------------------------------------------------------------------------------------- Float32be
-- | Big-endian 32-bit IEEE 754 float.
data Float32be  = Float32be             deriving (Eq, Show)
instance Format Float32be               where
 type Value Float32be   = Float
 fieldCount _           = 1
 minSize    _           = 4
 fixedSize  _           = Just 4
 packedSize _ _         = Just 4
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Float32be where
 packer      Float32be x buf k
  = packer   Word32be  (floatToWord32 x) buf k
 {-# INLINE packer #-}

 unpacker    Float32be start end stop fail eat
  = unpacker Word32be  start end stop fail
  $ \addr v -> eat addr (word32ToFloat v)
 {-# INLINE unpacker #-}


-- | Bitwise cast of `Float` to `Word32`.
--
--   The resulting `Word32` contains the binary representation of the
--   `Float`, rather than the integral part of its value.
--
floatToWord32 :: Float -> Word32
floatToWord32 d
 = Prim.unsafeInlineIO
 $ S.alloca $ \buf -> 
 do     S.poke (S.castPtr buf) d
        S.peek buf
{-# INLINE floatToWord32 #-}


-- | Inverse of `doubleToFloat32`
word32ToFloat :: Word32 -> Float
word32ToFloat w
 = Prim.unsafeInlineIO
 $ S.alloca $ \buf ->
 do     S.poke (S.castPtr buf) w
        S.peek buf
{-# INLINE word32ToFloat #-}


----------------------------------------------------------------------------------------- Float64be
-- | Big-endian 64-bit IEEE 754 float.
data Float64be  = Float64be             deriving (Eq, Show)
instance Format Float64be               where
 type Value Float64be   = Double
 fieldCount _           = 1
 minSize    _           = 8
 fixedSize  _           = Just 8
 packedSize _ _         = Just 8
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable Float64be where
 packer      Float64be x start fails eat
  = packer   Word64be (doubleToWord64 x) start fails eat
 {-# INLINE packer #-}

 unpacker    Float64be start end stop fail eat
  = unpacker Word64be  start end stop fail
  $ \addr v -> eat addr (word64ToDouble v)
 {-# INLINE unpacker #-}


-- | Bitwise cast of `Double` to `Word64`.
--
--   The resulting `Word64` contains the binary representation of the
--   `Double`, rather than the integral part of its value.
--
doubleToWord64 :: Double -> Word64
doubleToWord64 d
 = Prim.unsafeInlineIO
 $ S.alloca $ \buf -> 
 do     S.poke (S.castPtr buf) d
        S.peek buf
{-# INLINE doubleToWord64 #-}


-- | Inverse of `doubleToWord64`
word64ToDouble :: Word64 -> Double
word64ToDouble w
 = Prim.unsafeInlineIO
 $ S.alloca $ \buf ->
 do     S.poke (S.castPtr buf) w
        S.peek buf
{-# INLINE word64ToDouble #-}


---------------------------------------------------------------------------------------------------
pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}

