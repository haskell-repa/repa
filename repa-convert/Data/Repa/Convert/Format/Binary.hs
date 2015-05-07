
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
import Data.Repa.Convert.Format.Base
import Data.Bits                
import Data.Int                                 as V
import Data.Word                                as V
import qualified Foreign.Storable               as S
import qualified Foreign.Marshal.Alloc          as S
import qualified Foreign.Ptr                    as S
import qualified Control.Monad.Primitive        as Prim


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
 pack   Word8be x = Packer $ \buf k
  -> do S.poke buf (fromIntegral x)
        k (S.plusPtr buf 1)
 {-# INLINE pack #-}

 unpack buf _ Word8be k
  = do  x       <- S.peek buf
        k (fromIntegral x, 1)
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
 pack    Int8be x               = pack   Word8be (w8 x)
 unpack  buf len Int8be k       = unpack buf len Word8be (\(x, o) -> k (i8 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


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
 pack   Word16be x = Packer $ \buf k
  -> do S.poke        buf    (w8 ((w16 x .&. 0x0ff00) `shiftR` 8))
        S.pokeByteOff buf 1  (w8 ((w16 x .&. 0x000ff)))
        k (S.plusPtr buf 2)
 {-# INLINE pack #-}

 unpack buf _ Word16be k
  = do  x0 :: Word8  <- S.peek        buf 
        x1 :: Word8  <- S.peekByteOff buf 1
        let !x  =  w16 ((w16 x0 `shiftL` 8) .|. w16 x1)
        k (x, 2)
 {-# INLINE unpack #-}


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
 pack   Int16be x          = pack   Word16be (w16 x)
 unpack buf len Int16be k  = unpack buf len Word16be (\(x, o) -> k (i16 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


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
 pack   Word32be x = Packer $ \buf k
  -> do S.poke        buf    (w8 ((w32 x .&. 0x0ff000000) `shiftR` 24))
        S.pokeByteOff buf 1  (w8 ((w32 x .&. 0x000ff0000) `shiftR` 16))
        S.pokeByteOff buf 2  (w8 ((w32 x .&. 0x00000ff00) `shiftR`  8))
        S.pokeByteOff buf 3  (w8 ((w32 x .&. 0x0000000ff)))
        k (S.plusPtr buf 4)
 {-# INLINE pack #-}

 unpack buf _ Word32be k
  = do  x0 :: Word8  <- S.peek        buf 
        x1 :: Word8  <- S.peekByteOff buf 1
        x2 :: Word8  <- S.peekByteOff buf 2
        x3 :: Word8  <- S.peekByteOff buf 3
        let !x  =  w32 (    (w32 x0 `shiftL` 24) 
                        .|. (w32 x1 `shiftL` 16)
                        .|. (w32 x2 `shiftL`  8)
                        .|. (w32 x3))
        k (x, 4)
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
 pack   Int32be x            = pack   Word32be (w32 x)
 unpack buf len Int32be k    = unpack buf len Word32be (\(x, o) -> k (i32 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


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
 pack   Word64be x = Packer $ \buf k
  -> do S.poke        buf    (w8 ((w64 x .&. 0x0ff00000000000000) `shiftR` 56))
        S.pokeByteOff buf 1  (w8 ((w64 x .&. 0x000ff000000000000) `shiftR` 48))
        S.pokeByteOff buf 2  (w8 ((w64 x .&. 0x00000ff0000000000) `shiftR` 40))
        S.pokeByteOff buf 3  (w8 ((w64 x .&. 0x0000000ff00000000) `shiftR` 32))
        S.pokeByteOff buf 4  (w8 ((w64 x .&. 0x000000000ff000000) `shiftR` 24))
        S.pokeByteOff buf 5  (w8 ((w64 x .&. 0x00000000000ff0000) `shiftR` 16))
        S.pokeByteOff buf 6  (w8 ((w64 x .&. 0x0000000000000ff00) `shiftR`  8))
        S.pokeByteOff buf 7  (w8 ((w64 x .&. 0x000000000000000ff)            ))
        k (S.plusPtr buf 8)
 {-# INLINE pack #-}

 unpack buf _ Word64be k
  = do  x0 :: Word8  <- S.peek        buf 
        x1 :: Word8  <- S.peekByteOff buf 1
        x2 :: Word8  <- S.peekByteOff buf 2
        x3 :: Word8  <- S.peekByteOff buf 3
        x4 :: Word8  <- S.peekByteOff buf 4
        x5 :: Word8  <- S.peekByteOff buf 5
        x6 :: Word8  <- S.peekByteOff buf 6
        x7 :: Word8  <- S.peekByteOff buf 7
        let !x  =  w64 (    (w64 x0 `shiftL` 56) 
                        .|. (w64 x1 `shiftL` 48)
                        .|. (w64 x2 `shiftL` 40)
                        .|. (w64 x3 `shiftL` 32) 
                        .|. (w64 x4 `shiftL` 24) 
                        .|. (w64 x5 `shiftL` 16)
                        .|. (w64 x6 `shiftL`  8)
                        .|. (w64 x7           ))
        k (x, 8)
 {-# INLINE unpack #-}


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
 pack   Int64be x            = pack   Word64be (w64 x)
 unpack buf len Int64be k    = unpack buf len Word64be (\(x, o) -> k (i64 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


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
 pack      Float32be x           = pack Word32be (floatToWord32 x)
 {-# INLINE pack #-}

 unpack    buf len Float32be k
  = unpack buf len Word32be (\(v, i) -> k (word32ToFloat v, i))
 {-# INLINE unpack #-}


-- | Bitwise cast of `Float` to `Word32`.
--
--   The resulting `Word32` contains the representation of the `Float`, 
--   rather than it's value.
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
 pack      Float64be x          = pack   Word64be (doubleToWord64 x)
 {-# INLINE pack #-}

 unpack    buf len Float64be k
  = unpack buf len Word64be (\(v, i) -> k (word64ToDouble v, i))
 {-# INLINE unpack #-}


-- | Bitwise cast of `Double` to `Word64`.
--
--   The resulting `Word64` contains the representation of the `Double`, 
--   rather than it's value.
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


