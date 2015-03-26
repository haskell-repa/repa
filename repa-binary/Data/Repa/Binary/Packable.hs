
module Data.Repa.Binary.Packable
        ( Packable (..)
        , packToList
        , unpackFromList)
where
import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import System.IO.Unsafe
import Data.Repa.Binary.Format                  as F
import qualified Foreign.Storable               as S
import qualified Foreign.Marshal.Alloc          as S
import qualified Foreign.Ptr                    as S
import qualified Control.Monad.Primitive        as Prim


---------------------------------------------------------------------------------------------------
class Format   format 
   => Packable format where

 -- | Pack a value into a buffer using the given format.
 -- 
 --   If the format contains fixed width fields and the corresponding
 --   value has too many elements, then this function returns `False`, 
 --   otherwise `True`.
 --
 pack   :: S.Ptr Word8                  -- ^ Buffer.
        -> format                       -- ^ Format of buffer.
        -> Value format                 -- ^ Value to pack.
        -> (Int -> IO Bool)             -- ^ Accept the next offset.
        -> IO Bool

 -- | Unpack a value from a buffer using the given format.
 --
 --   This is the inverse of `pack` above.
 unpack :: S.Ptr Word8                  -- ^ Buffer.
        -> format                       -- ^ Format of buffer.
        -> ((Value format, Int) -> IO (Maybe a)) 
                                        -- ^ Accept the value and next offset. 
        -> IO (Maybe a)


------------------------------------------------------------------------------------------- Word8be
instance Packable Word8be where
 pack   buf Word8be x k
  = do  S.poke buf (fromIntegral x)
        k 1
 {-# INLINE pack #-}

 unpack buf Word8be k
  = do  x       <- S.peek buf
        k (fromIntegral x, 1)
 {-# INLINE unpack #-}


instance Packable Int8be where
 pack   buf  Int8be x k  = pack   buf Word8be (w8 x) k
 unpack buf  Int8be k    = unpack buf Word8be (\(x, o) -> k (i8 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

i8  :: Integral a => a -> Int8
i8 = fromIntegral
{-# INLINE i8  #-}


------------------------------------------------------------------------------------------ Word16be
instance Packable Word16be where
 pack   buf Word16be x k
  = do  S.poke  buf          (w8 ((w16 x .&. 0x0ff00) `shiftR` 8))
        S.pokeByteOff buf 1  (w8 ((w16 x .&. 0x000ff)))
        k 2
 {-# INLINE pack #-}

 unpack buf Word16be k
  = do  x0 :: Word8  <- S.peek        buf 
        x1 :: Word8  <- S.peekByteOff buf 1
        let !x  =  w16 ((w16 x0 `shiftL` 8) .|. w16 x1)
        k (x, 2)
 {-# INLINE unpack #-}


instance Packable Int16be where
 pack   buf  Int16be x k  = pack   buf Word16be (w16 x) k
 unpack buf  Int16be k    = unpack buf Word16be (\(x, o) -> k (i16 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


w16 :: Integral a => a -> Word16
w16 = fromIntegral
{-# INLINE w16 #-}

i16 :: Integral a => a -> Int16
i16 = fromIntegral
{-# INLINE i16 #-}


------------------------------------------------------------------------------------------ Word32be
instance Packable Word32be where
 pack   buf Word32be x k
  = do  S.poke        buf    (w8 ((w32 x .&. 0x0ff000000) `shiftR` 24))
        S.pokeByteOff buf 1  (w8 ((w32 x .&. 0x000ff0000) `shiftR` 16))
        S.pokeByteOff buf 2  (w8 ((w32 x .&. 0x00000ff00) `shiftR`  8))
        S.pokeByteOff buf 3  (w8 ((w32 x .&. 0x0000000ff)))
        k 4
 {-# INLINE pack #-}

 unpack buf Word32be k
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


instance Packable Int32be where
 pack   buf  Int32be x k  = pack   buf Word32be (w32 x) k
 unpack buf  Int32be k    = unpack buf Word32be (\(x, o) -> k (i32 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


w32 :: Integral a => a -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

i32 :: Integral a => a -> Int32
i32 = fromIntegral
{-# INLINE i32 #-}


------------------------------------------------------------------------------------------ Word64be
instance Packable Word64be where
 pack   buf Word64be x k
  = do  S.poke        buf    (w8 ((w64 x .&. 0x0ff00000000000000) `shiftR` 56))
        S.pokeByteOff buf 1  (w8 ((w64 x .&. 0x000ff000000000000) `shiftR` 48))
        S.pokeByteOff buf 2  (w8 ((w64 x .&. 0x00000ff0000000000) `shiftR` 40))
        S.pokeByteOff buf 3  (w8 ((w64 x .&. 0x0000000ff00000000) `shiftR` 32))
        S.pokeByteOff buf 4  (w8 ((w64 x .&. 0x000000000ff000000) `shiftR` 24))
        S.pokeByteOff buf 5  (w8 ((w64 x .&. 0x00000000000ff0000) `shiftR` 16))
        S.pokeByteOff buf 6  (w8 ((w64 x .&. 0x0000000000000ff00) `shiftR`  8))
        S.pokeByteOff buf 7  (w8 ((w64 x .&. 0x000000000000000ff)            ))
        k 8
 {-# INLINE pack #-}

 unpack buf Word64be k
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


instance Packable Int64be where
 pack   buf  Int64be x k  = pack   buf Word64be (w64 x) k
 unpack buf  Int64be k    = unpack buf Word64be (\(x, o) -> k (i64 x, o))
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


w64 :: Integral a => a -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

i64 :: Integral a => a -> Int64
i64 = fromIntegral
{-# INLINE i64 #-}


----------------------------------------------------------------------------------------- Float32be
instance Packable Float32be where
 pack      buf Float32be x k
  = pack   buf Word32be (floatToWord32 x) k
 {-# INLINE pack #-}

 unpack    buf Float32be k
  = unpack buf Word32be (\(v, i) -> k (word32ToFloat v, i))
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
instance Packable Float64be where
 pack      buf Float64be x k
  = pack   buf Word64be (doubleToWord64 x) k
 {-# INLINE pack #-}

 unpack    buf Float64be k
  = unpack buf Word64be (\(v, i) -> k (word64ToDouble v, i))
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


--------------------------------------------------------------------------------------------- (:*:)
instance (Packable fa, Packable fb) 
      =>  Packable (fa :*: fb) where

 pack   buf (fa :*: fb) (xa :*: xb) k
  =  pack buf                  fa xa $ \oa 
  -> pack (S.plusPtr buf oa)   fb xb $ \ob
  -> k (oa + ob)
 {-# INLINE pack #-}

 unpack buf (fa :*: fb) k
  =  unpack buf                fa    $ \(xa, oa)
  -> unpack (S.plusPtr buf oa) fb    $ \(xb, ob)
  -> k (xa :*: xb, oa + ob)
 {-# INLINE unpack #-}


----------------------------------------------------------------------------------------- FixString
instance Packable (FixString ASCII) where
 
  pack buf   (FixString ASCII lenField) xs k
   = do let !lenChars   = length xs
        let !lenPad     = lenField - lenChars

        if lenChars > lenField
         then return False
         else do
                mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x)) 
                        $ zip [0 .. lenChars - 1] xs

                mapM_ (\o      -> S.pokeByteOff buf (lenChars + o) (0 :: Word8))
                        $ [0 .. lenPad - 1]

                k lenField
  {-# NOINLINE pack #-}

  unpack buf (FixString ASCII lenField) k
   = do 
        let load_unpackChar o
                = do    x :: Word8 <- S.peekByteOff buf o
                        return $ chr $ fromIntegral x
            {-# INLINE load_unpackChar #-}

        xs      <- mapM load_unpackChar [0 .. lenField - 1]
        let (pre, _) = break (== '\0') xs
        k (pre, lenField)
  {-# NOINLINE unpack #-}


----------------------------------------------------------------------------------------- VarString
instance Packable (VarString ASCII) where

  pack buf   (VarString ASCII) xs k
   = do let !lenChars   = length xs

        mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x))
                $ zip [0 .. lenChars - 1] xs

        k lenChars
  {-# NOINLINE pack #-}

  unpack _   (VarString ASCII) _
   = return Nothing
  {-# NOINLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Pack a value into a list of `Word8`.
packToList 
        :: Packable format
        => format -> Value format -> Maybe [Word8]
packToList f x
 | Just size    <- packedSize f x
 = unsafePerformIO
 $ do   buf     <- S.mallocBytes size
        success <- pack buf f x (\_ -> return True)
        case success of
         False  -> return Nothing
         True   -> do
                xs      <- mapM (S.peekByteOff buf) [0..size - 1]
                S.free buf
                return $ Just xs

 | otherwise
 = Nothing


-- | Unpack a value from a list of `Word8`.
unpackFromList
        :: Packable format
        => format -> [Word8] -> Maybe (Value format)

unpackFromList f xs
 = unsafePerformIO
 $ do   let len = length xs
        buf     <- S.mallocBytes len
        mapM_ (\(o, x) -> S.pokeByteOff buf o x)
                $ zip [0 .. len - 1] xs
        unpack buf f $ \(v, _) -> return (Just v)

