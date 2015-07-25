
-- | Loading and storing integers directly from/to memory buffers.
module Data.Repa.Scalar.Int
        ( -- * Reading from strings
          readInt
        , readIntFromByteString

          -- * Loading from buffers
        , loadInt
        , loadInt#
        , loadIntWith#

          -- * Showing to strings
          -- ** Unpadded
        , showInt
        , showIntToByteString

          -- ** Padded
        , showIntPad
        , showIntPadToByteString

          -- * Storing to buffers
          -- ** Unpadded
        , storeInt
        , storeInt#
        , storeIntWith#

          -- ** Padded
        , storeIntPad
        , storeIntPad#
        , storeIntPadWith#)
where
import Data.Word
import Data.Char
import GHC.Exts
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Internal               as BS
import qualified Foreign.Ptr                            as F
import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Storable                       as F
import qualified Foreign.Marshal.Alloc                  as F
import System.IO.Unsafe                       


-- Read/Load --------------------------------------------------------------------------------------
-- | Read an `Int` from a `String`, or `Nothing` if this isn't one.
readInt :: String -> Maybe Int
readInt str
        = readIntFromByteString $ BS.pack str
{-# INLINE readInt #-}


-- | Read an `Int` from a `ByteString`, or `Nothing` if this isn't one.
readIntFromByteString 
        :: BS.ByteString -> Maybe Int

readIntFromByteString (BS.PS fptr offset len)
 --   accursed ... may increase sharing of result,
 --   but this is ok here as we're not allocating mutable object.
 = unsafePerformIO      
 $ F.withForeignPtr fptr
 $ \ptr -> return 
        $  loadInt (F.plusPtr ptr offset) len
                Nothing
                (\val n -> if n == len
                                then Just val
                                else Nothing)

{-# INLINE readIntFromByteString #-}


-- | Load an ASCII `Int` from a foreign buffer,
--   returning the value and number of characters read.
--
--   * The code 
loadInt :: Ptr Word8                    -- ^ Buffer holding digits.
        -> Int                          -- ^ Length of buffer.
        -> b                            -- ^ On convert failure, return this value.
        -> (Int -> Int -> b)            -- ^ On convert success, given int read and number of chars.
        -> b
 
loadInt (Ptr addr) (I# len) fails eat
 = case loadInt# addr len of
        (# 0#, _, _  #) -> fails
        (# _,  n, ix #) -> eat (I# n) (I# ix)
{-# INLINE loadInt #-}


-- | Load an ASCII `Int` from a buffer,
--   producing an unboxed tuple describing the result.
--
--   * This function is set to `NOINLINE`, so it can be safely called from 
--     multiple places in the program.
--
loadInt#
        :: Addr#                        -- ^ Address of buffer holding digits
        -> Int#                         -- ^ Length of buffer.
        -> (# Int#, Int#, Int# #)       -- ^ Convert success?, value, length read.

loadInt# addr len
 = let  
        buf :: Ptr Word8 
         = Ptr addr

        peek8 ix
           -- accursed .. may increase sharing of the result value, 
           -- but this isn't a problem here because the result is not
           -- mutable, and will be unboxed by the simplifier anyway.
         = case BS.accursedUnutterablePerformIO (F.peekByteOff buf (I# ix)) of
                (w8 :: Word8) -> case fromIntegral w8 of
                                        I# i    -> i
        {-# INLINE peek8 #-}

   in  loadIntWith# len peek8
{-# NOINLINE loadInt# #-}


-- | Primitive `Int` loading function.
--
--   * This function is set to `INLINE`, so you will get a new copy of it in the
--     compiled program each time it is invoked. Consider providing an appropriate
--     wrapper for your use case.
--
loadIntWith# 
        :: Int#                         -- ^ Length of input buffer.
        -> (Int# -> Int#)               -- ^ Function to get a byte from the source.
        -> (# Int#, Int#, Int# #)       -- ^ Convert success?, value, length read

loadIntWith# !len get
 = start 0#
 where
        start !ix
         | 1# <- ix >=# len = (# 0#, 0#, 0# #)
         | otherwise        = sign ix
        {-# INLINE start #-}

        -- Check for explicit sign character,
        -- and encode what it was as an integer.
        sign !ix
         | !s   <- get 0#
         = case chr $ fromIntegral (I# s) of
                '-'     -> loop 1# (ix +# 1#) 0#
                '+'     -> loop 2# (ix +# 1#) 0#
                _       -> loop 0#  ix        0#
        {-# INLINE sign #-}

        loop !neg !ix !n 
         -- We've hit the end of the array.
         | 1# <- ix >=# len   
         = end neg ix n

         | otherwise
         = case get ix of
               -- Current character is a digit, so add it to the accmulator.
             w | 1# <- w >=# 0x30# 
               , 1# <- w <=# 0x039#
               -> loop neg ( ix +# 1#) 
                           ((n  *# 10#) +# (w -# 0x30#))

               -- Current character is not a digit.
               | otherwise
               -> end neg ix n

        end !neg !ix !n
         -- We didn't find any digits, and there was no explicit sign.
         | 1# <- ix  ==# 0#
         , 1# <- neg ==# 0#
         = (# 0#, 0#, 0# #)

         -- We didn't find any digits, but there was an explicit sign.
         | 1# <- ix  ==# 1#
         , 1# <- neg /=# 0#
         = (# 0#, 0#, 0# #)

         -- Number was explicitly negated.
         | 1# <- neg ==# 1#                    
         , I# n' <- negate (I# n)
         = (# 1#, n', ix #)

         -- Number was not negated.
         | otherwise
         = (# 1#, n, ix #)
        {-# NOINLINE end #-}
{-# INLINE loadIntWith# #-}


---------------------------------------------------------------------------------------------------
-- | Show an `Int`, allocating a new `String`.
showInt :: Int -> String
showInt i
 = BS.unpack $ showIntToByteString i
{-# INLINE showInt #-}


-- | Show an `Int`, allocating a new `ByteString`.
showIntToByteString :: Int -> BS.ByteString
showIntToByteString (I# i)
 = unsafePerformIO
 $ let  
        alloc len
         = F.mallocBytes (I# len)
        {-# INLINE alloc #-}

        write  ptr ix val
         = F.pokeByteOff ptr (I# ix) (fromIntegral (I# val) :: Word8)
        {-# INLINE write #-}

        make   ptr len
         = do   fptr    <- F.newForeignPtr F.finalizerFree ptr
                return  $  BS.PS fptr 0 (I# len)
        {-# INLINE make #-}

   in   storeIntWith# alloc write i make
{-# NOINLINE showIntToByteString #-}


-- | Store an ASCII `Int` into a buffer, producing the number of bytes written.
storeInt :: Ptr Word8                   -- ^ Pointer to output buffer.
         -> Int                         -- ^ Int to store.
         -> IO Int                      -- ^ Number of bytes written.

storeInt (Ptr addr) (I# val)
 = storeInt# addr val
{-# INLINE storeInt #-}


-- | Store an ASCII `Int` into a buffer, producing the number of bytes written.
--
--   * This function is set to NOINLINE, so it can be safely called from
--     multiple places in the program.
--
storeInt# 
        :: Addr#                        -- ^ Address of output buffer.
        -> Int#                         -- ^ Int to store.
        -> IO Int                       -- ^ Number of bytes written.

storeInt# addr val
 = let
        -- move along, nothing to see..
        alloc _ 
         = return $ Ptr addr
        {-# INLINE alloc #-}

        write _ ix byte
         = F.pokeByteOff (Ptr addr) (I# ix) (fromIntegral (I# byte) :: Word8)
        {-# INLINE write #-}

        make _ len
         = return $ I# len
        {-# INLINE make #-}

  in do
        storeIntWith# alloc write val make
{-# NOINLINE storeInt# #-}


-- | Primitive `Int` storing function.
-- 
--   * This function is set to `INLINE`, so you will get a new copy of it in the compiled
--     program each time it is invoked. Consider providing an appropriate wrapper
--     for your use case.
--
storeIntWith#
        :: (Int# -> IO buf)             -- ^ Function to allocate a new output buffer,
                                        --   given the length in bytes.
        -> (buf -> Int# -> Int# -> IO ())      
                                        -- ^ Function to write a byte to the buffer,
                                        --   given the index and byte value.
        -> Int#                         -- ^ Int to store.
        -> (buf -> Int# -> IO b)        -- ^ Continuation for buffer and bytes written.
        -> IO b

storeIntWith# alloc write val k
 =  F.allocaBytes 32 $ \(buf :: Ptr Word8)
 -> let 
        -- Get starting magnitude.
        !start
         | 1#   <- val <# 0#    = digits (0# -# val) 0#
         | otherwise            = digits val         0#
        {-# INLINE start #-}

        -- Load digits into buffer.
        digits !mag !ix
         = do   F.pokeByteOff buf (I# ix) 
                        (fromIntegral (I# (0x030# +# mag `remInt#` 10#)) :: Word8)
                let  !ix'  = ix +# 1#
                let  !mag' = mag `quotInt#` 10#
                (case mag' ==# 0# of
                  1#    -> sign   ix'
                  _     -> digits mag' ix')
        {-# NOINLINE digits #-}

        -- Load sign into buffer.
        sign !ix
         = case val <# 0# of
            1# -> do F.pokeByteOff buf (I# ix) 
                        (fromIntegral (I# 0x02d#) :: Word8)
                     create (ix +# 1#)
            _  ->    create ix
        {-# INLINE sign #-}

        -- Create a new output buffer, now that we know the length.
        create len 
         = do   out     <- alloc len
                output len out 0#
        {-# NOINLINE create #-}

        -- Read chars back from buffer to output them
        -- in the correct order.
        output len out ix0
         = go ix0
         where  go ix
                 | 1# <- ix <# len
                 = do   x :: Word8  <- F.peekByteOff buf (I# ((len -# 1#) -# ix))
                        let !(I# i) = fromIntegral x
                        write out ix i
                        go (ix +# 1#)

                 | otherwise
                 = k out len
        {-# INLINE output #-}
    in start
{-# INLINE storeIntWith# #-}


---------------------------------------------------------------------------------------------------
-- | Show an `Int`, allocating a new `String`.
showIntPad :: Int -> Int -> String
showIntPad i pad
 = BS.unpack $ showIntPadToByteString i pad
{-# INLINE showIntPad #-}


-- | Show an `Int`, allocating a new `ByteString`.
showIntPadToByteString :: Int -> Int -> BS.ByteString
showIntPadToByteString (I# i) pad'
 = unsafePerformIO
 $ let  
        !(I# pad)
         = max 0 pad'

        alloc len
         = F.mallocBytes (I# len)
        {-# INLINE alloc #-}

        write  ptr ix val
         = F.pokeByteOff ptr (I# ix) (fromIntegral (I# val) :: Word8)
        {-# INLINE write #-}

        make   ptr len
         = do   fptr    <- F.newForeignPtr F.finalizerFree ptr
                return  $  BS.PS fptr 0 (I# len)
        {-# INLINE make #-}

   in   storeIntPadWith# alloc write i pad make
{-# NOINLINE showIntPadToByteString #-}


-- | Store an ASCII `Int` into a buffer, producing the number of bytes written.
storeIntPad
        :: Ptr Word8                   -- ^ Pointer to output buffer.
        -> Int                         -- ^ Int to store.
        -> Int                         -- ^ Minimum number of digits.
        -> IO Int                      -- ^ Number of bytes written.

storeIntPad (Ptr addr) (I# val) (I# pad)
 = storeIntPad# addr val pad
{-# INLINE storeIntPad #-}


-- | Store an ASCII `Int` into a buffer, producing the number of bytes written.
--
--   * This function is set to NOINLINE, so it can be safely called from
--     multiple places in the program.
--
storeIntPad# 
        :: Addr#                        -- ^ Address of output buffer.
        -> Int#                         -- ^ Int to store.
        -> Int#                         -- ^ Minimum number of digits.
        -> IO Int                       -- ^ Number of bytes written.

storeIntPad# addr val pad
 = let
        -- move along, nothing to see..
        alloc _ 
         = return $ Ptr addr
        {-# INLINE alloc #-}

        write _ ix byte
         = F.pokeByteOff (Ptr addr) (I# ix) (fromIntegral (I# byte) :: Word8)
        {-# INLINE write #-}

        make _ len
         = return $ I# len
        {-# INLINE make #-}

  in do
        storeIntPadWith# alloc write val pad make
{-# NOINLINE storeIntPad# #-}


-- | Like `storeIntWith#`, but add leading zeros to the front of the integer
--   to pad it out to at least the given number of digits.

storeIntPadWith#
        :: (Int# -> IO buf)             -- ^ Function to allocate a new output buffer,
                                        --   given the length in bytes.
        -> (buf -> Int# -> Int# -> IO ())      
                                        -- ^ Function to write a byte to the buffer,
                                        --   given the index and byte value.
        -> Int#                         -- ^ Int to store.
        -> Int#                         -- ^ Pad out result to achieve at this many digits.
        -> (buf -> Int# -> IO b)        -- ^ Continuation for buffer and bytes written.
        -> IO b

storeIntPadWith# alloc write val pad k
 =  F.allocaBytes (I# (32# +# pad)) $ \(buf :: Ptr Word8)
 -> let 
        -- Get starting magnitude.
        !start
         | 1#   <- val <# 0#    = digits (0# -# val) 0#
         | otherwise            = digits val         0#
        {-# INLINE start #-}

        -- Load digits into buffer.
        digits !mag !ix
         = do   F.pokeByteOff buf (I# ix) 
                        (fromIntegral (I# (0x030# +# mag `remInt#` 10#)) :: Word8)
                let  !ix'  = ix +# 1#
                let  !mag' = mag `quotInt#` 10#
                (case mag' ==# 0# of
                  1#    -> padder ix'
                  _     -> digits mag' ix')
        {-# NOINLINE digits #-}

        -- Pad result out with zeros.
        padder !ix
         | 1#   <- ix >=# pad
         = sign ix

         | otherwise
         = do   F.pokeByteOff buf (I# ix)
                        (fromIntegral (I# 0x030#) :: Word8)
                padder (ix +# 1#)

        -- Load sign into buffer.
        sign !ix
         = case val <# 0# of
            1# -> do F.pokeByteOff buf (I# ix) 
                        (fromIntegral (I# 0x02d#) :: Word8)
                     create (ix +# 1#)
            _  ->    create ix
        {-# INLINE sign #-}

        -- Create a new output buffer, now that we know the length.
        create len 
         = do   out     <- alloc len
                output len out 0#
        {-# NOINLINE create #-}

        -- Read chars back from buffer to output them
        -- in the correct order.
        output len out ix0
         = go ix0
         where  go ix
                 | 1# <- ix <# len
                 = do   x :: Word8  <- F.peekByteOff buf (I# ((len -# 1#) -# ix))
                        let !(I# i) = fromIntegral x
                        write out ix i
                        go (ix +# 1#)

                 | otherwise
                 = k out len
        {-# INLINE output #-}
    in start
{-# INLINE storeIntPadWith# #-}

