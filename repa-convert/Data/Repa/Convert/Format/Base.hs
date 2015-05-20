
module Data.Repa.Convert.Format.Base
        ( Format   (..)
        , Packable (..)

        -- * Packer
        , Packer   (..)
        , unsafeRunPacker

        -- * Unpacker
        , Unpacker (..)
        , unsafeRunUnpacker)
where
import Data.Word
import Data.IORef
import qualified Foreign.Ptr                    as S
import Prelude  hiding (fail)

---------------------------------------------------------------------------------------------------
-- | Relates a storage format to the Haskell type of the value
--   that is stored in that format.
class Format f where

 -- | Get the type of a value with this format.
 type Value f  

 -- | Yield the number of separate fields in this format.
 fieldCount :: f -> Int


 -- | Yield the minumum number of bytes that a value of this
 --   format will take up. 
 -- 
 --   Packing a value into this format
 --   is guaranteed to use at least this many bytes.
 --   This is exact for fixed-size formats.
 minSize    :: f -> Int


 -- | For fixed size formats, yield their size (length) in bytes.
 --
 --   Yields `Nothing` if this is not a fixed size format.
 --
 fixedSize  :: f -> Maybe Int


 -- | Yield the size of a value in the given format.
 --
 --   Yields `Nothing` when a collection of values is to be packed into a
 --   fixed length format, but the size of the collection does not match
 --   the format.
 --
 --   If `fixedSize` returns a size then `packedSize` returns the same size.
 --
 packedSize :: f -> Value f -> Maybe Int


---------------------------------------------------------------------------------------------------
-- | Packer wraps a function that can write to a buffer.
data Packer
  =  Packer
  { -- | Takes start of buffer, packs data into it, and calls the 
    --   continuation with a pointer to the byte just after the 
    --   last one that was written.
    fromPacker
        :: S.Ptr Word8 
        -> (S.Ptr Word8 -> IO (Maybe (S.Ptr Word8)))
        -> IO (Maybe (S.Ptr Word8))
  }

instance Monoid Packer where
 mempty 
  = Packer $ \buf k -> k buf
 {-# INLINE mempty #-}

 mappend (Packer fa) (Packer fb)
  = Packer $ \buf0 k -> fa buf0 (\buf1 -> fb buf1 k)
 {-# INLINE mappend #-}


-- | Pack data into the given buffer.
--   
--   PRECONDITION: The buffer needs to be big enough to hold the packed data,
--   otherwise you'll corrupt the heap (bad). Use `packedSize` to work out
--   how big it needs to be.
--
unsafeRunPacker 
        :: Packer       -- ^ Packer to run.
        -> S.Ptr Word8  -- ^ Start of buffer.
        -> IO (Maybe (S.Ptr Word8))
                        -- ^ Pointer to the byte after the last one written.

unsafeRunPacker (Packer make) buf
        = make buf (\buf' -> return (Just buf'))
{-# INLINE unsafeRunPacker #-}


---------------------------------------------------------------------------------------------------
data Unpacker a
  =  Unpacker 
  {  -- | Takes pointers to the first byte in the buffer, the first byte
     --   after the buffer, and a special field terminating character to
     --   terminate variable length encodings where the length is not
     --   determined from the representation of the encoding itself.
     --
     --   If a value can be successfully unpacked from the buffer, then
     --   it is passed to the continuation, along with a pointer to the
     --   byte after the last one that was read.
     --
     fromUnpacker
        :: forall b
        .  S.Ptr Word8          -- Start of buffer.
        -> S.Ptr Word8          -- Pointer to first byte after end of buffer.
        -> (Word8 -> Bool)      -- Detect a field terminator.
        -> IO b                 -- Signal failure.
        -> (S.Ptr Word8 -> a -> IO b)  -- Eat an unpacked value.
        -> IO b
  }


instance Functor Unpacker where
 fmap f (Unpacker fx)
  =  Unpacker $ \start end stop fail eat
  -> fx start end stop fail $ \start_x x 
  -> eat start_x (f x)
 {-# INLINE fmap #-}


instance Applicative Unpacker where
 pure  x
  =  Unpacker $ \start _end _fail _stop eat
  -> eat start x
 {-# INLINE pure #-}

 (<*>) (Unpacker ff) (Unpacker fx)
  =  Unpacker $ \start end stop fail eat
  -> ff start   end stop fail $ \start_f f
  -> fx start_f end stop fail $ \start_x x
  -> eat start_x (f x)
 {-# INLINE (<*>) #-}


instance Monad Unpacker where
 return = pure
 {-# INLINE return #-}

 (>>=) (Unpacker fa) mkfb
  =  Unpacker $ \start end stop fail eat
  -> fa start end stop fail $ \start_x x
  -> case mkfb x of
        Unpacker fb
         -> fb start_x end stop fail eat
 {-# INLINE (>>=) #-}


-- | Unpack data from the given buffer.
--
--   PRECONDITION: The buffer must be at least the minimum size of the 
--   format (minSize). This allows us to avoid repeatedly checking for 
--   buffer overrun when unpacking fixed size format. If the buffer
--   is not long enough then you'll get an indeterminate result (bad).
--
unsafeRunUnpacker
        :: Unpacker a           -- ^ Unpacker to run.
        -> S.Ptr Word8          -- ^ Source buffer.
        -> Int                  -- ^ Length of source buffer.
        -> (Word8 -> Bool)      -- ^ Detect a field terminator.
        -> IO (Maybe (a, S.Ptr Word8))  
                -- ^ Unpacked result, and pointer to the byte after the last
                --   one read.

unsafeRunUnpacker (Unpacker f) start len stop
 = do   ref     <- newIORef Nothing
        f       start 
                (S.plusPtr start len)
                stop
                (return ())
                (\ptr x -> writeIORef ref (Just (x, ptr)))
        readIORef ref
{-# INLINE unsafeRunUnpacker #-}


---------------------------------------------------------------------------------------------------
-- | Class of storage formats that can have values packed and unpacked
--   from foreign bufferes. 
-- 
--   The methods are written using continuations to make it easier for
--   GHC to optimise its core code when packing/unpacking many fields.
--
class Format   format 
   => Packable format where


 -- | Pack a value into a buffer using the given format.
 pack   :: format                       -- ^ Storage format.
        -> Value format                 -- ^ Value   to pack.
        -> Packer                       -- ^ Packer  that can write the value.


 -- | Unpack a value from a buffer using the given format.
 unpack :: format                       -- ^ Storage format.
        -> Unpacker (Value format)      -- ^ Unpacker for that format.


