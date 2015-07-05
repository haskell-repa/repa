
module Data.Repa.Convert.Internal.Packable
        (Packable (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Internal.Unpacker
import Data.Word
import GHC.Exts


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

 pack format value 
  = Packer (packer format value)
 {-# INLINE pack #-}


 -- | Unpack a value from a buffer using the given format.
 unpack :: format                       -- ^ Storage format.
        -> Unpacker (Value format)      -- ^ Unpacker for that format.

 unpack format 
  = Unpacker (unpacker format)
 {-# INLINE unpack #-}


 -- | Low level packing function for the given format.
 packer   :: format                     -- ^ Data format.
          -> Value format               -- ^ Value to pack.
          -> Addr#                      -- ^ Pointer to start of buffer.
          -> IO ()                      -- ^ Signal failure.
          -> (Addr# -> IO ())           -- ^ Accept the address after the packed field.
          -> IO ()

 -- | Low level unpacking function for the given format.
 unpacker :: format                     -- ^ Data format.
          -> Addr#                      -- ^ Start of buffer.
          -> Addr#                      -- ^ Pointer to first byte after end of buffer.
          -> (Word8 -> Bool)            -- ^ Detect a field terminator.
          -> IO ()                      -- ^ Signal failure.
          -> (Addr# -> Value format -> IO ())   -- ^ Accept an unpacked value.
          -> IO ()
