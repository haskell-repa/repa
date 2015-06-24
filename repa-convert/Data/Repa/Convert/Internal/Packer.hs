
module Data.Repa.Convert.Internal.Packer
        ( Packer (..)
        , unsafeRunPacker)
where
import Data.Word
import qualified Foreign.Ptr    as F


-- | Packer wraps a function that can write to a buffer.
data Packer
  =  Packer
  { -- | Takes start of buffer, packs data into it, and calls the 
    --   continuation with a pointer to the byte just after the 
    --   last one that was written.
    fromPacker
        ::  F.Ptr Word8 
        -> (F.Ptr Word8 -> IO (Maybe (F.Ptr Word8)))
        -> IO (Maybe (F.Ptr Word8))
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
        -> F.Ptr Word8  -- ^ Start of buffer.
        -> IO (Maybe (F.Ptr Word8))
                        -- ^ Pointer to the byte after the last one written.

unsafeRunPacker (Packer make) buf
        = make buf (\buf' -> return (Just buf'))
{-# INLINE unsafeRunPacker #-}
