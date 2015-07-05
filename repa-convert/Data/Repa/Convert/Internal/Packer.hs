
module Data.Repa.Convert.Internal.Packer
        ( Packer (..)
        , unsafeRunPacker)
where
import Data.Word
import Data.IORef
import GHC.Exts
import qualified Foreign.Ptr    as F


-- | Packer wraps a function that can write to a buffer.
data Packer
  =  Packer
  { -- | Takes start of buffer; failure action; and a continuation.
    -- 
    --   We try to pack data into the given buffer.
    --   If packing succeeds then we call the continuation with a pointer
    --   to the next byte after the packed value,
    --   otherwise we call the failure action.
    --
    fromPacker
        :: Addr#                -- Start of buffer.
        -> IO ()                -- Signal failure.
        -> (Addr# -> IO ())     -- Accept the address after the packed value.
        -> IO ()
  }


instance Monoid Packer where
 mempty 
  = Packer $ \buf _fail k -> k buf
 {-# INLINE mempty #-}

 mappend (Packer fa) (Packer fb)
  = Packer $ \buf0 fails k -> fa buf0 fails (\buf1 -> fb buf1 fails k)
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

unsafeRunPacker (Packer make) (Ptr addr)
 = do   ref     <- newIORef Nothing

        make addr
          (return ())
          (\addr' -> writeIORef ref (Just (Ptr addr')))

        readIORef ref
{-# INLINE unsafeRunPacker #-}
