
module Data.Repa.Convert.Internal.Unpacker
        ( Unpacker (..)
        , unsafeRunUnpacker)
where
import Data.IORef
import Data.Word
import GHC.Exts
import Prelude hiding (fail)
import qualified Foreign.Ptr            as F


---------------------------------------------------------------------------------------------------
data Unpacker a
  =  Unpacker 
  {  -- | Takes pointers to the first byte in the buffer; the first byte
     --   after the buffer; a predicate to detect a field terminator;
     --   a failure action; and a continuation.
     -- 
     --   The field terminator is used by variable length encodings where
     --   the length of the encoded data cannot be determined from the
     --   encoding itself.
     --
     --   We try to unpack a value from the buffer.
     --   If unpacking succeeds then call the continuation with a pointer
     --   to the next byte after the unpacked value, and the value itself,
     --   otherwise call the failure action.
     --
     fromUnpacker
        :: Addr#                 -- Start of buffer.
        -> Addr#                 -- Pointer to first byte after end of buffer.
        -> (Word8 -> Bool)       -- Detect a field terminator.
        -> IO ()                 -- Signal failure.
        -> (Addr# -> a -> IO ()) -- Accept an unpacked value.
        -> IO ()
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
        -> F.Ptr Word8          -- ^ Source buffer.
        -> Int                  -- ^ Length of source buffer.
        -> (Word8 -> Bool)      -- ^ Detect a field terminator.
        -> IO (Maybe (a, F.Ptr Word8))  
                -- ^ Unpacked result, and pointer to the byte after the last
                --   one read.

unsafeRunUnpacker (Unpacker f) (Ptr start) (I# len) stop
 = do   ref     <- newIORef Nothing
        f       start 
                (plusAddr# start len)
                stop
                (return ())
                (\addr' x -> writeIORef ref (Just (x, (Ptr addr'))))
        readIORef ref
{-# INLINE unsafeRunUnpacker #-}


