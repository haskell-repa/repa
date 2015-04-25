
module Data.Repa.Convert.Format.Base
        ( Format   (..)
        , Packable (..))
where
import Data.Repa.Product
import Data.Word
import qualified Foreign.Ptr                    as S


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
-- | Class of storage formats that can have values packed and unpacked
--   from foreign bufferes. 
-- 
--   The methods are written using continuations to make it easier for
--   GHC to optimise its core code when packing/unpacking many fields.
--
class Format   format 
   => Packable format where


 -- | Pack a value into a buffer using the given format.
 -- 
 --   The buffer must be at least as long as the size returned by
 --   `fixedSize` / `packedSize`. 
 -- 
 --   If the format contains fixed width fields and the corresponding
 --   value has too many elements, then this function returns `False`, 
 --   otherwise `True`.
 --
 pack   :: S.Ptr Word8                  -- ^ Target Buffer.
        -> format                       -- ^ Storage format.
        -> Value format                 -- ^ Value to pack.
        -> (Int -> IO (Maybe a))        -- ^ Continue, given the number of bytes written.
        -> IO (Maybe a)


 -- | Unpack a value from a buffer using the given format.
 --
 --   This is the inverse of `pack` above.
 -- 
 --   PRECONDITION: The length of the buffer must be at least the
 --   minimum size required of the format (minSize). This allows
 --   us to avoid repeatedly checking for buffer overrun when
 --   unpacking fixed size formats.
 --
 unpack :: S.Ptr Word8                  -- ^ Source buffer.
        -> Int                          -- ^ Length of buffer.
        -> format                       -- ^ Format of buffer.
        -> ((Value format, Int) -> IO (Maybe a)) 
                                        -- ^ Continue, given the unpacked value and the 
                                        --   number of bytes read. 
        -> IO (Maybe a)
