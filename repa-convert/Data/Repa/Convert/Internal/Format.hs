
module Data.Repa.Convert.Internal.Format
        (Format (..))
where


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
