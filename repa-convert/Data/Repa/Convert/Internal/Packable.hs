
module Data.Repa.Convert.Internal.Packable
        (Packable (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Internal.Unpacker


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

