
module Data.Repa.Array.Generic.Convert
        (Convert (..))
where
import Data.Repa.Array.Internals.Bulk

-- | Constant time conversion of one array representation to another.
class Convert r1 r2 a where
 convert  :: Array r1 a -> Array r2 a

