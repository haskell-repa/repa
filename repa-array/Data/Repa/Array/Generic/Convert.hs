{-# LANGUAGE IncoherentInstances #-}
module Data.Repa.Array.Generic.Convert
        (Convert (..))
where
import Data.Repa.Array.Internals.Bulk

-- | Constant time conversion of one array representation to another.
class Convert r1 a1 r2 a2 where
 convert  :: Array r1 a1 -> Array r2 a2

instance Convert r a r a where
 convert  = id
