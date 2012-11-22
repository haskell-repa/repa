
module Data.Array.Repa.Vector.Operators.Scan
        (vmapAccum)
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Base
import Data.Array.Repa
import qualified Data.Array.Repa.Stream as S
import qualified Data.Vector.Unboxed    as U

-- MapAccum -------------------------------------------------------------------
-- | TODO: make this parallel
vmapAccum  :: U.Unbox a
           => (acc -> a -> (acc, a)) 
           -> acc
           -> Vector U a
           -> Vector U a

vmapAccum f acc0 (AUnboxed sh vec)
        = AUnboxed sh
        $ S.unstreamUnboxed $ S.mapAccum f acc0 $ S.streamUnboxed vec


