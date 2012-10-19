
module Data.Array.Repa.Vector.Operators.Flatten
        (Flatten2 (..))
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Stream.Flatten   as S
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U


-- | Flatten a vector of tuples into a vector of their elements.
--
--   TODO: use less general result representations for unboxeded, delayed, chained sources.
class Flatten2 r a where
 type Flatten2R r
 vflatten2 :: Vector r (a, a) -> Vector (Flatten2R r) a


instance U.Unbox a => Flatten2 U a where
 type Flatten2R U = S
 vflatten2 vec
        = vflatten2 (vstream vec)
 {-# INLINE [1] vflatten2 #-}


instance U.Unbox a => Flatten2 D a where
 type Flatten2R D = S
 vflatten2 vec
        = vflatten2 (vstream vec)
 {-# INLINE [1] vflatten2 #-}


instance U.Unbox a => Flatten2 N a where
 type Flatten2R N = S
 vflatten2 vec
        = vflatten2 (vstream vec)
 {-# INLINE [1] vflatten2 #-}


instance U.Unbox a => Flatten2 S a where
 type Flatten2R S = S
 vflatten2 (AStream _ dstream _)
        = vcacheStream 
        $ S.flatten2D dstream
 {-# INLINE [1] vflatten2 #-}
