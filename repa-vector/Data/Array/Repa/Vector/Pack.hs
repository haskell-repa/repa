
module Data.Array.Repa.Vector.Pack
        (Pack (..))
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Stream.Pack      as S
import qualified Data.Vector.Unboxed    as U


-- | Given a vector of flags and values,
--   return just the values that had their corresponding flags set to `True`.
--
--   `vpack` must produce a stream because we don't know how many elements
--   will be produced on each node.
--
class Pack r a where
 type PackR r
 vpack :: Vector r (Bool, a) -> Vector (PackR r) a


-- Chained
instance U.Unbox a => Pack N a where
 type PackR N = S
 vpack c = vpack (vstreamOfChain c)


-- Streamed
instance U.Unbox a => Pack S a where
 type PackR S = S
 vpack (AStream _ dstream _)
        = vcacheS (S.packD dstream)

