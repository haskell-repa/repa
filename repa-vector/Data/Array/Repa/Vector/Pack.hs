
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
--   `vpack` must return a stream because we don't know how many elements
--   will be produced by each thread.
--
class Pack r a where
 vpack :: Vector r (Bool, a) -> Vector S a


-- Chained
instance U.Unbox a => Pack N a where
 vpack c = vpack (vstreamOfChain c)


-- Streamed
instance U.Unbox a => Pack S a where
 vpack (AStream _ dstream _)
        = vcacheStream (S.packD dstream)

