
module Data.Array.Repa.Vector.Pack
        ( Pack   (..)
        , Filter (..))
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Map
import Data.Array.Repa.Vector.Zip
import Data.Array.Repa.Stream.Pack      as S
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U


-- Pack -----------------------------------------------------------------------
-- | Given a vector of flags and elements,
--   return just the elements that had their corresponding flags set to `True`.
--
--   `vpack` must return a stream because we don't know how many elements
--   will be produced by each thread.
--
class Pack r a where
 vpack   :: Vector r (Bool, a) -> Vector S a

-- Unboxed
instance U.Unbox a => Pack U a where
 vpack v = vpack (vstream v)


-- Delayed
instance U.Unbox a => Pack D a where
 vpack v = vpack (vstream v)


-- Chained
instance U.Unbox a => Pack N a where
 vpack c = vpack (vstreamOfChain c)


-- Streamed
instance U.Unbox a => Pack S a where
 vpack (AStream _ dstream _)
        = vcacheStream (S.packD dstream)


-- Filter ---------------------------------------------------------------------
-- We could write a more polymorphic version of this in terms of vpack above,
-- but then we'd need some confusing looking class constraints. Instead, 
-- we just list out the instances we want one-by-one.
--
-- | Return just the elements of a vector that satify the given predicate.
--
--   `vfilter` must return a stream because we don't know how many elements
--   will be produced by each thread.
class Filter r a where
 vfilter  :: (a -> Bool) -> Vector r a -> Vector S a

-- Unboxed
instance U.Unbox a => Filter U a where
 vfilter f vec
  = vpack (vzip (vmap f vec) vec)


-- Delayed
instance U.Unbox a => Filter D a where
 vfilter f vec
  = vpack (vzip (vmap f vec) vec)


-- Chained
instance U.Unbox a => Filter N a where
 vfilter f vec
  = vpack (vzip (vmap f vec) vec)


-- Streamed
instance U.Unbox a => Filter S a where
 vfilter f vec
  = vpack (vzip (vmap f vec) vec)

