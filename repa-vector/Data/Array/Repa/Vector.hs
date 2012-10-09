
module Data.Array.Repa.Vector
        ( Vector
        , N
        , S

        -- * Conversions
        , Distro        (..)
        , vchain
        , vstream

        -- * Projections
        , vlength

        -- * Replicate
        , vreplicate
        , vreplicateEachN

        -- * Mapping
        , Map           (..)

        -- * Zipping
        , Zip           (..)

        -- * Indexed
        , Indexed(..)

        -- * Pack
        , packS)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Map
import Data.Array.Repa.Vector.Zip
import Data.Array.Repa.Vector.Indexed
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Repr.Stream
import qualified Data.Array.Repa.Chain  as C
import qualified Data.Array.Repa.Stream as S
import Data.Array.Repa                  as R
import Data.Vector.Unboxed              (Unbox)


-- Replicate ------------------------------------------------------------------
-- | Construct a vector containing copies of the same value.
vreplicate :: Int -> a -> Vector D a
vreplicate len x
        = R.fromFunction (Z :. len) $ const x


-- | Given the distribution of the result, 
--   and a vector of pairs containing a count and an element,
--   replicate each element the number of times given by the count.
--
--   @
--   replicateEach 10 [(2,10), (5,20), (3,30)]
--     = [10,10,20,20,20,20,20,30,30,30]
--   @
--
vreplicateEachN :: Unbox a => Distro -> Vector N (Int, a) -> Vector N a
vreplicateEachN distro (AChained _ dchain _)
        = vcacheN (C.replicateEachD distro dchain) 


-- Pack -----------------------------------------------------------------------
-- | Given a vector of flags and values,
--   return just the values that had their corresponding flags set to `True`.
packS     :: Unbox a => Vector S (Bool, a) -> Vector S a
packS (AStream _ dstream _)
        = vcacheS (S.packD dstream)

