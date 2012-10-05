
module Data.Array.Repa.Vector
        ( Vector
        , N

        -- * Conversions
        , Distro        (..)
        , vchain

        -- * Projections
        , vlength

        -- * Maps and Zips
        , Map           (..)
        , Zip           (..)

        -- * Replicate
        , vreplicate
        , vreplicateEachN

        -- * Indexed
        , vindexed,     vindexedN)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Repr.Chain
import qualified Data.Array.Repa.Chain  as C
import Data.Array.Repa                  as R
import Data.Vector.Unboxed              (Unbox)


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
        = vcache (C.replicateEachD distro dchain) 


-- | Tag each element of an vector with its index in that vector.
--
-- @
-- indexed [42,93,13]
--  = [(0,42), (1,93), (2,13)]
-- @
vindexed  :: Source r a => Vector r a -> Vector D (Int, a)
vindexed vec
        = R.fromFunction (R.extent vec)
        $ \ ix@(Z :. n) -> (n, R.unsafeIndex vec ix)


-- | Chain consuming version.
--
--   When the source elements come in a chain, use this verison to avoid
--   creating an intermediate array.
--
vindexedN :: Unbox a    => Vector N a -> Vector N (Int, a)
vindexedN (AChained _ dchain _)
        = vcache (C.indexedD dchain)


