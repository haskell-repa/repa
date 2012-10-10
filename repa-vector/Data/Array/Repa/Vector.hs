
module Data.Array.Repa.Vector
        ( Vector
        , N
        , S

        -- * Conversions
        , Distro(..)
        , vchain
        , vchainWith
        , vstream
        , vstreamWith
        , vstreamOfChain

        -- * Projections
        , vlength

        -- * Replicate
        , vreplicate
        , vreplicateEachN

        -- * Mapping
        , Map(..)

        -- * Zipping
        , Zip(..)

        -- * Indexed
        , Indexed(..)

        -- * Pack
        , Pack(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Map
import Data.Array.Repa.Vector.Zip
import Data.Array.Repa.Vector.Indexed
import Data.Array.Repa.Vector.Pack
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa                          as R
import qualified Data.Array.Repa.Chain          as C
import Data.Vector.Unboxed                      (Unbox)


-- Replicate ------------------------------------------------------------------
-- | Construct a vector containing copies of the same value.
vreplicate :: Int -> a -> Vector D a
vreplicate len x
        = R.fromFunction (Z :. len) $ const x


-- | Special case version of `vreplicateEach` where the distribution of the
--   result vector is known ahead of time.
--
--   @
--   replicateEach 10 [(2,10), (5,20), (3,30)]
--     = [10,10,20,20,20,20,20,30,30,30]
--   @
--
vreplicateEachN :: Unbox a => Distro -> Vector N (Int, a) -> Vector N a
vreplicateEachN distro (AChained _ dchain _)
        = vcacheChain (C.replicateEachD distro dchain) 

