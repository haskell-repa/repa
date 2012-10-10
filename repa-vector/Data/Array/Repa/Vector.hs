
module Data.Array.Repa.Vector
        ( Vector
        , S
        , N

        -- * Construction
        , Distro(..)
        , vstream
        , vstreamWith
        , vstreamOfChain
        , vchain
        , vchainWith

        -- * Computation
        , Compute(..)

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
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Eval                     as R
import Data.Array.Repa                          as R
import qualified Data.Array.Repa.Chain          as C
import qualified Data.Array.Repa.Stream         as S
import Data.Vector.Unboxed                      (Unbox)


-- Computation ----------------------------------------------------------------
-- | Parallel computation of array elements,
--   using a computation method appropriate to the vector representation.
class Compute r a where
 vcomputeUnboxedP  :: (Unbox a, Monad m) => Vector r a -> m (Vector U a)

instance Compute D a where
 vcomputeUnboxedP arr
  = R.computeUnboxedP arr

instance Compute N a where
 vcomputeUnboxedP (AChained sh dchain _) 
  = R.now (AUnboxed sh $ C.unchainUnboxedD dchain)

instance Compute S a where
 vcomputeUnboxedP (AStream  sh dstream _)
  = R.now (AUnboxed sh $ S.unstreamUnboxedD dstream)


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

