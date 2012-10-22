
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

        -- * Conversion
        , vfromUnboxed
        , vfromListUnboxed
        , vtoList

        -- * Projections
        , vlength
        , vindex

        -- * Replicate
        , vreplicate
        , vreplicateEach
        , vreplicates
        , vreplicatesSplit

        -- * Flatten
        , Flatten2(..)

        -- * Append
        , vappends
        , vappendsSplit

        -- * Indexed
        , Indexed(..)

        -- * Folds
	, fold_s
        , sum_s
        , count_s

        -- * Maps
        , Map(..)

        -- * Zips
        , Zip(..)
        , vzipWith

        -- * Scans
        , vmapAccum

        -- * Pack and Filter
        , Pack  (..)
        , Filter(..)
        , vpacks

        -- * Combine
        , vcombine2
        , vcombineSegs2)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Flatten
import Data.Array.Repa.Vector.Operators.Indexed
import Data.Array.Repa.Vector.Operators.Fold
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Vector.Operators.Zip
import Data.Array.Repa.Vector.Operators.Scan
import Data.Array.Repa.Vector.Operators.Pack
import Data.Array.Repa.Vector.Operators.Combine
import Data.Array.Repa.Vector.Segd
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Eval                     as R
import Data.Array.Repa                          as R
import qualified Data.Array.Repa.Chain          as C
import qualified Data.Array.Repa.Stream         as S
import qualified Data.Vector.Unboxed            as U
import Data.Vector.Unboxed                      (Unbox)


-- Computation ----------------------------------------------------------------
-- | Computation of array elements,
--   using a computation method appropriate to the vector representation.
--
--   TODO: make the parallel verison actually run in parallel.
class Compute r a where
 -- | Sequential computation.
 vcomputeUnboxedS :: Unbox a => Vector r a -> Vector U a

 -- | Parallel computation in some state-like monad. Use ST or IO.
 vcomputeUnboxedP :: Unbox a => Vector r a -> Vector U a
 

-- Delayed
instance Compute D a where
 vcomputeUnboxedS arr
  = R.computeUnboxedS arr

 vcomputeUnboxedP arr
  = R.suspendedComputeP arr


-- Chained
instance Compute N a where
 vcomputeUnboxedS (AChain sh dchain _) 
  = AUnboxed sh $ C.unchainUnboxedD dchain

 vcomputeUnboxedP (AChain sh dchain _) 
  = AUnboxed sh $ C.unchainUnboxedD dchain


-- Streamed
instance Compute S a where
 vcomputeUnboxedS (AStream  sh dstream _)
  = AUnboxed sh $ S.unstreamUnboxedD dstream

 vcomputeUnboxedP (AStream  sh dstream _)
  = AUnboxed sh $ S.unstreamUnboxedD dstream


-- Conversion -----------------------------------------------------------------
vfromUnboxed :: Unbox a => U.Vector a -> Vector U a
vfromUnboxed vec
        = R.fromUnboxed (Z :. U.length vec) vec

-- | Convert a list to an unboxed vector.
vfromListUnboxed :: Unbox a => [a] -> Vector U a
vfromListUnboxed xs = fromListUnboxed (Z :. length xs) xs

-- | Convert a vector to a list.
vtoList :: Source r a => Vector r a -> [a]
vtoList vec = toList vec


-- Replicate ------------------------------------------------------------------
-- | Construct a vector containing copies of the same value.
vreplicate :: Int -> a -> Vector D a
vreplicate len x
        = R.fromFunction (Z :. len) $ const x


-- | Replicate a value the given number of times.
--   The distribution of the result must be known up-front.
--
--   @
--   replicateEach 10 [(2,10), (5,20), (3,30)]
--     = [10,10,20,20,20,20,20,30,30,30]
--   @
--
vreplicateEach :: Unbox a => Distro -> Vector N (Int, a) -> Vector N a
vreplicateEach distro (AChain _ dchain _)
        = vcacheChain (C.replicateEachD distro dchain) 

