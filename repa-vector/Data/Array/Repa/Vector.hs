
module Data.Array.Repa.Vector
        ( Vector
        , N
        , Map   (..)
        , Zip   (..)
        , vlength
        , vreplicate
        , vreplicateEach)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Chain            as C
import Data.Array.Repa                  as R
import Data.Vector.Unboxed              (Unbox)


-- | Construct a vector containing copies of the same value.
vreplicate :: Int -> a -> Vector D a
vreplicate len x
        = R.fromFunction (Z :. len) $ const x


-- | Given a vector of pairs containing a count an an element,
--   replicate element the number of times given by the count.
--
--   The first parameter sets the size of the resulting stream.
-- 
--   @
--   replicateEach 10 [(2,10), (5,20), (3,30)]
--     = [10,10,20,20,20,20,20,30,30,30]
--   @
--
vreplicateEach :: Unbox a => Distro -> Vector N (Int, a) -> Vector N a
vreplicateEach distro (AChained _ dchain _)
        = vcache (C.replicateEachD distro dchain) 
