
module Data.Array.Repa.Vector.Indexed
        (Indexed (..))
where
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Chain.Indexed    as C
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U

-- | Tag each element of an vector with its index in that vector.
--
-- @
-- indexed [42,93,13]
--  = [(0,42), (1,93), (2,13)]
-- @
class Indexed r a where
 type IndexedR r
 vindexed :: Vector r a -> Vector (IndexedR r) (Int, a)


instance U.Unbox a => Indexed U a where
 type IndexedR U = D
 vindexed vec
        = R.fromFunction (R.extent vec)
        $ \ ix@(Z :. n) -> (n, R.unsafeIndex vec ix)


instance Indexed D a where
 type IndexedR D = D
 vindexed vec
        = R.fromFunction (R.extent vec)
        $ \ ix@(Z :. n) -> (n, R.unsafeIndex vec ix)


instance U.Unbox a => Indexed N a where
 type IndexedR N = N
 vindexed (AChained _ dchain _)
        = vcacheN (C.indexedD dchain)


