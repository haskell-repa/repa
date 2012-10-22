
module Data.Array.Repa.Vector.Operators.Indexs
        (Indexs (..))
where
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Chain.Indexs     as C
import Data.Array.Repa.Stream.Indexs    as S
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U

import Data.Array.Repa.Vector.Operators.Map

-- | Tag each element of a vector with its index in that vector.
--
--   `vindexs` doesn't have a stream-consuming version because we won't 
--    know how many elements will be produced by each thread.
--
-- @
-- vindexs [42,93,13] 
--  = [(0,42), (1,93), (2,13)]
-- @
--
class (Source r a) => Indexs r a where
 type IndexsR r
 vindexs :: (Source r2 a)
         => Vector r Int -> Vector r2 a -> Vector (IndexsR r) a


instance U.Unbox a => Indexs U a where
 type IndexsR U = D
 vindexs ixs vec
        = vmap (R.unsafeLinearIndex vec) ixs


instance Indexs D a where
 type IndexsR D = D
 vindexs ixs vec
        = vmap (R.unsafeLinearIndex vec) ixs


instance U.Unbox a => Indexs N a where
 type IndexsR N = N
 vindexs (AChain _ dchain _) vec
        = vcacheChain (C.indexsD dchain vec)

instance U.Unbox a => Indexs S a where
 type IndexsR S = S
 vindexs (AStream _ dstr _) vec
        = vcacheStream (S.indexsD dstr vec) -- TODO: losing size information


