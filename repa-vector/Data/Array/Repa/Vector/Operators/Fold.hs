
module Data.Array.Repa.Vector.Operators.Fold
        (Fold(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import Prelude                                  hiding (map)


class Fold r a where
 type TF r
 foldsP :: (a -> a -> a) -> a 
        -> Segd 
        -> Vector r a 
        -> Vector (TF r) a


instance U.Unbox a => Fold (O mode BB) a where
 type TF (O mode BB)
        = O mode BN

 foldsP f z segd (AFlow ff)
        = AFlow (F.folds f z segd ff)
 {-# INLINE foldsP #-}
