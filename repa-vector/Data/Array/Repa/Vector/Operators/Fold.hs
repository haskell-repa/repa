
module Data.Array.Repa.Vector.Operators.Fold
        (Fold(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Flow.Par.Segd            (Segd)
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import Prelude                                  hiding (map)


class Fold r a where
 type TF r
 -- | Segmented fold.
 folds  :: (a -> a -> a) -> a 
        -> Segd 
        -> Vector r a 
        -> Vector (TF r) a

 -- | Segmented sum.
 sums   :: (Num a, Fold r a) 
        => Segd -> Vector r a -> Vector (TF r) a
 sums segd vec
         = folds (+) 0 segd vec
 {-# INLINE [4] sums #-}


instance U.Unbox a => Fold D a where
 type TF D = O FD BN
 folds f z segd vec
        = folds f z segd (flow vec)


instance U.Unbox a => Fold (O mode BB) a where
 type TF (O mode BB)
        = O mode BN

 folds f z segd (AFlow ff)
        = AFlow (F.folds f z segd ff)
 {-# INLINE [4] folds #-}


