
module Data.Array.Repa.Vector.Operators.Fold
        (Fold(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Flow.Par.Segd                    (Segd)
import Data.Array.Repa.Vector.Operators.Map             as R
import qualified Data.Array.Repa.Flow.Par               as F
import qualified Data.Vector.Unboxed                    as U
import Prelude                                          hiding (map)


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


 -- | Segmented count. 
 --   Count the number of elements in each segment that match
 --   the given predicate.
 counts :: (Eq a, Map r a, Fold (TM r) Int)
        => (a -> Bool) 
        -> Segd -> Vector r a -> Vector (TF (TM r)) Int
 counts f segd vec
        = sums segd $ R.map (\x -> if f x then 1 else 0) vec
 {-# INLINE [4] counts #-}


instance U.Unbox a => Fold D a where
 type TF D = O FD BN
 folds f z segd vec
        = folds f z segd (flow vec)
 {-# INLINE [4] folds #-}


instance (U.Unbox a, Elt a) => Fold U a where
 type TF U = O FD BN
 folds f z segd vec
        = folds f z segd (flow vec)
 {-# INLINE [4] folds #-}


instance U.Unbox a => Fold (O mode BB) a where
 type TF (O mode BB)
        = O mode BN

 folds f z segd (AFlow _ ff)
        = AFlow DistBN (F.folds f z segd ff)
 {-# INLINE [4] folds #-}


