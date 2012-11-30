
module Data.Array.Repa.Vector.Operators.Fold
        ( Folds         (..)
        , sums
        , counts
        , selects)
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


-------------------------------------------------------------------------------
class Folds r a where
 type TF r
 -- | Segmented fold.
 folds  :: (a -> a -> a) -> a 
        -> Segd
        -> Vector r a
        -> Vector (TF r) a


instance U.Unbox a => Folds D a where
 type TF D = O FD BN
 folds f z segd vec
        = folds f z segd (flow vec)
 {-# INLINE [4] folds #-}


instance U.Unbox a => Folds (O mode BB) a where
 type TF (O mode BB)
        = O mode BN

 folds f z segd (AFlow _ ff)
        = AFlow DistBN (F.folds f z segd ff)
 {-# INLINE [4] folds #-}


instance (U.Unbox a, Elt a) => Folds U a where
 type TF U = O FD BN
 folds f z segd vec
        = folds f z segd (flow vec)
 {-# INLINE [4] folds #-}


-------------------------------------------------------------------------------
-- | Segmented sum.
--   Sum up each segment individually,
--   producing a vector of all the rsults.
sums    :: (Num a, Folds r a) 
        => Segd 
        -> Vector r a 
        -> Vector (TF r) a
sums segd vec
        = folds (+) 0 segd vec
{-# INLINE [4] sums #-}


-- | Segmented count. 
--   Count the number of elements in each segment that match
--   the given predicate.
counts  :: (Eq a, Map r a, Folds (TM r) Int)
        => (a -> Bool) 
        -> Segd 
        -> Vector r a 
        -> Vector (TF (TM r)) Int

counts f segd vec
       = sums segd $ R.map (\x -> if f x then 1 else 0) vec
{-# INLINE [4] counts #-}


-- | Right-biased segmented selection.
--   If the predicate returns true then keep the right element over the
--   left one. Produces a single result element for each segment.
selects :: (Folds r a, U.Unbox a)
        => (a -> a -> Bool)
        -> a
        -> Segd
        -> Vector r a
        -> Vector (TF r) a

selects choose z segd vec
 = folds f z segd vec
 where  f x1 x2
         = if choose x1 x2 then x2 else x1
{-# INLINE [4] selects #-}

