{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Vector.Operators.Fold
        ( -- * Uniform folds
          Fold          (..)
        , sum
        , prod
        , count
        , select

          -- * Segmented folds
        , Folds         (..)
        , sums
        , prods
        , counts
        , selects)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Bulk.Par
import Data.Array.Repa.Flow.Par.Segd                    (Segd)
import Data.Array.Repa.Vector.Operators.Map             as R
import qualified Data.Array.Repa.Flow.Par               as F
import qualified Data.Vector.Unboxed                    as U
import System.IO.Unsafe
import GHC.Exts
import Prelude  hiding (map, sum)


-------------------------------------------------------------------------------
class Fold r a where
 -- | Undirected fold of all elements in an array.
 fold   :: Shape sh
        => (a -> a -> a) -> a
        -> Array r sh a
        -> a

instance Bulk r a => Fold r a where
 fold f z arr
  = let  get ix          = linearIndex arr (I# ix)
         !(I# len)       = size (extent arr)
    in   unsafePerformIO
          $ foldAll theGang get f z len
 {-# INLINE [4] fold #-}


-- | Sum up all elements in an array.
sum     :: (Fold r a, Shape sh, Num a) => Array r sh a -> a
sum arr = fold (+) 0 arr
{-# INLINE [4] sum #-}


-- | Multiply together all elements in an array.
prod    :: (Fold r a, Shape sh, Num a) => Array r sh a -> a
prod arr = fold (*) 1 arr
{-# INLINE [4] prod #-}


-- | Count the number of elements that match the given predicate.
count   :: (Fold (TM r) a, Map r a, Shape sh, Num a) 
        => (a -> Bool) -> Array r sh a -> a
count f arr
        = sum $ R.map (\x -> if f x then 1 else 0) arr
{-# INLINE [4] count #-}


-- | Right-biased select.
--   If the predicate returns true then keep the right element over the
--   left one. 
select  :: (Fold r a, Shape sh)
        => (a -> a -> Bool) -> a -> Array r sh a -> a
select choose z arr
 = fold f z arr
 where  f x1 x2
         = if choose x1 x2 then x2 else x1      
{-# INLINE [4] select #-}


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


-- | Segmented product.
--   Reduce each segment individually,
--   producing a vector of all the rsults.
prods   :: (Num a, Folds r a) 
        => Segd 
        -> Vector r a 
        -> Vector (TF r) a
prods segd vec
        = folds (*) 1 segd vec
{-# INLINE [4] prods #-}


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
