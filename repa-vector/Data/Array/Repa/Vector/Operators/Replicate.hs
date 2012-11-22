
module Data.Array.Repa.Vector.Operators.Replicate
        ( replicate
        , replicate2
        , replicates
        , replicatesSplit )
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Operators.Bulk    as R
import Data.Array.Repa.Flow.Par.Segd            (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par       as F
import GHC.Exts
import Prelude  hiding (map, replicate)


-- | Replicate
replicate :: sh -> a -> Array D sh a
replicate ex x
        = fromFunction ex (const x)
{-# INLINE [4] replicate #-}


-- | Regular replicate, repeating each element by 2.
replicate2 :: Bulk r a => Vector r a -> Vector D a
replicate2 vec 
 = let  len2    = 2 * R.length vec
   in   fromFunction (Z :. len2) (\(Z :. ix) -> index vec (Z :. ix `div` 2))


-- | Segmented replicate.
replicates 
        :: Bulk r a
        => Segd 
        -> Vector r a
        -> Vector (O mode BB) a

replicates segd vec
 = let  get ix  = linearIndex vec (I# ix)
   in   fromFlowBB 
                (Z :. R.length vec)
                (F.replicates segd get)
{-# INLINE [4] replicates #-}



-- | Segmented replicate that takes a pre-split segment descriptor.
replicatesSplit
        :: Bulk r a
        => SplitSegd 
        -> Vector r a
        -> Vector (O mode BB) a

replicatesSplit segd vec
 = let  get ix  = linearIndex vec (I# ix)
   in   fromFlowBB 
                (Z :. R.length vec)
                (F.replicatesSplit segd get)
{-# INLINE [4] replicatesSplit #-}



