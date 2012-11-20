
module Data.Array.Repa.Vector.Operators.Replicate
        ( replicates
        , replicatesSplit )
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Flow.Par.Segd            (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par       as F
import Prelude                                  hiding (map)
import GHC.Exts


-- | Segmented replicate.
replicates 
        :: Bulk r a
        => Segd 
        -> Vector r a
        -> Vector (O mode BB) a

replicates segd vec
 = let  get ix  = linearIndex vec (I# ix)
   in   fromFlow (F.replicates segd get)
{-# INLINE [4] replicates #-}


-- | Segmented replicate that takes a pre-split segment descriptor.
replicatesSplit
        :: Bulk r a
        => SplitSegd 
        -> Vector r a
        -> Vector (O mode BB) a

replicatesSplit segd vec
 = let  get ix  = linearIndex vec (I# ix)
   in   fromFlow (F.replicatesSplit segd get)
{-# INLINE [4] replicatesSplit #-}

