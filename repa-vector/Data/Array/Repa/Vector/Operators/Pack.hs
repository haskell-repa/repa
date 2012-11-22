
module Data.Array.Repa.Vector.Operators.Pack
        ( Pack(..)
        , filter
        , packs)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Segd                      (Segd)
import Data.Array.Repa.Vector.Operators.Replicate       as R
import Data.Array.Repa.Vector.Operators.Bulk            as R
import Data.Array.Repa.Vector.Operators.Map             as R
import Data.Array.Repa.Vector.Operators.Zip             as R
import qualified Data.Array.Repa.Flow.Par               as F
import qualified Data.Vector.Unboxed                    as U
import Prelude hiding (filter)


class Pack r a where
 -- | Produce only those elements that have their corresponding flag set.
 pack   :: U.Unbox a
        => Vector r (Bool, a) -> Vector (O FD BN) a


-------------------------------------------------------------------------------
instance Pack (O FD dist) a where
 pack (AFlow _ ff)
        = AFlow DistBN (F.pack ff)
 {-# INLINE [4] pack #-}


instance Pack D a where
 pack arr  = pack (flow arr)
 {-# INLINE pack #-}


instance Elt a => Pack U a where
 pack arr  = pack (flow arr)
 {-# INLINE pack #-}


--------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: (U.Unbox a, Map r a, Pack (TM r) a)
       => (a -> Bool) 
       -> Vector r a -> Vector (O FD BN) a

filter f vec
       = pack $ R.map (\x -> (f x, x)) vec


-- | Segmented Pack
packs   :: ( Pack (O FD BB) a, Bulk r Bool
           , U.Unbox a)
        => Vector r Bool
        -> Segd
        -> Vector U a
        -> Vector (O FD BN) a

packs flags segd vec
 = let  flags' :: Vector (O FD BB) Bool 
                = replicates segd flags
   in   pack $ R.zip flags' vec
