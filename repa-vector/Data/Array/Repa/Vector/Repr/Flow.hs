
module Data.Array.Repa.Vector.Repr.Flow
        ( O
        , FD, FS
        , BB, BN
        , Array (..)
        , flow
        , unflow)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Flow.Par.Distro          (BB, BN)
import Data.Array.Repa.Flow.Seq                 (FD, FS, Touch)
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import GHC.Exts


-- | A parallel flow with the given mode and distribution.
data O mode dist

instance U.Unbox a => Source (O mode dist) a where
 data Array (O mode dist) sh a
        =  forall r
        .  Source r a
        => AFlow
                !sh                   -- Overall extent of vector.
                (F.Flow mode dist a)  -- A delayed parallel flow.
                (Vector r a)          -- A LAZY cache of the computed elements.

 extent (AFlow sh _ _)
        = sh
 {-# INLINE [4] extent #-}

 linearIndex (AFlow _ _ vec) ix
        = unsafeLinearIndex vec ix
 {-# INLINE [4] linearIndex #-}

 deepSeqArray (AFlow _ _ vec) x
  = vec `seq` x
 {-# INLINE [4] deepSeqArray #-}


-- | O(1). Convert an unboxed vector to a balanced flow.
flow :: U.Unbox a => Vector U a -> Vector (O FD BB) a
flow vec
 = let  !(Z :. (I# len)) = extent vec
        get ix           = unsafeLinearIndex vec (I# ix)
   in   AFlow   (extent vec)
                (F.generate len get)
                vec
{-# INLINE [4] flow #-}


-- | Compute a delayed flow, producing an unboxed vector.
unflow  :: (Touch a, U.Unbox a, F.Unflow dist) 
        => Vector (O FD dist) a -> Vector U a
unflow (AFlow sh ff _)
        = AUnboxed sh (F.unflow ff)
{-# INLINE [4] unflow #-}


instance Map (O mode dist) a where
 type TM (O mode dist)
       = (O mode dist)

 map f (AFlow sh ff arr)
        = AFlow sh 
                (F.map f ff)
                (R.map f arr)
