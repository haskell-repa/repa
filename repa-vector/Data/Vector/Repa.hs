
module Data.Vector.Repa
        ( umap
        , uzip2
        , uzip3
        , ureplicate

        , veat
        , vcompute)
where
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Repa.Base
import Data.Array.Repa.Eval
import Data.Array.Repa                  as R




umap    :: (Unbox a, Unbox b)
        => (a -> b) -> Vector U a -> Vector U b
umap f vec1
        = vcompute
        $ vmap f vec1
{-# INLINE [6] umap #-}


uzip2   :: (Unbox a, Unbox b)
        => Vector U a -> Vector U b -> Vector U (a, b)
uzip2 vec1 vec2
        = vcompute
        $ vzip vec1 vec2
{-# INLINE [6] uzip2 #-}


uzip3   :: (Unbox a, Unbox b, Unbox c)
        => Vector U a -> Vector U b -> Vector U c
        -> Vector U (a, b, c)
uzip3 vec1 vec2 vec3
        = vcompute
        $ vzip3 (veat vec1)
                (veat vec2)
                (veat vec3)
{-# INLINE [6] uzip3 #-}


ureplicate 
        :: Unbox a
        => Int
        -> a
        -> Vector U a
ureplicate n x
        = vcompute
        $ vreplicate n x
{-# INLINE [6] ureplicate #-}


veat    = delay
{-# INLINE [4] veat #-}


vcompute = suspendedComputeP
{-# INLINE [4] vcompute #-}

{-# RULES "eat/compute"
    forall x. veat (vcompute x) = x #-}

