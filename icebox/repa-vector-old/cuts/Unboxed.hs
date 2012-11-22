

-- Chain ----------------------------------------------------------------------
uchain  :: Unbox a
        => Vector U a
        -> Vector U a
uchain vec
        = vunchainP
        $ vchain vec
{-# INLINE [6] uchain #-}

vunchainP = unchainP
{-# INLINE [4] vunchainP #-}

vchain  = chain
{-# INLINE [4] vchain #-}

-- Replicate ------------------------------------------------------------------
ureplicate 
        :: Unbox a
        => Int
        -> a
        -> Vector U a
ureplicate n x
        = vcompute
        $ vreplicate n x
{-# INLINE [6] ureplicate #-}


-- Maps -----------------------------------------------------------------------
umap    :: (Unbox a, Unbox b)
        => (a -> b) -> Vector U a -> Vector U b
umap f vec1
        = vcompute
        $ vmap f vec1
{-# INLINE [6] umap #-}


-- Zips -----------------------------------------------------------------------
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


uzip4   :: (Unbox a, Unbox b, Unbox c, Unbox d)
        => Vector U a -> Vector U b -> Vector U c -> Vector U d
        -> Vector U (a, b, c, d)
uzip4 vec1 vec2 vec3 vec4
        = vcompute
        $ vzip4 (veat vec1)
                (veat vec2)
                (veat vec3)
                (veat vec4)
{-# INLINE [6] uzip4 #-}


-- ZipWiths -------------------------------------------------------------------
uzipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
          => (a -> b -> c -> d -> e)
          -> Vector U a -> Vector U b -> Vector U c -> Vector U d
          -> Vector U e
uzipWith4 f vec1 vec2 vec3 vec4
        = vcompute
        $ vzipWith4 f (veat vec1) (veat vec2) (veat vec3) (veat vec4)
{-# INLINE [6] uzipWith4 #-}
