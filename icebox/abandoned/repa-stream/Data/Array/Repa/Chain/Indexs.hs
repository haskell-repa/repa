module Data.Array.Repa.Chain.Indexs
    ( indexs
    , indexsD)
where
import Data.Array.Repa.Chain.Base
import Data.Array.Repa

indexs  :: (Source r e)
	=> Chain Int
	-> Array r DIM1 e
	-> Chain e
indexs (Chain sz state next) vec
 = Chain sz state next'
 where
    next' ix s
     = case next ix s of
        Update s'   -> Update s'
        Yield  s' i -> Yield s' (vec `unsafeLinearIndex` i)
    {-# INLINE next' #-}
{-# INLINE indexs #-}


indexsD :: (Source r e)
        => DistChain Int -> Array r DIM1 e -> DistChain e
indexsD (DistChain distro frag) vec
 = DistChain distro frag'
 where  frag' i = indexs (frag i) vec
{-# INLINE [1] indexsD #-}
