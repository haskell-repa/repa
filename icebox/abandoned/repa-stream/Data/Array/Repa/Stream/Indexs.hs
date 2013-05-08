module Data.Array.Repa.Stream.Indexs
    ( indexs
    , indexsD)
where
import Data.Array.Repa.Stream.Base
import Data.Array.Repa

indexs  :: (Source r e)
	=> Stream  Int
	-> Array   r DIM1 e
	-> Stream  e
indexs (Stream sz state next) vec
 = Stream sz state next'
 where
    next' s
     = case next s of
        Done        -> Done
        Update s'   -> Update s'
        Yield  s' i -> Yield s' (vec `unsafeLinearIndex` i)
    {-# INLINE next' #-}
{-# INLINE indexs #-}

indexsD :: (Source r e)
        => DistStream Int -> Array r DIM1 e -> DistStream e
indexsD (DistStream sz frags frag) vec
 = DistStream sz frags frag'
 where  frag' c' = indexs (frag c') vec
{-# INLINE [1] indexsD #-}

