

-- | Combine all elements of a series with an associative operator.
fold    :: forall k a b. Prim b 
        => (a -> b -> a) -> a -> Series k b -> Process a

fold f z !source
 = go (int2Word# 0#) z
 where  go !ix !acc
         | geWord# ix (S.length source)
         = acc

         | otherwise
         = let  x = S.index source ix
           in   go (plusWord# ix (int2Word# 1#)) (f acc x)
{-# INLINE [0] fold #-}


-- | Combine all elements of a series with an associative operator.
--   The worker function is given the current index into the series.
foldIndex :: forall k a b. Prim b 
          => (Word -> a -> b -> a) -> a -> Series k b -> a

foldIndex f z !source
 = go (int2Word# 0#) z
 where  
        len = S.length source
        go !ix !acc
         | geWord# ix len
         = acc

         | otherwise
         = let  x = S.index source ix
           in   go (plusWord# ix (int2Word# 1#)) (f (W# ix) acc x)
{-# INLINE [0] foldIndex #-}


