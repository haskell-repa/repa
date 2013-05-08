
module Data.Array.Repa.Vector.Operators.Flatten
        (flatten2)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Repr.Delayed


flatten2
        :: (Shape sh, Bulk r (a, a))
        => Array r (sh :. Int) (a, a)
        -> Array D (sh :. Int) a

flatten2 arr
 = let  sh :. len       = extent arr
   in   fromFunction 
                (sh :. len * 2)
                (\(ix :. n) 
                   -> if n `mod` 2 == 0
                        then fst (index arr (ix :. (n `div` 2)))
                        else snd (index arr (ix :. (n `div` 2))))
{-# INLINE flatten2 #-}

