
-- | Fallback implementations of stream operators.
--
--   Code using these stream operators is typically fused and vectorised by
--   the Repa plugin. If this transformation is successful then the resulting
--   GHC Core program will use primitives from the @Data.Array.Repa.Series.Prim@
--   module instead. If the fusion process is not successful then you will get a
--   compile-time warning and the implementations in this module will be used directly.
--
module Data.Array.Repa.Series.Fallback
        ( map
        , fold)
where
import Data.Array.Repa.Series.Stream
import Data.Vector.Unboxed              (Unbox)
import GHC.Exts
import Prelude                          hiding (map)

-- | Apply a function to all elements of a stream.
map     :: forall k a b
        .  (a -> b) -> Stream k a -> Stream k b

map f (Stream len g)
 = Stream len comp
 where  comp ix  = f (g ix)
        {-# INLINE comp #-}
{-# INLINE [0] map #-}


-- | Combine all elements of a stream with an associative operator.
fold    :: forall k a b. Unbox b 
        => (a -> b -> a) -> a -> Stream k b -> a
fold f z !source
 = go 0# z
 where  go !ix !acc
         | ix >=# streamLength source
         = acc

         | otherwise
         = let  x = streamNext source ix
           in   go (ix +# 1#) (f acc x)
{-# INLINE [0] fold #-}


