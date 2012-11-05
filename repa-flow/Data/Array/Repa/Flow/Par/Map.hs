
module Data.Array.Repa.Flow.Par.Map
        (map)
where
import Data.Array.Repa.Flow.Par.Base
import qualified Data.Array.Repa.Flow.Seq       as Seq
import Prelude hiding (map)


-- | Apply a function to every element of a flow.
map :: (a -> b) -> Flow d r a -> Flow d r b
map f (Flow distro frag)
 = Flow distro frag'
 where  frag' ix = Seq.map f (frag ix)
{-# INLINE [1] map #-}
