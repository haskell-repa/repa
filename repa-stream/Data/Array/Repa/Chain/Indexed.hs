
module Data.Array.Repa.Chain.Indexed
        ( indexed
        , indexedD)
where
import Data.Array.Repa.Chain.Base
import GHC.Exts
import Prelude  


-- | Tag each element of an stream with its index in that stream.
--
-- @
-- indexed [42,93,13]
--  = [(0,42), (1,93), (2,13)]
-- @
indexed :: Chain a -> Chain (Int,a)
indexed (Chain n start next) 
 = Chain n start next'
 where  next' ix s
         = case next ix s of
                Yield  s' x     -> Yield  s' (I# ix, x)
                Update s'       -> Update s'
{-# INLINE [1] indexed #-}


-- | Tag each element of a distributed stream with its index in that stream.
--
indexedD :: DistChain a -> DistChain (Int, a)
indexedD (DistChain distro frag)
 = DistChain distro frag'
 where  frag' i = indexed (frag i)
{-# INLINE [1] indexedD #-}
