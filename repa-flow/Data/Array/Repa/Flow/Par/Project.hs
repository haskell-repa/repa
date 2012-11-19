
module Data.Array.Repa.Flow.Par.Project
        (gather)
where
import Data.Array.Repa.Flow.Par.Base
import qualified Data.Array.Repa.Flow.Seq       as Seq
import GHC.Exts


-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather  :: (Int# -> a)
        -> Flow rep bal Int 
        -> Flow rep bal a

gather !get (Flow distro start frag)
 = Flow distro start frag'
 where  frag' state n 
         = Seq.gather get (frag state n)
{-# INLINE [2] gather #-}
