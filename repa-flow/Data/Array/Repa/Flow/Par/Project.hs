
module Data.Array.Repa.Flow.Par.Project
        (gather)
where
import Data.Array.Repa.Flow.Par.Base
import qualified Data.Array.Repa.Flow.Seq       as Seq
import GHC.Exts


-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather  :: (Int# -> a)
        -> Flow mode dist Int 
        -> Flow mode dist a

gather !get (Flow gang distro start frag)
 = Flow gang distro start frag'
 where  frag' state n 
         = Seq.gather get (frag state n)
        {-# INLINE frag' #-}
{-# INLINE [2] gather #-}

