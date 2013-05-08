
module Data.Array.Repa.Flow.Par.Operator.Project
        (gather)
where
import Data.Array.Repa.Flow.Par.Flow
import qualified Data.Array.Repa.Flow.Seq       as Seq
import GHC.Exts


-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather  :: (Int# -> a)
        -> Flow mode dist Int 
        -> Flow mode dist a

gather !get (Flow gang distro start frag)
 = Flow gang distro start frag'
 where  
        frag' state n 
         = Seq.gather_bi get (frag state n)
        {-# INLINE frag' #-}
{-# INLINE [2] gather #-}

