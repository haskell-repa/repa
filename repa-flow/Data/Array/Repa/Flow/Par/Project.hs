
module Data.Array.Repa.Flow.Par.Project
        (gather)
where
import Data.Array.Repa.Flow.Par.Base
import qualified Data.Array.Repa.Flow.Seq       as Seq
import qualified Data.Vector.Unboxed            as U


-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather  :: U.Unbox a 
        => U.Vector a 
        -> Flow rep bal Int 
        -> Flow rep bal a

gather !vec (Flow distro frag)
 = Flow distro frag'
 where  frag' n = Seq.gather vec (frag n)
{-# INLINE [2] gather #-}

