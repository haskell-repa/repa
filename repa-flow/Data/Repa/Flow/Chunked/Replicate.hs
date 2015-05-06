
module Data.Repa.Flow.Chunked.Replicate
        ( replicates_i )
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Fusion.Unpack
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Array.Generic        as A
import qualified Data.Repa.Array.Generic.Target as A


replicates_i 
        :: ( Flow i m lSrc (Int, a)
           , A.TargetI lDst a
           , Unpack    (A.Buffer lDst a) at)
        => A.Name lDst
        -> Sources    i m lSrc (Int, a)
        -> m (Sources i m lDst a)

replicates_i nDst ss
        = G.smap_i (\_ arr -> A.replicates nDst arr) ss
{-# INLINE replicates_i #-}
