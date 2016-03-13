
module Data.Repa.Flow.Chunked.Replicate
        ( replicates_i )
where
import Data.Repa.Flow.Chunked.Base
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Array.Generic        as A


replicates_i 
        :: ( Flow i m lSrc (Int, a)
           , A.TargetI lDst a)
        => A.Name lDst
        -> Sources    i m lSrc (Int, a)
        -> m (Sources i m lDst a)

replicates_i nDst ss
        = G.smap_i (\_ arr -> A.replicates nDst arr) ss
{-# INLINE replicates_i #-}
