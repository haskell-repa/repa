
module Data.Repa.Flow.Chunked.Fold
        ( foldlS_i )
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.States                    as S
import qualified Data.Repa.Array                as A
import qualified Data.Repa.Flow.Generic         as G
#include "repa-flow.h"


-- | Fold all the elements of each stream in a bundle, one stream after the
--   other, returning an array of fold results.
--
foldlS_i  
        :: ( States Int m
           , A.Target lDst a, A.Index lDst ~ Int
           , A.BulkI  lSrc b)
        => A.Name lDst
        -> (a -> b -> a)
        -> a
        -> Sources Int m lSrc b
        -> m (A.Array lDst a)

foldlS_i nDst f z (G.Sources n pull)
 = do  
        -- Refs to hold partial fold states between chunks.
        refsState       <- newRefs n z

        let loop_foldlS !ix
             = pull ix eat_foldlS eject_foldlS
             where  
                eat_foldlS arr
                 = do   s       <- readRefs refsState ix 
                        let !s' =  A.foldl f s arr 
                        writeRefs refsState ix s'
                {-# INLINE eat_foldlS #-}

                eject_foldlS 
                 = case next ix n of
                    Nothing     -> return ()
                    Just ix'    -> loop_foldlS ix'
                {-# INLINE eject_foldlS #-}
            {-# INLINE loop_foldlS #-}

        loop_foldlS first

        ls      <- S.toListM refsState
        return  $ A.fromList nDst ls
{-# INLINE foldlS_i #-}

