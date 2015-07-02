
module Data.Repa.Flow.Chunked.Fold
        ( foldlS
        , foldlAllS )
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.States                    as S
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Array.Generic.Index  as A
import qualified Data.Repa.Array.Generic        as A
#include "repa-flow.h"


-------------------------------------------------------------------------------
-- | Fold all elements of all streams in a bundle individually,
--   returning an array of per-stream results.
foldlS  
        :: ( States Int m
           , A.Target lDst a, A.Index lDst ~ Int
           , A.BulkI  lSrc b)
        => A.Name lDst                  -- ^ Destination layout.
        -> (a -> b -> a)                -- ^ Combining function.
        -> a                            -- ^ Starting value for fold.
        -> Sources Int m lSrc b         -- ^ Input elements to fold.
        -> m (A.Array lDst a)

foldlS nDst f z (G.Sources n pull)
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
                        loop_foldlS ix
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
{-# INLINE_FLOW foldlS #-}


-- | Fold all elements of all streams in a bundle together,
--   one stream after the other, returning the single final value.
foldlAllS
        :: ( States () m
           , A.BulkI   lSrc b)
        => (a -> b -> a)                -- ^ Combining function.
        -> a                            -- ^ Starting value for fold.
        -> Sources Int m lSrc b          -- ^ Input elements to fold.
        -> m a

foldlAllS f z (G.Sources n pull)
 = do   
        ref <- newRefs () Nothing

        let loop_foldlAllS !s !ix
             = pull ix eat_foldlAllS eject_foldlAllS
             where
                eat_foldlAllS arr
                 = do   let s'  = A.foldl f s arr
                        loop_foldlAllS s' ix
                {-# INLINE eat_foldlAllS #-}

                eject_foldlAllS
                 = case next ix n of
                        Nothing     -> writeRefs ref () (Just s)
                        Just ix'    -> loop_foldlAllS s ix'
                {-# INLINE eject_foldlAllS #-}
            {-# INLINE loop_foldlAllS #-}

        loop_foldlAllS z first
        Just x  <- readRefs ref ()
        return x
{-# INLINE_FLOW foldlAllS #-}


