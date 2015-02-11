
module Data.Repa.Flow.Generic.Shuffle
        ( shuffle_o
        , dshuffle_o)
where
import Data.Repa.Flow.Generic.Base              as F
import Data.Repa.Array                          as A
import Data.Repa.Eval.Elt
#include "repa-stream.h"



-- | Given a bundle of argument sinks, produce a result sink.
--   Arrays of indices and elements are pushed to the result sink. 
--   On doing so, the elements are pushed into the corresponding streams
--   of the argument sinks. 
-- 
--   If the index associated with an element does not have a corresponding
--   stream in the argument sinks, then pass it to the provided spill
--   function.
--  
--
-- @
--  |                      ..                         |
--  | [(0, v0), (1, v1), (0, v2), (0, v3), (2, v4)]   |  :: Sources Int IO (Array l (Int, a))
--  |                      ..                         |
--          \\       \\                          |
--           \\       .------------.            |
--            v                   v            .---------> spilled
--
--       |       ..       |       ..       |
--       |  [v0, v2, v3]  |      [v1]      |             :: Sinks Int IO (Array l a)
--       |       ..       |       ..       | 
-- @
--
shuffle_o
        :: ( BulkI lDst a, BulkI lSrc (Int, a)
           , Windowable lDst a
           , Target lDst a
           , Elt a)
        => Name lSrc                            -- ^ Name of source layout.
        -> Sinks Int IO (Array lDst a)          -- ^ Sinks to push results to.
        -> (Int -> Array lDst a -> IO ())       -- ^ Handle spilled elements.
        -> IO (Sinks () IO  (Array lSrc (Int, a)))

shuffle_o _ (Sinks nSinks opush oeject) aSpill
 = return $ Sinks () shuffle_push shuffle_eject
 where
        shuffle_push _ !arr
         = do   
                -- Partition the elements by segment number.
                let !parts   = A.partition name nSinks arr

                -- Push the individual segments into the argument sinks.
                let loop_shuffle_push !i
                     | i >= A.length parts  
                     = return ()

                     | i >= nSinks         
                     = do aSpill i  (parts `index` i)
                          loop_shuffle_push (i + 1)

                     | otherwise
                     = do opush i   (parts `index` i)
                          loop_shuffle_push (i + 1)

                loop_shuffle_push 0
        {-# INLINE shuffle_push #-}

        shuffle_eject _
         = do   
                let loop_shuffle_eject !i
                     | i >= nSinks
                     = return ()

                     | otherwise
                     = do oeject i
                          loop_shuffle_eject (i + 1)

                loop_shuffle_eject 0
        {-# INLINE shuffle_eject #-}

{-# INLINE_FLOW shuffle_o #-}


-- | Like `shuffle_o`, but drop spilled elements on the floor.
dshuffle_o
        :: ( BulkI lDst a, BulkI lSrc (Int, a)
           , Windowable lDst a
           , Target lDst a
           , Elt a)
        => Name lSrc                            -- ^ Name of source layout.
        -> Sinks Int IO (Array lDst a)          -- ^ Sinks to push results to.
        -> IO (Sinks () IO  (Array lSrc (Int, a)))

dshuffle_o nSrc sinks
        = shuffle_o nSrc sinks (\_ _ -> return ())
{-# INLINE dshuffle_o #-}

