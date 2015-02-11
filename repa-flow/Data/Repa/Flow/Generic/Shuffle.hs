
module Data.Repa.Flow.Generic.Shuffle
        (shuffle_o)
where
import Data.Repa.Flow.Generic.Base              as F
import Data.Repa.Array                          as A
import Data.Repa.Eval.Elt
#include "repa-stream.h"



-- | 
--
-- @
--  |                      ..                         |
--  | [(0, v0), (1, v1), (0, v2), (0, v3), (2, v4)]   |  :: Sources Int IO (Array l (Int, a))
--  |                      ..                         |
--          \\                                  |
--           \\                                 |
--            v                                .---------> spilled
--       |         ..     |        ..      |
--       |                |                |      
--       |   [v0, v2, v3] |      [v1]      |            :: Sinks Int IO (Array l a)
--       |         ..     |        ..      | 
-- @
--
shuffle_o
        :: ( BulkI lDst a, BulkI lSrc (Int, a)
           , Windowable lDst a
           , Target lDst a
           , Elt a)
        => Sinks Int IO (Array lDst a)          -- ^ Sinks to push results to.
        -> (Int -> a -> IO ())                  -- ^ Handle spilled elements.
        -> Sinks () IO  (Array lSrc (Int, a))

shuffle_o (Sinks nSinks opush oeject)
 = return $ Sinks () shuffle_push shuffle_eject
 where
        shuffle_push _ arr
         = do   
                -- Partition the elements by segment number.
                let !parts   = A.partition name nSinks arr

                -- Push the individual segments into the argument sinks.
                let loop_shuffle_push !i
                     | i >= A.length parts
                     = return ()

                     | otherwise
                     = do opush i (parts `index` i)
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
