
module Data.Repa.Flow.Generic.Vector
        ( -- | 1-dimensional distribution.
          distribute_o
        , ddistribute_o

          -- | 2-dimensional distribution.
        , distribute2_o
        , ddistribute2_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array
import Prelude hiding (length)
#include "repa-flow.h"


-------------------------------------------------------------------------------
-- | Given a bundle of sinks indexed by an `Int`, 
--   produce a result sink for arrays.
--  
--   Each time an array is pushed to the result sink, its elements are
--   pushed to the corresponding streams of the argument sink. If there
--   are more elements than sinks then then give  them to the spill action.
--
-- @
-- 
--   |          ..             |
--   | [w0,  x0,  y0,  z0]     |   :: Sinks () IO (Array l a)
--   | [w1,  x1,  y1,  z1, u1] |     (sink for a single stream of arrays)
--   |          ..             |
--
--      |    |    |    |    |
--      v    v    v    v    .------> spilled
--
--    | .. | .. | .. | .. |
--    | w0 | x0 | y0 | z0 |        :: Sinks Int IO a
--    | w1 | x1 | y1 | z1 |          (sink for several streams of elements)
--    | .. | .. | .. | .. |
-- @
--
distribute_o 
        :: BulkI l a 
        => (Int -> a -> IO ())  -- ^ Spill action, given the spilled element
                                --   along with its index in the array.
        -> Sinks Int IO a       -- ^ Sinks to push elements into.
        -> IO (Sinks () IO (Array l a))

distribute_o aSpill (Sinks nSinks push eject)
 = do   
        let push_distribute _ !xs
             = loop_distribute 0
             where !nx = length xs

                   loop_distribute !ix
                    | ix >= nx
                    = return ()

                    | ix >= nSinks
                    = do aSpill ix (index xs ix)
                         loop_distribute (ix + 1)

                    | otherwise  
                    = do push  ix (index xs ix)
                         loop_distribute (ix + 1)
                   {-# INLINE loop_distribute #-}
            {-# INLINE push_distribute #-}

        let eject_distribute _
              = loop_distribute 0
              where 
                    loop_distribute !ix
                     | ix >= nSinks
                     = return ()

                     | otherwise 
                     = do eject ix
                          loop_distribute (ix + 1)
                    {-# INLINE loop_distribute #-}
            {-# INLINE eject_distribute #-}

        return $ Sinks () push_distribute eject_distribute
{-# INLINE_FLOW distribute_o #-}


-- | Like `distribute_o`, but drop spilled elements on the floor.
ddistribute_o
        :: BulkI l a
        => Sinks Int IO a
        -> IO (Sinks () IO (Array l a))

ddistribute_o sinks 
        = distribute_o (\_ _ -> return ()) sinks 
{-# INLINE ddistribute_o #-}


-------------------------------------------------------------------------------
-- | Like `distribute_o`, but with 2-d stream indexes.
--
--   Given the argument and result sinks, when pushing to the result the 
--   stream index is used as the first component for the argument sink,
--   and the index of the element in its array is used as the second 
--   component.
-- 
--   If you want to the components of stream index the other way around
--   then apply `flipIndex2_o` to the argument sinks.
--
distribute2_o 
        :: BulkI l a 
        => (SH2 -> a -> IO ())          -- ^ Spill action, given the spilled element
                                        --   along with its index in the array.
        -> Sinks SH2 IO a               -- ^ Sinks to push elements into.
        -> IO (Sinks Int IO (Array l a))

distribute2_o aSpill (Sinks (Z :. a1 :. a0) push eject)
 = do   
        let push_distribute i1 !xs
             = loop_distribute 0
             where !nx = length xs

                   loop_distribute !ix
                    | ix >= nx
                    = return ()

                    | ix >= a0
                    = do aSpill (ish2 i1 ix) (index xs ix)
                         loop_distribute (ix + 1)

                    | otherwise  
                    = do push  (ish2 i1 ix) (index xs ix)
                         loop_distribute (ix + 1)
                   {-# INLINE loop_distribute #-}
            {-# INLINE push_distribute #-}

        let eject_distribute i1
              = loop_distribute 0
              where 
                    loop_distribute !ix
                     | ix >= a0
                     = return ()

                     | otherwise 
                     = do eject (ish2 i1 ix)
                          loop_distribute (ix + 1)
                    {-# INLINE loop_distribute #-}
            {-# INLINE eject_distribute #-}

        return $ Sinks a1 push_distribute eject_distribute
{-# INLINE_FLOW distribute2_o #-}


-- | Like `distribute2_o`, but drop spilled elements on the floor.
ddistribute2_o
        :: BulkI l a
        => Sinks SH2 IO a
        -> IO (Sinks Int IO (Array l a))

ddistribute2_o sinks 
        = distribute2_o (\_ _ -> return ()) sinks 
{-# INLINE ddistribute2_o #-}


