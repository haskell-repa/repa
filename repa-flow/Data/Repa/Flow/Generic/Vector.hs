
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
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Given a bundle of sinks, produce a result sink for arrays.
--  
--   Each element pushed to the result sink is pushed to the corresponding
--   element of the bundle. If there are more elements than sinks then then
--   give  them to the spill action.
--
distribute_o 
        :: BulkI l a 
        => Sinks Int IO a       -- ^ Sinks to push elements into.
        -> (Int -> a -> IO ())  -- ^ Spill action, given the spilled element
                                --   along with its index in the array.
        -> IO (Sinks () IO (Array l a))

distribute_o (Sinks nSinks push eject) spill
 = do   
        let push_distribute _ !xs
             = loop_distribute 0
             where !nx = length xs

                   loop_distribute !ix
                    | ix >= nx
                    = return ()

                    | ix >= nSinks
                    = do spill ix (index xs ix)
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
        = distribute_o sinks (\_ _ -> return ())
{-# INLINE ddistribute_o #-}


-------------------------------------------------------------------------------
distribute2_o 
        :: BulkI l a 
        => Sinks SH2 IO a               -- ^ Sinks to push elements into.
        -> (SH2 -> a -> IO ())          -- ^ Spill action, given the spilled element
                                        --   along with its index in the array.
        -> IO (Sinks Int IO (Array l a))

distribute2_o (Sinks (Z :. a1 :. a0) push eject) spill
 = do   
        let push_distribute i1 !xs
             = loop_distribute 0
             where !nx = length xs

                   loop_distribute !ix
                    | ix >= nx
                    = return ()

                    | ix >= a0
                    = do spill (ish2 i1 ix) (index xs ix)
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
        = distribute2_o sinks (\_ _ -> return ())
{-# INLINE ddistribute2_o #-}


