
module Data.Repa.Flows.Internals.Operator
        ( maps_i
        , distributes_o
        , ddistributes_o
        , discards_o)
where
import Data.Repa.Flow.Internals.Base
import Data.Repa.Flows.Internals.Base
import Data.Repa.Array
import Prelude hiding (length)
import qualified Data.Vector.Fusion.Stream.Monadic as V


-------------------------------------------------------------------------------
-- | Apply a function to every element pulled from some sources, 
--   producing some new sources.
maps_i :: (a -> b) -> Sources a -> IO (Sources b)
maps_i f (Sources n pullsA)
 = return $ Sources n pullsB_map
 where  
        pullsB_map eat eject
         = pullsA eat_a eject_a
         where  
                eat_a i v = eat i (f v)
                {-# INLINE eat_a #-}

                eject_a i = eject i
                {-# INLINE eject_a #-}

        {-# INLINE [1] pullsB_map #-}
{-# INLINE [2] maps_i #-}


-------------------------------------------------------------------------------
-- | Given a gang of sinks, produce a result sink for arrays.
--  
--   Each element pushed to the result sink is pushed to the corresponding
--   element of the gang. If there are more elements than sinks then then give
--   them to the spill action.
--
distributes_o 
        :: Bulk r DIM1 a 
        => Sinks a              -- ^ Sinks to push elements into.
        -> ((Int, a) -> IO ())  -- ^ Spill action, given the spilled element
                                --   along with its index in the array.
        -> IO (Sink (Vector r a))

distributes_o (Sinks mnSinks push eject) spill
 = do   
        let push_distributes !xs
             = loop_distributes 0
             where !nx = length xs
                   loop_distributes !i
                    | i >= nx
                    = return ()

                    | Just nSinks <- mnSinks
                    , i >= nSinks
                    = do spill (i, index xs (Z :. i))
                         loop_distributes (i + 1)

                    | otherwise  
                    = do push i (index xs (Z :. i))
                         loop_distributes (i + 1)
                   {-# INLINE_INNER loop_distributes #-}

            {-# INLINE push_distributes #-}

        let eject_distributes
              = loop_distributes 0
              where loop_distributes !i
                     | Nothing     <- mnSinks
                     = return ()

                     | Just nSinks <- mnSinks
                     , i >= nSinks
                     = return ()

                     | otherwise 
                     = do eject i
                          loop_distributes (i + 1)
                     {-# INLINE_INNER loop_distributes #-}

            {-# INLINE eject_distributes #-}

        return $ Sink push_distributes eject_distributes
{-# INLINE [2] distributes_o #-}


-- | Like `distributes_o` but drop spilled elements on the floor.
ddistributes_o
        :: Bulk r DIM1 a
        => Sinks a
        -> IO (Sink (Vector r a))

ddistributes_o sinks 
        = distributes_o sinks (\_ -> return ())
{-# INLINE [2] ddistributes_o #-}


-- Discard --------------------------------------------------------------------
-- | A wide sink that drops all data on the floor.
discards_o :: IO (Sinks a)
discards_o
 = do   let push_discards !_ !_ = return ()
        let eject_discards _    = return ()
        return $ Sinks Nothing push_discards eject_discards
{-# INLINE [2] discards_o #-}


