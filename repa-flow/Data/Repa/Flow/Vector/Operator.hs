
module Data.Repa.Flow.
        ( distributes_o
        , ddistributes_o
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.Gang.Base
import Data.Repa.Array
import Prelude hiding (length)
import qualified Data.Vector.Fusion.Stream.Monadic as V



-------------------------------------------------------------------------------
-- | Given a gang of sinks, produce a result sink for arrays.
--  
--   Each element pushed to the result sink is pushed to the corresponding
--   element of the gang. If there are more elements than sinks then then give
--   them to the spill action.
--
distributes_o 
        :: Bulk r DIM1 a 
        => Sinks IO a           -- ^ Sinks to push elements into.
        -> ((Int, a) -> IO ())  -- ^ Spill action, given the spilled element
                                --   along with its index in the array.
        -> IO (Sink IO (Vector r a))

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
        => Sinks IO a
        -> IO (Sink IO (Vector r a))

ddistributes_o sinks 
        = distributes_o sinks (\_ -> return ())
{-# INLINE [2] ddistributes_o #-}

