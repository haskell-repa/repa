
module Data.Repa.Flow.Generic.Vector
        ( distributes_o
        , ddistributes_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array
import Prelude hiding (length)


-------------------------------------------------------------------------------
-- | Given a gang of sinks, produce a result sink for arrays.
--  
--   Each element pushed to the result sink is pushed to the corresponding
--   element of the gang. If there are more elements than sinks then then give
--   them to the spill action.
--
distributes_o 
        :: Bulk r DIM1 a 
        => Sinks Int IO a       -- ^ Sinks to push elements into.
        -> ((Int, a) -> IO ())  -- ^ Spill action, given the spilled element
                                --   along with its index in the array.
        -> IO (Sinks () IO (Vector r a))

distributes_o (Sinks nSinks push eject) spill
 = do   
        let push_distributes _ !xs
             = loop_distributes 0
             where !nx = length xs

                   loop_distributes !ix
                    | ix >= nx
                    = return ()

                    | ix >= nSinks
                    = do spill (ix, index xs (Z :. ix))
                         loop_distributes (ix + 1)

                    | otherwise  
                    = do push (IIx ix nx) (index xs (Z :. ix))
                         loop_distributes (ix + 1)

                   {-# INLINE loop_distributes #-}
            {-# INLINE push_distributes #-}

        let eject_distributes _
              = loop_distributes 0
              where 
                    loop_distributes !ix
                     | ix >= nSinks
                     = return ()

                     | otherwise 
                     = do eject (IIx ix nSinks)
                          loop_distributes (ix + 1)
                    {-# INLINE loop_distributes #-}
            {-# INLINE eject_distributes #-}

        return $ Sinks () push_distributes eject_distributes
{-# INLINE [2] distributes_o #-}


-- | Like `distributes_o` but drop spilled elements on the floor.
ddistributes_o
        :: Bulk r DIM1 a
        => Sinks Int IO a
        -> IO (Sinks () IO (Vector r a))

ddistributes_o sinks 
        = distributes_o sinks (\_ -> return ())
{-# INLINE [2] ddistributes_o #-}
