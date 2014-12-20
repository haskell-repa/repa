
module Data.Repa.Flows.Internals.Operator
        ( maps_i
        , distributes_o )
where
import Data.Repa.Flow.Internals.Base
import Data.Repa.Flows.Internals.Base
import Data.Repa.Array
import Prelude hiding (length)


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


-- | Given a gang of sinks, produce a result sink for arrays, where each element
--   pushed to the result sink is pushed to the corresponding element of the gang.
distributes_o :: Bulk r DIM1 a => Sinks a -> IO (Sink (Vector r a))
distributes_o (Sinks n push eject)
 = do   
        let push_distributes !xs
             = let nx = length xs
                   loop_distributes !i
                    | i >= nx     = return ()
                    | otherwise  
                    = do push i (index xs (Z :. i))
                         loop_distributes (i + 1)
               in loop_distributes 0
            {-# INLINE push_distributes #-}

        let eject_distributes
              = let loop_distributes !i
                     | i >= n    = return ()
                     | otherwise 
                     = do eject i
                          loop_distributes (i + 1)
                in  loop_distributes 0
            {-# INLINE eject_distributes #-}

        return $ Sink push_distributes eject_distributes
{-# INLINE [2] distributes_o #-}
