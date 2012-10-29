
module Data.Array.Repa.Flow.Filter
        ( pack
        , filter)
where
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Map
import Prelude hiding (map, filter)


-------------------------------------------------------------------------------
-- | Produce only the elements that have their corresponding flag set to `True`.
---  TODO: This can only produce elements one at a time.
--   Use a buffer instead to collect elements from the source.
pack :: Flow (Bool, a) -> Flow a
pack (Flow getSize get1 _)
 = Flow getSize' get1' get8'
 where
        getSize' _
         = do   size    <- getSize ()
                return  $ case size of
                           Exact len       -> Max len
                           Max   len       -> Max len

        get1' push1
         = eat ()
         where  eat ()
                 =  get1 $ \mx
                 -> case mx of
                        Just (True,  x) -> push1 (Just x)
                        Just (False, _) -> eat ()
                        Nothing         -> push1 Nothing

        get8' push8
         = push8 $ Right 1
{-# INLINE [1] pack #-}


-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: (a -> Bool) -> Flow a -> Flow a
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}
