
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
                        Yield1 (True,  x) _ -> push1 (Yield1 x False)
                        Yield1 (False, _) _ -> eat ()
                        Done                -> push1 Done

        get8' push8
         = push8 $ Pull1
{-# INLINE [1] pack #-}


-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: (a -> Bool) -> Flow a -> Flow a
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}
