
module Data.Repa.Flow.Simple.Base
        ( Source (..)
        , Sink   (..)
        , Wrap   (..)
        , wrap2, unwrap2)
where
import qualified Data.Repa.Flow.Generic as G


data Source m e
        = Source !(G.Sources () m e)

data Sink   m e
        = Sink   !(G.Sinks   () m e)


class Wrap a b | b -> a where
 wrap      :: a -> b
 unwrap    :: b -> a

instance Wrap (G.Sources () m e) (Source m e) where
 wrap s    = Source s
 unwrap g  = case g of Source s -> s

instance Wrap (G.Sinks   () m e) (Sink   m e)  where
 wrap s    = Sink   s
 unwrap g  = case g of Sink s  -> s


wrap2   :: (Wrap a1 b1, Wrap a2 b2)
        => (a1, a2) -> (b1, b2)
wrap2 (x1, x2) = (wrap x1, wrap x2)


unwrap2   :: (Wrap a1 b1, Wrap a2 b2)
          => (b1, b2) -> (a1, a2)
unwrap2 (x1, x2) = (unwrap x1, unwrap x2)
