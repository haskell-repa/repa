
module Data.Repa.Flow.Simple.Base
        ( Source (..)
        , Sink   (..)
        , Convert (..))
where
import qualified Data.Repa.Flow.Generic as G


data Source m e
        = Source !(G.Sources () m e)

data Sink   m e
        = Sink   !(G.Sinks   () m e)


class Convert a b where
 convert :: a -> b

instance Convert (G.Sources () m e) (Source m e) where
 convert s = Source s

instance Convert (Source m e) (G.Sources () m e) where
 convert g = case g of Source s -> s

instance Convert (G.Sinks  () m e) (Sink   m e)  where
 convert s = Sink   s

instance Convert (Sink   m e) (G.Sinks   () m e) where
 convert g = case g of Sink s  -> s


