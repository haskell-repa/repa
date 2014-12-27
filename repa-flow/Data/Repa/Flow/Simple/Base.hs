
module Data.Repa.Flow.Simple.Base
        ( Source (..)
        , Sink   (..)
        , Wrap   (..)
        , wrapI_i
        , wrapI_o
        , wrap2, unwrap2)
where
import qualified Data.Repa.Flow.Generic as G


data Source m e
        = Source !(G.Sources () m e)

data Sink   m e
        = Sink   !(G.Sinks   () m e)


-- Wrapping -----------------------------------------------------------------------------
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


wrapI_i  :: G.Sources Int m e -> Maybe (Source m e)
wrapI_i (G.Sources n pullX)
 | n /= 1       = Nothing
 | otherwise    
 = let  pullX' _ eat eject 
         = pullX (G.IIx 0 1) eat eject 
   in   Just $ Source (G.Sources () pullX')
{-# INLINE wrapI_i #-}


wrapI_o  :: G.Sinks Int m e -> Maybe (Sink m e)
wrapI_o (G.Sinks n eatX ejectX)
 | n /= 1       = Nothing
 | otherwise    
 = let  eatX' _ x       = eatX   (G.IIx 0 1) x
        ejectX' _       = ejectX (G.IIx 0 1)
   in   Just $ Sink (G.Sinks () eatX' ejectX')
{-# INLINE wrapI_o #-}
 

