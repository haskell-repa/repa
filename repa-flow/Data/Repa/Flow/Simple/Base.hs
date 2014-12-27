
module Data.Repa.Flow.Simple.Base
        ( Source, Sink
        , wrapI_i
        , wrapI_o)
where
import qualified Data.Repa.Flow.Generic as G

-- | Source consisting of a single stream.
type Source m e = G.Sources () m e

-- | Sink consisting of a single stream.
type Sink   m e = G.Sinks   () m e


-- Wrapping -------------------------------------------------------------------
wrapI_i  :: G.Sources Int m e -> Maybe (Source m e)
wrapI_i (G.Sources n pullX)
 | n /= 1       = Nothing
 | otherwise    
 = let  pullX' _ eat eject 
         = pullX (G.IIx 0 1) eat eject 
   in   Just $ G.Sources () pullX'
{-# INLINE wrapI_i #-}


wrapI_o  :: G.Sinks Int m e -> Maybe (Sink m e)
wrapI_o (G.Sinks n eatX ejectX)
 | n /= 1       = Nothing
 | otherwise    
 = let  eatX' _ x       = eatX   (G.IIx 0 1) x
        ejectX' _       = ejectX (G.IIx 0 1)
   in   Just $ G.Sinks () eatX' ejectX'
{-# INLINE wrapI_o #-}

