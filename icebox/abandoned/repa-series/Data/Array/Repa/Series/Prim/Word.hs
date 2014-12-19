
module Data.Array.Repa.Series.Prim.Word where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Ref ------------------------------------------------------------------------
repa_newRefWord         :: Word# -> W -> (# W, Ref Word #)
repa_newRefWord x       = unwrapIO' (Ref.new (W# x))
{-# INLINE [1] repa_newRefWord #-}


repa_readRefWord        :: Ref Word -> W -> (# W, Word# #)
repa_readRefWord ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', W# i #) -> (# world', i #)
{-# INLINE [1] repa_readRefWord #-}


repa_writeRefWord        :: Ref Word -> Word# -> W -> W
repa_writeRefWord ref val = unwrapIO_ (Ref.write ref (W# val))
{-# INLINE [1] repa_writeRefWord #-}


-- Vector ---------------------------------------------------------------------
repa_newVectorWord       :: Word#  -> W -> (# W, Vector Word #)
repa_newVectorWord len    = unwrapIO' (V.new' len)
{-# INLINE [1] repa_newVectorWord #-}


repa_readVectorWord      :: Vector Word -> Word# -> W -> (# W, Word# #)
repa_readVectorWord vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', W# i #) -> (# world', i #)
{-# INLINE [1] repa_readVectorWord #-}


repa_writeVectorWord     :: Vector Word -> Word# -> Word# -> W -> W
repa_writeVectorWord vec ix val   
        = unwrapIO_ (V.write vec ix (W# val))
{-# INLINE [1] repa_writeVectorWord #-}


repa_truncVectorWord     :: Word# -> Vector Word -> W -> W
repa_truncVectorWord len vec   
        = unwrapIO_ (V.trunc len vec)
{-# INLINE [1] repa_truncVectorWord #-}


-- Series ---------------------------------------------------------------------
repa_nextWord    :: Series k Word -> Word# -> W -> (# W, Word# #)
repa_nextWord s ix world
 = case S.index s ix of
        W# i    -> (# world, i #)
{-# INLINE [1] repa_nextWord #-}

