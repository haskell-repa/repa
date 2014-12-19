
module Data.Array.Repa.Series.Prim.Int where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Ref ------------------------------------------------------------------------
repa_newRefInt          :: Int# -> W -> (# W, Ref Int #)
repa_newRefInt x        = unwrapIO' (Ref.new (I# x))
{-# INLINE [1] repa_newRefInt #-}


repa_readRefInt         :: Ref Int -> W -> (# W, Int# #)
repa_readRefInt ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE [1] repa_readRefInt #-}


repa_writeRefInt        :: Ref Int -> Int# -> W -> W
repa_writeRefInt ref val = unwrapIO_ (Ref.write ref (I# val))
{-# INLINE [1] repa_writeRefInt #-}


-- Vector ---------------------------------------------------------------------
repa_newVectorInt       :: Word#  -> W -> (# W, Vector Int #)
repa_newVectorInt len    = unwrapIO' (V.new' len)
{-# INLINE [1] repa_newVectorInt #-}


repa_readVectorInt      :: Vector Int -> Word# -> W -> (# W, Int# #)
repa_readVectorInt vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE [1] repa_readVectorInt #-}


repa_writeVectorInt     :: Vector Int -> Word# -> Int# -> W -> W
repa_writeVectorInt vec ix val   
        = unwrapIO_ (V.write vec ix (I# val))
{-# INLINE [1] repa_writeVectorInt #-}


repa_truncVectorInt     :: Word# -> Vector Int -> W -> W
repa_truncVectorInt len vec   
        = unwrapIO_ (V.trunc len vec)
{-# INLINE [1] repa_truncVectorInt #-}


-- Series ---------------------------------------------------------------------
repa_nextInt    :: Series k Int -> Word# -> W -> (# W, Int# #)
repa_nextInt s ix world
 = case S.index s ix of
        I# i    -> (# world, i #)
{-# INLINE [1] repa_nextInt #-}


