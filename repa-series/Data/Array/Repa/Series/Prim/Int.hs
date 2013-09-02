
module Data.Array.Repa.Series.Prim.Int where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Ref
repa_newRefInt          :: Int# -> World -> (# World, Ref Int #)
repa_newRefInt x        = unwrapIO' (Ref.new (I# x))
{-# INLINE repa_newRefInt #-}


repa_readRefInt         :: Ref Int -> World -> (# World, Int# #)
repa_readRefInt ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE repa_readRefInt #-}


repa_writeRefInt        :: Ref Int -> Int# -> World -> World
repa_writeRefInt ref val = unwrapIO_ (Ref.write ref (I# val))
{-# INLINE repa_writeRefInt #-}


-- Vector
repa_newVectorInt       :: Word#  -> World -> (# World, Vector Int #)
repa_newVectorInt len    = unwrapIO' (V.new' len)
{-# INLINE repa_newVectorInt #-}


repa_readVectorInt      :: Vector Int -> Word# -> World -> (# World, Int# #)
repa_readVectorInt vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE repa_readVectorInt #-}


repa_writeVectorInt     :: Vector Int -> Word# -> Int# -> World -> World
repa_writeVectorInt vec ix val   
        = unwrapIO_ (V.write vec ix (I# val))
{-# INLINE repa_writeVectorInt #-}


repa_sliceVectorInt     :: Word# -> Vector Int -> World -> (# World, Vector Int #)
repa_sliceVectorInt len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorInt #-}


-- Series
-- | Get the next element of a series.
repa_nextInt    :: Series k Int -> Word# -> World -> (# World, Int# #)
repa_nextInt s ix world
 = case S.index s ix of
        I# i    -> (# world, i #)
{-# INLINE repa_nextInt #-}


