
module Data.Array.Repa.Series.Prim.Word where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Ref
repa_newRefWord         :: Word# -> World -> (# World, Ref Word #)
repa_newRefWord x       = unwrapIO' (Ref.new (W# x))
{-# INLINE repa_newRefWord #-}


repa_readRefWord        :: Ref Word -> World -> (# World, Word# #)
repa_readRefWord ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', W# i #) -> (# world', i #)
{-# INLINE repa_readRefWord #-}


repa_writeRefWord        :: Ref Word -> Word# -> World -> World
repa_writeRefWord ref val = unwrapIO_ (Ref.write ref (W# val))
{-# INLINE repa_writeRefWord #-}


-- Vector
repa_newVectorWord       :: Word#  -> World -> (# World, Vector Word #)
repa_newVectorWord len    = unwrapIO' (V.new' len)
{-# INLINE repa_newVectorWord #-}


repa_readVectorWord      :: Vector Word -> Word# -> World -> (# World, Word# #)
repa_readVectorWord vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', W# i #) -> (# world', i #)
{-# INLINE repa_readVectorWord #-}


repa_writeVectorWord     :: Vector Word -> Word# -> Word# -> World -> World
repa_writeVectorWord vec ix val   
        = unwrapIO_ (V.write vec ix (W# val))
{-# INLINE repa_writeVectorWord #-}


repa_sliceVectorWord     :: Word# -> Vector Word -> World -> (# World, Vector Word #)
repa_sliceVectorWord len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorWord #-}


-- Series
-- | Get the next element of a series.
repa_nextWord    :: Series k Word -> Word# -> World -> (# World, Word# #)
repa_nextWord s ix world
 = case S.index s ix of
        W# i    -> (# world, i #)
{-# INLINE repa_nextWord #-}

