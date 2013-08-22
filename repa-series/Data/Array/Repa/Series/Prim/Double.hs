
module Data.Array.Repa.Series.Prim.Double where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Ref
repa_newRefDouble       :: Double# -> World -> (# World, Ref Double #)
repa_newRefDouble x     = unwrapIO' (Ref.new (D# x))
{-# INLINE repa_newRefDouble #-}


repa_readRefDouble      :: Ref Double -> World -> (# World, Double# #)
repa_readRefDouble ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', D# i #) -> (# world', i #)
{-# INLINE repa_readRefDouble #-}


repa_writeRefDouble     :: Ref Double -> Double# -> World -> World
repa_writeRefDouble ref val = unwrapIO_ (Ref.write ref (D# val))
{-# INLINE repa_writeRefDouble #-}


-- Vector
repa_newVectorDouble    :: Word#  -> World -> (# World, Vector Double #)
repa_newVectorDouble len    = unwrapIO' (V.new len)
{-# INLINE repa_newVectorDouble #-}


repa_readVectorDouble   :: Vector Double -> Word# -> World -> (# World, Double# #)
repa_readVectorDouble vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', D# i #) -> (# world', i #)
{-# INLINE repa_readVectorDouble #-}


repa_writeVectorDouble  :: Vector Double -> Word# -> Double# -> World -> World
repa_writeVectorDouble vec ix val   
        = unwrapIO_ (V.write vec ix (D# val))
{-# INLINE repa_writeVectorDouble #-}


repa_sliceVectorDouble  :: Word# -> Vector Double -> World -> (# World, Vector Double #)
repa_sliceVectorDouble len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorDouble #-}


-- Series
-- | Get the next element of a series.
repa_nextDouble         :: Series k Double -> Word# -> World -> (# World, Double# #)
repa_nextDouble s ix world
 = case S.index s ix of
        D# i    -> (# world, i #)
{-# INLINE repa_nextDouble #-}
