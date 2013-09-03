
module Data.Array.Repa.Series.Prim.Double where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types

-- Ref ------------------------------------------------------------------------
repa_newRefDouble       :: Double# -> W -> (# W, Ref Double #)
repa_newRefDouble x     = unwrapIO' (Ref.new (D# x))
{-# INLINE [1] repa_newRefDouble #-}


repa_readRefDouble      :: Ref Double -> W -> (# W, Double# #)
repa_readRefDouble ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', D# i #) -> (# world', i #)
{-# INLINE [1] repa_readRefDouble #-}


repa_writeRefDouble     :: Ref Double -> Double# -> W -> W
repa_writeRefDouble ref val = unwrapIO_ (Ref.write ref (D# val))
{-# INLINE [1] repa_writeRefDouble #-}


-- Vector ---------------------------------------------------------------------
repa_newVectorDouble    :: Word# -> W -> (# W, Vector Double #)
repa_newVectorDouble len    = unwrapIO' (V.new' len)
{-# INLINE [1] repa_newVectorDouble #-}


repa_readVectorDouble   :: Vector Double -> Word# -> W -> (# W, Double# #)
repa_readVectorDouble vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', D# i #) -> (# world', i #)
{-# INLINE [1] repa_readVectorDouble #-}


repa_writeVectorDouble  :: Vector Double -> Word# -> Double# -> W -> W
repa_writeVectorDouble vec ix val   
        = unwrapIO_ (V.write vec ix (D# val))
{-# INLINE [1] repa_writeVectorDouble #-}


repa_sliceVectorDouble  :: Word# -> Vector Double -> W -> (# W, Vector Double #)
repa_sliceVectorDouble len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE [1] repa_sliceVectorDouble #-}


-- Series ---------------------------------------------------------------------
repa_nextDouble         :: Series k Double -> Word# -> W -> (# W, Double# #)
repa_nextDouble s ix world
 = case S.index s ix of
        D# i    -> (# world, i #)
{-# INLINE [1] repa_nextDouble #-}


repa_next2Double        :: Series (Down2 k) Float -> Word# -> W -> (# W, DoubleX2# #)
repa_next2Double s ix world
 = case S.indexDoubleX2 s ix of
        d2      -> (# world, d2 #)
{-# INLINE [1] repa_next2Double #-}
