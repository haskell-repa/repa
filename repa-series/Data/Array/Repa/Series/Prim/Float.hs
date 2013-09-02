
module Data.Array.Repa.Series.Prim.Float where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types
import Debug.Trace


-- Ref ------------------------------------------------------------------------
repa_newRefFloat
        :: Float# -> World -> (# World, Ref Float #)
repa_newRefFloat x
        = unwrapIO' (Ref.new (F# x))
{-# INLINE repa_newRefFloat #-}


repa_readRefFloat
        :: Ref Float -> World -> (# World, Float# #)
repa_readRefFloat ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', F# i #) -> (# world', i #)
{-# INLINE repa_readRefFloat #-}


repa_writeRefFloat
        :: Ref Float -> Float# -> World -> World
repa_writeRefFloat ref val 
        = unwrapIO_ (Ref.write ref (F# val))
{-# INLINE repa_writeRefFloat #-}


-- Vector ---------------------------------------------------------------------
repa_newVectorFloat
        :: Word#  
        -> World -> (# World, Vector Float #)
repa_newVectorFloat len    
        = unwrapIO' (V.new' len)
{-# INLINE repa_newVectorFloat #-}


repa_readVectorFloat
        :: Vector Float -> Word# -> World -> (# World, Float# #)
repa_readVectorFloat vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', F# i #) -> (# world', i #)
{-# INLINE repa_readVectorFloat #-}


repa_writeVectorFloat     
        :: Vector Float -> Word# -> Float# -> World -> World
repa_writeVectorFloat vec ix val   
        = unwrapIO_ (V.write vec ix (F# val))
{-# INLINE repa_writeVectorFloat #-}


repa_sliceVectorFloat     
        :: Word# -> Vector Float -> World -> (# World, Vector Float #)
repa_sliceVectorFloat len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorFloat #-}


-- Series ---------------------------------------------------------------------
repa_nextFloat  
        :: Series k Float -> Word# -> World -> (# World, Float# #)
repa_nextFloat s ix world
 = case S.index s ix of
        F# i    -> (# world, i #)
{-# INLINE repa_nextFloat #-}


repa_next4Float 
        :: Series (Down4 k) Float -> Word# -> World -> (# World, FloatX4# #)
repa_next4Float s ix world
 = case S.indexFloatX4 s ix of
        f4      -> (# world, f4 #)
{-# INLINE repa_next4Float #-}

