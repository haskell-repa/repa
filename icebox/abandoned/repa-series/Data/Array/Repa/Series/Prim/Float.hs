
module Data.Array.Repa.Series.Prim.Float where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Ref ------------------------------------------------------------------------
repa_newRefFloat        :: Float# -> W -> (# W, Ref Float #)
repa_newRefFloat x
        = unwrapIO' (Ref.new (F# x))
{-# INLINE [1] repa_newRefFloat #-}


repa_readRefFloat       :: Ref Float -> W -> (# W, Float# #)
repa_readRefFloat ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', F# i #) -> (# world', i #)
{-# INLINE [1] repa_readRefFloat #-}


repa_writeRefFloat      :: Ref Float -> Float# -> W -> W
repa_writeRefFloat ref val 
        = unwrapIO_ (Ref.write ref (F# val))
{-# INLINE [1] repa_writeRefFloat #-}


-- Vector ---------------------------------------------------------------------
repa_newVectorFloat     :: Word# -> W -> (# W, Vector Float #)
repa_newVectorFloat len    
        = unwrapIO' (V.new' len)
{-# INLINE [1] repa_newVectorFloat #-}


repa_readVectorFloat    :: Vector Float -> Word# -> W -> (# W, Float# #)
repa_readVectorFloat vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', F# i #) -> (# world', i #)
{-# INLINE [1] repa_readVectorFloat #-}


repa_writeVectorFloat   :: Vector Float -> Word# -> Float# -> W -> W
repa_writeVectorFloat vec ix val   
        = unwrapIO_ (V.write vec ix (F# val))
{-# INLINE [1] repa_writeVectorFloat #-}


repa_writeVectorFloatX4 :: Vector Float -> Word# -> FloatX4# -> W -> W
repa_writeVectorFloatX4 vec ix val
        = unwrapIO_ (writeFloatX4 vec ix val)
{-# INLINE [1] repa_writeVectorFloatX4 #-}


repa_writeVectorFloatX8 :: Vector Float -> Word# -> FloatX8# -> W -> W
repa_writeVectorFloatX8 vec ix val
        = unwrapIO_ (writeFloatX8 vec ix val)
{-# INLINE [1] repa_writeVectorFloatX8 #-}


repa_truncVectorFloat   :: Word# -> Vector Float -> W -> W
repa_truncVectorFloat len vec   
        = unwrapIO_ (V.trunc len vec)
{-# INLINE [1] repa_truncVectorFloat #-}


-- Series ---------------------------------------------------------------------
repa_nextFloat          :: Series k Float -> Word# -> W -> (# W, Float# #)
repa_nextFloat s ix world
 = case S.index s ix of
        F# i    -> (# world, i #)
{-# INLINE [1] repa_nextFloat #-}


repa_next4Float         :: Series (Down4 k) Float -> Word# -> W -> (# W, FloatX4# #)
repa_next4Float s ix world
 = case S.indexFloatX4 s ix of
        f4      -> (# world, f4 #)
{-# INLINE [1] repa_next4Float #-}


repa_next8Float         :: Series (Down8 k) Float -> Word# -> W -> (# W, FloatX8# #)
repa_next8Float s ix world
 = case S.indexFloatX8 s ix of
        f8      -> (# world, f8 #)
{-# INLINE [1] repa_next8Float #-}

