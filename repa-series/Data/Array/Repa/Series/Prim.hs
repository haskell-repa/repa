
-- | The @repa-plugin@ rewrites client code to use these primitives.
--
--   The plugin will use whatever names are in scope, so if you want to debug
--   your code you can import a different implementation of these primitives
--   into the module to be vectorized.
--
module Data.Array.Repa.Series.Prim
        ( Primitives (..)
        , primitives )
where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Prim.Loop
import Data.Array.Repa.Series.Prim.Int
import Data.Array.Repa.Series.Prim.Word
import Data.Array.Repa.Series.Prim.Float
import Data.Array.Repa.Series.Prim.Double
import GHC.Exts
import GHC.Types


-- | Primitives needed by the repa-plugin.
data Primitives
  = Primitives
  { prim_Series         :: forall k a. Series k a
  , prim_Vector         :: forall a.   Vector a
  , prim_Ref            :: forall a.   Ref a


    -- Loop Combinators -----------------------------------
  , prim_rateOfSeries   :: forall k a.  Series k a -> Word#

  , prim_loop           :: Word#  -> (Word# -> World -> World)
                        -> World -> World

  , prim_guard          :: Ref Word -> Bool
                        -> (Word# -> World -> World)
                        -> World -> World


    -- Hacks ----------------------------------------------
  , prim_nextInt_T2     :: forall k
                        .  Series k (Int,Int) -> Word#
                        -> World -> (# World, (# Int#, Int# #) #)


    -- Int ------------------------------------------------
  , prim_newRefInt      :: Int#    -> World -> (# World, Ref Int #) 
  , prim_readRefInt     :: Ref Int -> World -> (# World, Int# #)
  , prim_writeRefInt    :: Ref Int -> Int#  -> World -> World

  , prim_newVectorInt   :: Word# -> World -> (# World, Vector Int #)
  , prim_readVectorInt  :: Vector Int -> Word# -> World -> (# World, Int# #)
  , prim_writeVectorInt :: Vector Int -> Word# -> Int# -> World -> World
  , prim_sliceVectorInt :: Word# -> Vector Int -> World -> (# World, Vector Int #)

  , prim_nextInt        :: forall k
                        .  Series k Int -> Word#
                        -> World -> (# World, Int# #)


    -- Word ------------------------------------------------
  , prim_newRefWord     :: Word#    -> World -> (# World, Ref Word #) 
  , prim_readRefWord    :: Ref Word -> World -> (# World, Word# #)
  , prim_writeRefWord   :: Ref Word -> Word#  -> World -> World

  , prim_newVectorWord   :: Word# -> World -> (# World, Vector Word #)
  , prim_readVectorWord  :: Vector Word -> Word# -> World -> (# World, Word# #)
  , prim_writeVectorWord :: Vector Word -> Word# -> Word# -> World -> World
  , prim_sliceVectorWord :: Word# -> Vector Word -> World -> (# World, Vector Word #)

  , prim_nextWord       :: forall k
                        .  Series k Word -> Word#
                        -> World -> (# World, Word# #)


    -- Float ------------------------------------------------
  , prim_newRefFloat       :: Float#    -> World -> (# World, Ref Float #) 
  , prim_readRefFloat      :: Ref Float -> World -> (# World, Float# #)
  , prim_writeRefFloat     :: Ref Float -> Float#  -> World -> World

  , prim_newVectorFloat    :: Word# -> World -> (# World, Vector Float #)
  , prim_readVectorFloat   :: Vector Float -> Word# -> World -> (# World, Float# #)
  , prim_writeVectorFloat  :: Vector Float -> Word# -> Float# -> World -> World
  , prim_sliceVectorFloat  :: Word# -> Vector Float -> World -> (# World, Vector Float #)

  , prim_nextFloat         :: forall k
                           .  Series k Float -> Word#
                           -> World -> (# World, Float# #)


    -- Double ------------------------------------------------
  , prim_newRefDouble      :: Double#    -> World -> (# World, Ref Double #) 
  , prim_readRefDouble     :: Ref Double -> World -> (# World, Double# #)
  , prim_writeRefDouble    :: Ref Double -> Double#  -> World -> World

  , prim_newVectorDouble   :: Word# -> World -> (# World, Vector Double #)
  , prim_readVectorDouble  :: Vector Double -> Word# -> World -> (# World, Double# #)
  , prim_writeVectorDouble :: Vector Double -> Word# -> Double# -> World -> World
  , prim_sliceVectorDouble :: Word# -> Vector Double -> World -> (# World, Vector Double #)

  , prim_nextDouble        :: forall k
                           .  Series k Double -> Word#
                           -> World -> (# World, Double# #)
  }


-- | Table of primitives used by the repa-plugin.
primitives :: Primitives
primitives
  = Primitives
  { prim_Series = error "repa-series.primitives: you can't touch this."
  , prim_Vector = error "repa-series.primitives: you can't touch this."
  , prim_Ref    = error "repa-series.primitives: you can't touch this."

    -- Loop Combinators -------------------------
  , prim_rateOfSeries           = repa_rateOfSeries
  , prim_loop                   = repa_loop
  , prim_guard                  = repa_guard

    -- Hacks ------------------------------------
  , prim_nextInt_T2             = repa_nextInt_T2 

    -- Int --------------------------------------
  , prim_newRefInt              = repa_newRefInt
  , prim_readRefInt             = repa_readRefInt
  , prim_writeRefInt            = repa_writeRefInt

  , prim_newVectorInt           = repa_newVectorInt
  , prim_readVectorInt          = repa_readVectorInt
  , prim_writeVectorInt         = repa_writeVectorInt
  , prim_sliceVectorInt         = repa_sliceVectorInt

  , prim_nextInt                = repa_nextInt

    -- Word --------------------------------------
  , prim_newRefWord             = repa_newRefWord
  , prim_readRefWord            = repa_readRefWord
  , prim_writeRefWord           = repa_writeRefWord

  , prim_newVectorWord          = repa_newVectorWord
  , prim_readVectorWord         = repa_readVectorWord
  , prim_writeVectorWord        = repa_writeVectorWord
  , prim_sliceVectorWord        = repa_sliceVectorWord

  , prim_nextWord               = repa_nextWord

    -- Float --------------------------------------
  , prim_newRefFloat            = repa_newRefFloat
  , prim_readRefFloat           = repa_readRefFloat
  , prim_writeRefFloat          = repa_writeRefFloat

  , prim_newVectorFloat         = repa_newVectorFloat
  , prim_readVectorFloat        = repa_readVectorFloat
  , prim_writeVectorFloat       = repa_writeVectorFloat
  , prim_sliceVectorFloat       = repa_sliceVectorFloat

  , prim_nextFloat              = repa_nextFloat

    -- Double --------------------------------------
  , prim_newRefDouble            = repa_newRefDouble
  , prim_readRefDouble           = repa_readRefDouble
  , prim_writeRefDouble          = repa_writeRefDouble

  , prim_newVectorDouble         = repa_newVectorDouble
  , prim_readVectorDouble        = repa_readVectorDouble
  , prim_writeVectorDouble       = repa_writeVectorDouble
  , prim_sliceVectorDouble       = repa_sliceVectorDouble

  , prim_nextDouble              = repa_nextDouble
  }


-- Hacks ---------------------------------------------------------------------
-- TODO generalise
repa_nextInt_T2 :: Series k (Int,Int) -> Word# -> World -> (# World, (# Int#, Int# #) #)
repa_nextInt_T2 s ix world
 = case S.index s ix of
        (I# i1, I# i2)    -> (# world, (# i1, i2 #) #)
{-# INLINE repa_nextInt_T2 #-}


