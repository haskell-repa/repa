
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

import GHC.Exts
import GHC.Types


-- | Primitives needed by the repa-plugin.
data Primitives
  = Primitives
  { prim_Series         :: forall k a. Series k a
  , prim_Vector         :: forall a.   Vector a
  , prim_Ref            :: forall a.   Ref a


    -- Loop Combinators -----------------------------------
  , prim_rateOfSeries   :: forall k a.  Series k a -> Int#

  , prim_loop           :: Int#  -> (Int# -> World -> World)
                        -> World -> World

  , prim_guard          :: Ref Int -> Bool
                        -> (Int# -> World -> World)
                        -> World -> World


    -- Hacks ----------------------------------------------
  , prim_nextInt_T2     :: forall k
                        .  Series k (Int,Int) -> Int#
                        -> World -> (# World, (# Int#, Int# #) #)


    -- Int ------------------------------------------------
  , prim_addInt         :: Int# -> Int# -> Int#
  , prim_subInt         :: Int# -> Int# -> Int#
  , prim_mulInt         :: Int# -> Int# -> Int#
  , prim_divInt         :: Int# -> Int# -> Int#
  , prim_modInt         :: Int# -> Int# -> Int#
  , prim_remInt         :: Int# -> Int# -> Int#

  , prim_eqInt          :: Int# -> Int# -> Bool
  , prim_neqInt         :: Int# -> Int# -> Bool
  , prim_gtInt          :: Int# -> Int# -> Bool
  , prim_geInt          :: Int# -> Int# -> Bool
  , prim_ltInt          :: Int# -> Int# -> Bool
  , prim_leInt          :: Int# -> Int# -> Bool

  , prim_newRefInt      :: Int#    -> World -> (# World, Ref Int #) 
  , prim_readRefInt     :: Ref Int -> World -> (# World, Int# #)
  , prim_writeRefInt    :: Ref Int -> Int#  -> World -> World

  , prim_newVectorInt   :: Int# -> World -> (# World, Vector Int #)
  , prim_readVectorInt  :: Vector Int -> Int# -> World -> (# World, Int# #)
  , prim_writeVectorInt :: Vector Int -> Int# -> Int# -> World -> World
  , prim_sliceVectorInt :: Int# -> Vector Int -> World -> (# World, Vector Int #)

  , prim_nextInt        :: forall k
                        .  Series k Int -> Int#
                        -> World -> (# World, Int# #)


    -- Word ------------------------------------------------
  , prim_addWord        :: Word# -> Word# -> Word#
  , prim_subWord        :: Word# -> Word# -> Word#
  , prim_mulWord        :: Word# -> Word# -> Word#
  , prim_divWord        :: () -- Word# -> Word# -> Word#  NOT IMPLEMENTED
  , prim_modWord        :: () -- Word# -> Word# -> Word#  NOT IMPLEMENTED
  , prim_remWord        :: Word# -> Word# -> Word#

  , prim_eqWord         :: Word# -> Word# -> Bool
  , prim_neqWord        :: Word# -> Word# -> Bool
  , prim_gtWord         :: Word# -> Word# -> Bool
  , prim_geWord         :: Word# -> Word# -> Bool
  , prim_ltWord         :: Word# -> Word# -> Bool
  , prim_leWord         :: Word# -> Word# -> Bool

  , prim_newRefWord     :: Word#    -> World -> (# World, Ref Word #) 
  , prim_readRefWord    :: Ref Word -> World -> (# World, Word# #)
  , prim_writeRefWord   :: Ref Word -> Word#  -> World -> World

  , prim_newVectorWord   :: Int# -> World -> (# World, Vector Word #)
  , prim_readVectorWord  :: Vector Word -> Int# -> World -> (# World, Word# #)
  , prim_writeVectorWord :: Vector Word -> Int# -> Word# -> World -> World
  , prim_sliceVectorWord :: Int# -> Vector Word -> World -> (# World, Vector Word #)

  , prim_nextWord       :: forall k
                        .  Series k Word -> Int#
                        -> World -> (# World, Word# #)


    -- Float ------------------------------------------------
  , prim_addFloat          :: Float# -> Float# -> Float#
  , prim_subFloat          :: Float# -> Float# -> Float#
  , prim_mulFloat          :: Float# -> Float# -> Float#
  , prim_divFloat          :: Float# -> Float# -> Float#
  , prim_modFloat          :: () -- Float# -> Float# -> Float#  NOT IMPLEMENTED
  , prim_remFloat          :: () -- Float# -> Float# -> Float#  NOT IMPLEMENTED

  , prim_eqFloat           :: Float# -> Float# -> Bool
  , prim_neqFloat          :: Float# -> Float# -> Bool
  , prim_gtFloat           :: Float# -> Float# -> Bool
  , prim_geFloat           :: Float# -> Float# -> Bool
  , prim_ltFloat           :: Float# -> Float# -> Bool
  , prim_leFloat           :: Float# -> Float# -> Bool

  , prim_newRefFloat       :: Float#    -> World -> (# World, Ref Float #) 
  , prim_readRefFloat      :: Ref Float -> World -> (# World, Float# #)
  , prim_writeRefFloat     :: Ref Float -> Float#  -> World -> World

  , prim_newVectorFloat    :: Int# -> World -> (# World, Vector Float #)
  , prim_readVectorFloat   :: Vector Float -> Int# -> World -> (# World, Float# #)
  , prim_writeVectorFloat  :: Vector Float -> Int# -> Float# -> World -> World
  , prim_sliceVectorFloat  :: Int# -> Vector Float -> World -> (# World, Vector Float #)

  , prim_nextFloat         :: forall k
                           .  Series k Float -> Int#
                           -> World -> (# World, Float# #)


    -- Double ------------------------------------------------
  , prim_addDouble         :: Double# -> Double# -> Double#
  , prim_subDouble         :: Double# -> Double# -> Double#
  , prim_mulDouble         :: Double# -> Double# -> Double#
  , prim_divDouble         :: Double# -> Double# -> Double#
  , prim_modDouble         :: () -- Double# -> Double# -> Double#  NOT IMPLEMENTED
  , prim_remDouble         :: () -- Double# -> Double# -> Double#  NOT IMPLEMENTED

  , prim_eqDouble          :: Double# -> Double# -> Bool
  , prim_neqDouble         :: Double# -> Double# -> Bool
  , prim_gtDouble          :: Double# -> Double# -> Bool
  , prim_geDouble          :: Double# -> Double# -> Bool
  , prim_ltDouble          :: Double# -> Double# -> Bool
  , prim_leDouble          :: Double# -> Double# -> Bool

  , prim_newRefDouble      :: Double#    -> World -> (# World, Ref Double #) 
  , prim_readRefDouble     :: Ref Double -> World -> (# World, Double# #)
  , prim_writeRefDouble    :: Ref Double -> Double#  -> World -> World

  , prim_newVectorDouble   :: Int# -> World -> (# World, Vector Double #)
  , prim_readVectorDouble  :: Vector Double -> Int# -> World -> (# World, Double# #)
  , prim_writeVectorDouble :: Vector Double -> Int# -> Double# -> World -> World
  , prim_sliceVectorDouble :: Int# -> Vector Double -> World -> (# World, Vector Double #)

  , prim_nextDouble        :: forall k
                           .  Series k Double -> Int#
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
  , prim_addInt                 = (+#)
  , prim_subInt                 = (-#)
  , prim_mulInt                 = (*#)
  , prim_divInt                 = repa_divInt
  , prim_modInt                 = repa_modInt
  , prim_remInt                 = remInt#

  , prim_eqInt                  = (==#)
  , prim_neqInt                 = (/=#)
  , prim_gtInt                  = (>#)
  , prim_geInt                  = (>=#)
  , prim_ltInt                  = (<#)
  , prim_leInt                  = (<=#)

  , prim_newRefInt              = repa_newRefInt
  , prim_readRefInt             = repa_readRefInt
  , prim_writeRefInt            = repa_writeRefInt

  , prim_newVectorInt           = repa_newVectorInt
  , prim_readVectorInt          = repa_readVectorInt
  , prim_writeVectorInt         = repa_writeVectorInt
  , prim_sliceVectorInt         = repa_sliceVectorInt

  , prim_nextInt                = repa_nextInt


    -- Word --------------------------------------
  , prim_addWord                = plusWord#
  , prim_subWord                = minusWord#
  , prim_mulWord                = timesWord#
  , prim_divWord                = repa_divWord
  , prim_modWord                = repa_modWord
  , prim_remWord                = remWord#

  , prim_eqWord                 = eqWord#
  , prim_neqWord                = neWord#
  , prim_gtWord                 = gtWord#
  , prim_geWord                 = geWord#
  , prim_ltWord                 = ltWord#
  , prim_leWord                 = leWord#

  , prim_newRefWord             = repa_newRefWord
  , prim_readRefWord            = repa_readRefWord
  , prim_writeRefWord           = repa_writeRefWord

  , prim_newVectorWord          = repa_newVectorWord
  , prim_readVectorWord         = repa_readVectorWord
  , prim_writeVectorWord        = repa_writeVectorWord
  , prim_sliceVectorWord        = repa_sliceVectorWord

  , prim_nextWord               = repa_nextWord


    -- Float --------------------------------------
  , prim_addFloat               = plusFloat#
  , prim_subFloat               = minusFloat#
  , prim_mulFloat               = timesFloat#
  , prim_divFloat               = divideFloat#
  , prim_modFloat               = repa_modFloat
  , prim_remFloat               = repa_remFloat

  , prim_eqFloat                = eqFloat#
  , prim_neqFloat               = neFloat#
  , prim_gtFloat                = gtFloat#
  , prim_geFloat                = geFloat#
  , prim_ltFloat                = ltFloat#
  , prim_leFloat                = leFloat#

  , prim_newRefFloat            = repa_newRefFloat
  , prim_readRefFloat           = repa_readRefFloat
  , prim_writeRefFloat          = repa_writeRefFloat

  , prim_newVectorFloat         = repa_newVectorFloat
  , prim_readVectorFloat        = repa_readVectorFloat
  , prim_writeVectorFloat       = repa_writeVectorFloat
  , prim_sliceVectorFloat       = repa_sliceVectorFloat

  , prim_nextFloat              = repa_nextFloat


    -- Double --------------------------------------
  , prim_addDouble               = (+##)
  , prim_subDouble               = (-##)
  , prim_mulDouble               = (*##)
  , prim_divDouble               = (/##)
  , prim_modDouble               = repa_modDouble
  , prim_remDouble               = repa_remDouble

  , prim_eqDouble                = (==##)
  , prim_neqDouble               = (/=##)
  , prim_gtDouble                = (>##)
  , prim_geDouble                = (>=##)
  , prim_ltDouble                = (<##)
  , prim_leDouble                = (<=##)

  , prim_newRefDouble            = repa_newRefDouble
  , prim_readRefDouble           = repa_readRefDouble
  , prim_writeRefDouble          = repa_writeRefDouble

  , prim_newVectorDouble         = repa_newVectorDouble
  , prim_readVectorDouble        = repa_readVectorDouble
  , prim_writeVectorDouble       = repa_writeVectorDouble
  , prim_sliceVectorDouble       = repa_sliceVectorDouble

  , prim_nextDouble              = repa_nextDouble

  }

-- Loop combinators -----------------------------------------------------------
-- | Primitive stateful loop combinator.
repa_loop       :: Int#  -> (Int# -> World -> World) -> World -> World
repa_loop len worker world0
 = go 0# world0
 where  
        go ix world
         | ix >=# len           
         = world

         | world' <- worker ix world
         = go (ix +# 1#) world'
{-# INLINE repa_loop #-}


-- | Guard an inner context with a flag.
repa_guard      :: Ref Int -> Bool
                -> (Int# -> World -> World)
                -> World -> World

repa_guard ref flag worker world0
 | False                <- flag
 = world0

 | (# world1, ix #)     <- repa_readRefInt  ref world0
 , world2               <- repa_writeRefInt ref (ix +# 1#) world1
 , world3               <- worker ix world2
 = world3
{-# INLINE repa_guard #-}


-- | Get the Rate / Length of a series.
repa_rateOfSeries :: Series k a -> Int#
repa_rateOfSeries s = seriesLength s
{-# INLINE repa_rateOfSeries #-}


repa_split4  
        :: forall k
        .  RateNat k 
        -> (RateNat (Down4 k) -> World -> World)
        -> (RateNat (Tail4 k) -> World -> World)
        -> World -> World
repa_split4 r fDown4 fTail4 w
 = error "repa_split4: not done yet"
{-# NOINLINE repa_split4 #-}


-- Hacks ---------------------------------------------------------------------
-- TODO generalise
repa_nextInt_T2 :: Series k (Int,Int) -> Int# -> World -> (# World, (# Int#, Int# #) #)
repa_nextInt_T2 s ix world
 = case S.index s ix of
        (I# i1, I# i2)    -> (# world, (# i1, i2 #) #)
{-# INLINE repa_nextInt_T2 #-}


-- Int ========================================================================
-- Arithmetic
repa_divInt i1 i2       = case div (I# i1) (I# i2) of { I# i3   -> i3 }
{-# INLINE repa_divInt #-}

repa_modInt i1 i2       = case mod (I# i1) (I# i2) of { I# i3   -> i3 }
{-# INLINE repa_modInt #-}


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
repa_newVectorInt       :: Int#  -> World -> (# World, Vector Int #)
repa_newVectorInt len    = unwrapIO' (V.new len)
{-# INLINE repa_newVectorInt #-}


repa_readVectorInt      :: Vector Int -> Int# -> World -> (# World, Int# #)
repa_readVectorInt vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE repa_readVectorInt #-}


repa_writeVectorInt     :: Vector Int -> Int# -> Int# -> World -> World
repa_writeVectorInt vec ix val   
        = unwrapIO_ (V.write vec ix (I# val))
{-# INLINE repa_writeVectorInt #-}


repa_sliceVectorInt     :: Int# -> Vector Int -> World -> (# World, Vector Int #)
repa_sliceVectorInt len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorInt #-}


-- Series
-- | Get the next element of a series.
repa_nextInt    :: Series k Int -> Int# -> World -> (# World, Int# #)
repa_nextInt s ix world
 = case S.index s ix of
        I# i    -> (# world, i #)
{-# INLINE repa_nextInt #-}


-- Word ========================================================================
-- Arithmetic
repa_divWord            = error "repa-series: div[Word] not implemented"
{-# INLINE repa_divWord #-}

repa_modWord            = error "repa-series: mod[Word] not implemented"
{-# INLINE repa_modWord #-}


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
repa_newVectorWord       :: Int#  -> World -> (# World, Vector Word #)
repa_newVectorWord len    = unwrapIO' (V.new len)
{-# INLINE repa_newVectorWord #-}


repa_readVectorWord      :: Vector Word -> Int# -> World -> (# World, Word# #)
repa_readVectorWord vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', W# i #) -> (# world', i #)
{-# INLINE repa_readVectorWord #-}


repa_writeVectorWord     :: Vector Word -> Int# -> Word# -> World -> World
repa_writeVectorWord vec ix val   
        = unwrapIO_ (V.write vec ix (W# val))
{-# INLINE repa_writeVectorWord #-}


repa_sliceVectorWord     :: Int# -> Vector Word -> World -> (# World, Vector Word #)
repa_sliceVectorWord len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorWord #-}


-- Series
-- | Get the next element of a series.
repa_nextWord    :: Series k Word -> Int# -> World -> (# World, Word# #)
repa_nextWord s ix world
 = case S.index s ix of
        W# i    -> (# world, i #)
{-# INLINE repa_nextWord #-}



-- Float ======================================================================
-- Arithmetic
repa_modFloat           = error "repa-series: mod[Float] not implemented"
{-# NOINLINE repa_modFloat #-}

repa_remFloat           = error "repa-series: rem[Float] not implemented"
{-# NOINLINE repa_remFloat #-}


-- Ref
repa_newRefFloat          :: Float# -> World -> (# World, Ref Float #)
repa_newRefFloat x        = unwrapIO' (Ref.new (F# x))
{-# INLINE repa_newRefFloat #-}

repa_readRefFloat         :: Ref Float -> World -> (# World, Float# #)
repa_readRefFloat ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', F# i #) -> (# world', i #)
{-# INLINE repa_readRefFloat #-}

repa_writeRefFloat        :: Ref Float -> Float# -> World -> World
repa_writeRefFloat ref val = unwrapIO_ (Ref.write ref (F# val))
{-# INLINE repa_writeRefFloat #-}


-- Vector
repa_newVectorFloat       :: Int#  -> World -> (# World, Vector Float #)
repa_newVectorFloat len    = unwrapIO' (V.new len)
{-# INLINE repa_newVectorFloat #-}


repa_readVectorFloat      :: Vector Float -> Int# -> World -> (# World, Float# #)
repa_readVectorFloat vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', F# i #) -> (# world', i #)
{-# INLINE repa_readVectorFloat #-}


repa_writeVectorFloat     :: Vector Float -> Int# -> Float# -> World -> World
repa_writeVectorFloat vec ix val   
        = unwrapIO_ (V.write vec ix (F# val))
{-# INLINE repa_writeVectorFloat #-}


repa_sliceVectorFloat     :: Int# -> Vector Float -> World -> (# World, Vector Float #)
repa_sliceVectorFloat len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorFloat #-}


-- Series
-- | Get the next element of a series.
repa_nextFloat    :: Series k Float -> Int# -> World -> (# World, Float# #)
repa_nextFloat s ix world
 = case S.index s ix of
        F# i    -> (# world, i #)
{-# INLINE repa_nextFloat #-}


-- Double =====================================================================
-- Arithmetic
repa_modDouble           = error "repa-series: mod[Double] not implemented"
{-# NOINLINE repa_modDouble #-}

repa_remDouble           = error "repa-series: rem[Double] not implemented"
{-# INLINE repa_remDouble #-}


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
repa_newVectorDouble    :: Int#  -> World -> (# World, Vector Double #)
repa_newVectorDouble len    = unwrapIO' (V.new len)
{-# INLINE repa_newVectorDouble #-}


repa_readVectorDouble   :: Vector Double -> Int# -> World -> (# World, Double# #)
repa_readVectorDouble vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', D# i #) -> (# world', i #)
{-# INLINE repa_readVectorDouble #-}


repa_writeVectorDouble  :: Vector Double -> Int# -> Double# -> World -> World
repa_writeVectorDouble vec ix val   
        = unwrapIO_ (V.write vec ix (D# val))
{-# INLINE repa_writeVectorDouble #-}


repa_sliceVectorDouble  :: Int# -> Vector Double -> World -> (# World, Vector Double #)
repa_sliceVectorDouble len vec   
        = unwrapIO' (V.take len vec)
{-# INLINE repa_sliceVectorDouble #-}


-- Series
-- | Get the next element of a series.
repa_nextDouble         :: Series k Double -> Int# -> World -> (# World, Double# #)
repa_nextDouble s ix world
 = case S.index s ix of
        D# i    -> (# world, i #)
{-# INLINE repa_nextDouble #-}



