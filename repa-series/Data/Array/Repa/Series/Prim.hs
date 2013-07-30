
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
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types

type World      = State# RealWorld


-- | Primitives needed by the repa-plugin.
data Primitives
  = Primitives
  { prim_Series         :: forall k a. Series k a
  , prim_Vector         :: forall a.   Vector a
  , prim_Ref            :: forall a.   Ref a


    -- Int ------------------------------------------------
    -- Arith Int
  , prim_addInt         :: Int# -> Int# -> Int#
  , prim_subInt         :: Int# -> Int# -> Int#
  , prim_mulInt         :: Int# -> Int# -> Int#
  , prim_divInt         :: Int# -> Int# -> Int#
  , prim_modInt         :: Int# -> Int# -> Int#
  , prim_remInt         :: Int# -> Int# -> Int#

    -- Eq Int
  , prim_eqInt          :: Int# -> Int# -> Bool
  , prim_neqInt         :: Int# -> Int# -> Bool
  , prim_gtInt          :: Int# -> Int# -> Bool
  , prim_geInt          :: Int# -> Int# -> Bool
  , prim_ltInt          :: Int# -> Int# -> Bool
  , prim_leInt          :: Int# -> Int# -> Bool

    -- Ref Int
  , prim_newRefInt      :: Int#    -> World -> (# World, Ref Int #) 
  , prim_readRefInt     :: Ref Int -> World -> (# World, Int# #)
  , prim_writeRefInt    :: Ref Int -> Int#  -> World -> World

    -- Vector Int
  , prim_newVectorInt   :: Int# -> World -> (# World, Vector Int #)
  , prim_readVectorInt  :: Vector Int -> Int# -> World -> (# World, Int# #)
  , prim_writeVectorInt :: Vector Int -> Int# -> Int# -> World -> World
  , prim_sliceVectorInt :: Int# -> Vector Int -> World -> (# World, Vector Int #)

    -- Loop
  , prim_rateOfSeries   :: forall k a.  Series k a -> Int#

  , prim_loop           :: Int#  -> (Int# -> World -> World)
                        -> World -> World

  , prim_guard          :: Ref Int -> Bool
                        -> (Int# -> World -> World)
                        -> World -> World

    -- Next
  , prim_nextInt        :: forall k
                        .  Series k Int -> Int#
                        -> World -> (# World, Int# #)

  , prim_nextInt_T2     :: forall k
                        .  Series k (Int,Int) -> Int#
                        -> World -> (# World, (# Int#, Int# #) #)
  }


-- | Table of primitives used by the repa-plugin.
primitives :: Primitives
primitives
  = Primitives
  { prim_Series         = error "repa-series.primitives: you can't touch this."
  , prim_Vector         = error "repa-series.primitives: you can't touch this."
  , prim_Ref            = error "repa-series.primitives: you can't touch this."

    -- Arith Int
  , prim_addInt         = (+#)
  , prim_subInt         = (-#)
  , prim_mulInt         = (*#)
  , prim_divInt         = repa_divInt
  , prim_modInt         = repa_modInt
  , prim_remInt         = repa_remInt

    -- Eq Int
  , prim_eqInt          = repa_eqInt
  , prim_neqInt         = repa_neqInt
  , prim_gtInt          = repa_gtInt
  , prim_geInt          = repa_geInt
  , prim_ltInt          = repa_ltInt
  , prim_leInt          = repa_leInt

    -- Ref Int
  , prim_newRefInt      = repa_newRefInt
  , prim_readRefInt     = repa_readRefInt
  , prim_writeRefInt    = repa_writeRefInt

    -- Vector Int
  , prim_newVectorInt   = repa_newVectorInt
  , prim_readVectorInt  = repa_readVectorInt
  , prim_writeVectorInt = repa_writeVectorInt
  , prim_sliceVectorInt = repa_sliceVectorInt

    -- Loop
  , prim_rateOfSeries   = repa_rateOfSeries
  , prim_loop           = repa_loop
  , prim_guard          = repa_guard

    -- Next
  , prim_nextInt        = repa_nextInt
  , prim_nextInt_T2     = repa_nextInt_T2 }


-- Utils ----------------------------------------------------------------------
unwrapIO'  :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unwrapIO' (IO f) = f
{-# INLINE unwrapIO' #-}

unwrapIO_  :: IO a -> State# RealWorld -> State# RealWorld
unwrapIO_ (IO f) world 
 = case f world of
        (# world', _ #) -> world'
{-# INLINE unwrapIO_ #-}


-- Arith Int ------------------------------------------------------------------
repa_divInt i1 i2
 = case div (I# i1) (I# i2) of
        I# i3   -> i3
{-# INLINE repa_divInt #-}

repa_modInt i1 i2
 = case mod (I# i1) (I# i2) of
        I# i3   -> i3
{-# INLINE repa_modInt #-}

repa_remInt i1 i2
 = case rem (I# i1) (I# i2) of
        I# i3   -> i3
{-# INLINE repa_remInt #-}


-- Eq Int ---------------------------------------------------------------------
repa_eqInt i1 i2        = I# i1 == I# i2
{-# INLINE repa_eqInt #-}

repa_neqInt i1 i2       = I# i1 /= I# i2
{-# INLINE repa_neqInt #-}

repa_gtInt i1 i2        = I# i1 >  I# i2
{-# INLINE repa_gtInt #-}

repa_geInt i1 i2        = I# i1 >= I# i2
{-# INLINE repa_geInt #-}

repa_ltInt i1 i2        = I# i1 <  I# i2
{-# INLINE repa_ltInt #-}

repa_leInt i1 i2        = I# i1 <= I# i2
{-# INLINE repa_leInt #-}


-- Ref Int --------------------------------------------------------------------
repa_newRefInt          :: Int# -> World -> (# World, Ref Int #)
repa_newRefInt x
        = unwrapIO' (Ref.new (I# x))
{-# INLINE repa_newRefInt #-}


repa_readRefInt         :: Ref Int -> World -> (# World, Int# #)
repa_readRefInt ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE repa_readRefInt #-}


repa_writeRefInt        :: Ref Int -> Int# -> World -> World
repa_writeRefInt ref val
        = unwrapIO_ (Ref.write ref (I# val))
{-# INLINE repa_writeRefInt #-}


-- Vector Int -----------------------------------------------------------------
repa_newVectorInt       :: Int#  -> World -> (# World, Vector Int #)
repa_newVectorInt len            
        = unwrapIO' (V.new len)
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


-- Series ---------------------------------------------------------------------
-- | Get the Rate / Length of a series.
repa_rateOfSeries :: Series k a -> Int#
repa_rateOfSeries s = seriesLength s
{-# INLINE repa_rateOfSeries #-}


-- | Get the next element of a series.
repa_nextInt    :: Series k Int -> Int# -> World -> (# World, Int# #)
repa_nextInt s ix world
 = case S.index s ix of
        I# i    -> (# world, i #)
{-# INLINE repa_nextInt #-}


-- TODO generalise
repa_nextInt_T2 :: Series k (Int,Int) -> Int# -> World -> (# World, (# Int#, Int# #) #)
repa_nextInt_T2 s ix world
 = case S.index s ix of
        (I# i1, I# i2)    -> (# world, (# i1, i2 #) #)
{-# INLINE repa_nextInt_T2 #-}
