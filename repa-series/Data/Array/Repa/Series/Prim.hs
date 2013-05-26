
-- | The repa-plugin rewrites client code to use these primitives.
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


-- | Primitives needed by the lowering transform.
data Primitives
  = Primitives
  { prim_Series         :: forall k a. Series k a
  , prim_Vector         :: forall a.   Vector a
  , prim_Ref            :: forall a.   Ref a

    -- Arith Int
  , prim_addInt         :: Int# -> Int# -> Int#
  , prim_subInt         :: Int# -> Int# -> Int#
  , prim_mulInt         :: Int# -> Int# -> Int#
  , prim_divInt         :: Int# -> Int# -> Int#
  , prim_modInt         :: Int# -> Int# -> Int#
  , prim_remInt         :: Int# -> Int# -> Int#

    -- Ref Int
  , prim_newRefInt      :: World   -> (# World, Ref Int #) 
  , prim_readRefInt     :: Ref Int -> World -> (# World, Int# #)
  , prim_writeRefInt    :: Ref Int -> Int#  -> World -> World

    -- Vector Int
  , prim_newIntVector   :: Int# -> World -> (# World, Vector Int #)
  , prim_readIntVector  :: Vector Int -> Int# -> World -> (# World, Int# #)
  , prim_writeIntVector :: Vector Int -> Int# -> Int# -> World -> World

    -- Loop
  , prim_loop           :: Int#  -> (Int# -> World -> World)
                        -> World -> World

  , prim_rateOfSeries   :: forall k a.  Series k a -> Int#

    -- Next
  , prim_nextInt        :: forall k
                        .  Series k Int -> Int#
                        -> World -> (# World, Int# #)
  }


-- | Table of primitives used by the lowering transform.
primitives :: Primitives
primitives
  = Primitives
  { prim_Series         = undefined
  , prim_Vector         = undefined
  , prim_Ref            = undefined

    -- Arith Int
  , prim_addInt         = (+#)
  , prim_subInt         = (-#)
  , prim_mulInt         = (*#)
  , prim_divInt         = repa_divInt
  , prim_modInt         = repa_modInt
  , prim_remInt         = repa_remInt

    -- Ref Int
  , prim_newRefInt      = repa_newRefInt
  , prim_readRefInt     = repa_readRefInt
  , prim_writeRefInt    = repa_writeRefInt

    -- Vector Int
  , prim_newIntVector   = repa_newIntVector
  , prim_readIntVector  = repa_readIntVector
  , prim_writeIntVector = repa_writeIntVector

    -- Loop
  , prim_loop           = repa_loop
  , prim_rateOfSeries   = repa_rateOfSeries
  , prim_nextInt        = repa_nextInt }


-- Primitive Arithmetic -------------------------------------------------------
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


-- Ref Int ---------------------------------------------------------------
repa_newRefInt          :: Int# -> World -> (# World, Ref Int #)
repa_newRefInt x
        = unwrapIO' (Ref.new x)
{-# INLINE repa_newRefInt #-}


repa_readRefInt         :: Ref Int -> World -> (# World, Int# #)
repa_readRefInt ref
 = case Ref.read ref of
        IO f -> \world
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE repa_readRefInt #-}


repa_writeRefInt        :: Ref Int -> Int# -> World -> World


-- Vector Int ------------------------------------------------------------
repa_newIntVector   
        :: Int# 
        -> State# RealWorld -> (# State# RealWorld, Vector Int #)

repa_newIntVector len            
        = unwrapIO' (V.new len)
{-# INLINE repa_newIntVector #-}


repa_readIntVector  
        :: Vector Int -> Int# 
        -> State# RealWorld -> (# State# RealWorld, Int# #)

repa_readIntVector vec ix
 = case V.read vec ix of
        IO f -> \world 
             -> case f world of
                        (# world', I# i #) -> (# world', i #)
{-# INLINE repa_readIntVector #-}


repa_writeIntVector
        :: Vector Int -> Int# -> Int#
        -> State# RealWorld -> State# RealWorld

repa_writeIntVector vec ix val   
        = unwrapIO_ (V.write vec ix (I# val))
{-# INLINE repa_writeIntVector #-}


unwrapIO'  :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unwrapIO' (IO f) = f
{-# INLINE unwrapIO' #-}

unwrapIO_  :: IO a -> State# RealWorld -> State# RealWorld
unwrapIO_ (IO f) world 
 = case f world of
        (# world', _ #) -> world'
{-# INLINE unwrapIO_ #-}


-- Loop combinators -----------------------------------------------------------
-- | Primitive stateful loop combinator.
repa_loop 
        :: Int# 
        -> (Int# -> State# RealWorld -> State# RealWorld)
        -> State# RealWorld 
        -> State# RealWorld

repa_loop len worker world0
 = go 0# world0
 where  
        go ix world
         | ix >=# len           
         = world

         | world' <- worker ix world
         = go (ix +# 1#) world'
{-# INLINE repa_loop #-}


-- Series ---------------------------------------------------------------------
-- | Get the Rate / Length of a series.
repa_rateOfSeries :: Series k a -> Int#
repa_rateOfSeries s = seriesLength s
{-# INLINE repa_rateOfSeries #-}


-- | Get the next element of a series.
repa_nextInt 
        :: Series k Int   
        -> Int# 
        -> State# RealWorld -> (# State# RealWorld, Int# #)

repa_nextInt s ix world
 = case S.index s ix of
        I# i    -> (# world, i #)
{-# INLINE repa_nextInt #-}

