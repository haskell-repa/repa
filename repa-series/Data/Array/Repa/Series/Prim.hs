
-- | The repa-plugin rewrites client code to use these primitives.
--
--   The plugin will use whatever names are in scope, so if you want to debug
--   your code you can import a different implementation of these primitives
--   into the module to be vectorized.
--
module Data.Array.Repa.Series.Prim
        ( -- * Primitive arithmetic
          repa_addInt
        , repa_mulInt

          -- * Vector operations.
        , repa_newIntVector
        , repa_readIntVector
        , repa_writeIntVector

          -- * Loops
        , repa_loop

          -- * Series
        , repa_rateOfSeries
        , repa_nextInt)
where
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import GHC.Exts
import GHC.Types


-- Primitive Arithmetic -------------------------------------------------------
repa_addInt              = (+#)
{-# INLINE repa_addInt #-}

repa_mulInt              = (*#)
{-# INLINE repa_mulInt #-}


-- Vector Operators ------------------------------------------------------------
repa_newIntVector   :: Int# 
                    -> State# RealWorld -> (# State# RealWorld, Vector Int #)
repa_newIntVector len            = unwrapIO' (V.new len)
{-# INLINE repa_newIntVector #-}

repa_readIntVector  :: Vector Int -> Int# 
                    -> State# RealWorld -> (# State# RealWorld, Int #)
repa_readIntVector vec ix        = unwrapIO' (V.read vec ix)
{-# INLINE repa_readIntVector #-}

repa_writeIntVector :: Vector Int -> Int# -> Int 
                    -> State# RealWorld -> State# RealWorld
repa_writeIntVector vec ix val   = unwrapIO_ (V.write vec ix val)
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

