
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

          -- * Array operators
        , repa_newByteArray
        , repa_readIntArray
        , repa_writeIntArray

          -- * Streams
        , repa_rateOfStream
        , repa_nextInt

          -- * Loops
        , repa_loop)
where
import Data.Array.Repa.Series.Stream
import GHC.Exts


-- Primitive Arithmetic -------------------------------------------------------
repa_addInt              = (+#)
{-# INLINE repa_addInt #-}

repa_mulInt              = (*#)
{-# INLINE repa_mulInt #-}


-- Array Operators ------------------------------------------------------------
repa_newByteArray       = newByteArray#
{-# INLINE repa_newByteArray #-}

repa_readIntArray       = readIntArray#
{-# INLINE repa_readIntArray #-}

repa_writeIntArray      = writeIntArray#
{-# INLINE repa_writeIntArray #-}


-- Streams --------------------------------------------------------------------
-- | Get the Rate / Length of a stream.
repa_rateOfStream :: Stream k a -> Int#
repa_rateOfStream s = streamLength s
{-# INLINE repa_rateOfStream #-}


-- | Get the next element of a stream.
repa_nextInt 
        :: Stream k Int   
        -> Int# 
        -> State# RealWorld -> (# State# RealWorld, Int# #)

repa_nextInt s ix world
 = case streamNext s ix of
        I# i    -> (# world, i #)
{-# INLINE repa_nextInt #-}


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

