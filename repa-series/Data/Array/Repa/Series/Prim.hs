
-- | The repa-plugin rewrites client code to use these primitives.
--
--   The plugin will use whatever names are in scope, so if you want to debug
--   your code you can import a different implementation of these primitives
--   into the module to be vectorized.
--
module Data.Array.Repa.Series.Prim
        ( -- * Primitive arithmetic
          primAddInt
        , primMulInt

          -- * Array operators
        , primNewByteArray
        , primReadIntArray
        , primWriteIntArray

          -- * Streams
        , primRateOfStream
        , primNextInt

          -- * Loops
        , primLoop)
where
import Data.Array.Repa.Series.Stream
import GHC.Exts


-- Primitive Arithmetic -------------------------------------------------------
primAddInt              = (+#)
{-# INLINE primAddInt #-}

primMulInt              = (*#)
{-# INLINE primMulInt #-}


-- Array Operators ------------------------------------------------------------
primNewByteArray        = newByteArray#
{-# INLINE primNewByteArray #-}

primReadIntArray        = readIntArray#
{-# INLINE primReadIntArray #-}

primWriteIntArray       = writeIntArray#
{-# INLINE primWriteIntArray #-}


-- Streams --------------------------------------------------------------------
-- | Get the Rate / Length of a stream.
primRateOfStream :: Stream k a -> Int#
primRateOfStream s = streamLength s
{-# INLINE primRateOfStream #-}


-- | Get the next element of a stream.
primNextInt 
        :: Stream k Int   
        -> Int# 
        -> State# RealWorld -> (# State# RealWorld, Int# #)

primNextInt s ix world
 = case streamNext s ix of
        I# i    -> (# world, i #)
{-# INLINE primNextInt #-}


-- Loop combinators -----------------------------------------------------------
-- | Primitive stateful loop combinator.
primLoop 
        :: Int# 
        -> (Int# -> State# RealWorld -> State# RealWorld)
        -> State# RealWorld 
        -> State# RealWorld

primLoop len worker world0
 = go 0# world0
 where  
        go ix world
         | ix >=# len           
         = world

         | world' <- worker ix world
         = go (ix +# 1#) world'
{-# INLINE primLoop #-}

