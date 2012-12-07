
module Data.Array.Repa.Vector.Compute
        ( -- * Target Representations
          Target        (..)

          -- * Loading Array Elements
        , Load          (..)
        , LoadRange     (..)

          -- * Computation
        , Compute       (..)
        , computeP
        , computeS

          -- * Conversions
        , fromList)
where
import Data.Array.Repa.Vector.Compute.Target
import Data.Array.Repa.Vector.Compute.Load
import Data.Array.Repa.Vector.Base
import System.IO.Unsafe


class Compute r1 sh a where
 -- | Parallel computation of array elements.
 computeIOP 
        :: Target r2 a
        => Array r1 sh a -> IO (Array r2 sh a)

 -- | Sequential computation of array elements.
 computeIOS
        :: Target r2 a
        => Array r1 sh a -> IO (Array r2 sh a)


-- | Pure parallel computation of array elements.
--
--   NOTE: 
--   You must strictly demand the result array before computing another one,
--   so that the call to `computeP` is not suspended due to lazy evaluation.
--   If lazy evaluation causes a `computeP` to start while another is already
--   running then you will get a warning on the console and the computation
--   will run sequentially. Repa does not support nested parallelism, 
--   where multiple parallel computations run at the same time.
--
--   Use the following coding style, with bang patterns on each intermediate
--   array binding.
--  
--   @
--    let  !arr1 = computeP ...
--         !arr2 = computeP ... arr1 ...
--    in   arr2
--    @
--
computeP 
        :: Compute r1 sh a
        => Target r2 a
        => Array r1 sh a -> Array r2 sh a

computeP !arr
 = unsafePerformIO $ computeIOP arr
{-# INLINE [4] computeP #-}


-- | Pure sequential computation of array elements.
computeS
        :: Compute r1 sh a
        => Target r2 a
        => Array r1 sh a -> Array r2 sh a

computeS !arr
 = unsafePerformIO $ computeIOS arr
{-# INLINE [4] computeS #-}

