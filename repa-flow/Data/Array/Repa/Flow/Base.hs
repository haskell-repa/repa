
module Data.Array.Repa.Flow.Base
        ( Unbox
        , vindex, uindex
        , unew, uread, uwrite
        , inew, iread, iwrite)
where
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import GHC.Exts


vindex  = (V.!)

uindex  = (U.!)
unew    = UM.unsafeNew
uread   = UM.unsafeRead
uwrite  = UM.unsafeWrite


-- | Allocate a vector of ints.
inew :: Int -> IO (UM.IOVector Int)
inew  len 
        = UM.new len
{-# INLINE inew #-}


-- | Read an unboxed int from a vector.
iread :: UM.IOVector Int -> Int# -> IO Int
iread  vec ix
 = do   !x      <- UM.read vec (I# ix)
        return x
{-# INLINE iread #-}


-- | Write an unboxed into to a vector.
iwrite :: UM.IOVector Int -> Int# -> Int# -> IO ()
iwrite vec ix x 
        = UM.write vec (I# ix) (I# x)
{-# INLINE iwrite #-}

