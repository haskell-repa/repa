
module Data.Array.Repa.Flow.Base
        ( Unbox
        , vnew, vfreeze, vindex, vwrite
        , unew, uread, uwrite, uslice, ufreeze, uindex
        , inew, iread, iwrite)
where
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector                    as V
import qualified Data.Vector.Mutable            as VM
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import GHC.Exts


vindex  = (V.!)
vnew    = VM.new
vfreeze = V.unsafeFreeze

vwrite str vec ix val
 | ix >= VM.length vec  = error $ "vwrite: " ++ str
 | otherwise            = VM.write vec ix val

uindex str vec ix
 | ix >= U.length vec   = error $ "uindex: " ++ str
 | otherwise            = U.unsafeIndex vec ix

unew    = UM.new
uread   = UM.read

uwrite str vec ix val
 | ix >= UM.length vec  = error $ "uwrite: " ++ str
 | otherwise            = UM.write vec ix val

uslice  = U.unsafeSlice
ufreeze = U.unsafeFreeze


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

