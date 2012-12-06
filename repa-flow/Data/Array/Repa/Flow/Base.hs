
module Data.Array.Repa.Flow.Base
        ( Unbox
        , vindex, vfreeze, vnew, vread, vwrite
        , uindex, ufreeze, unew, uread, uwrite, uslice
        , inew, iread, iwrite)
where
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector                    as V
import qualified Data.Vector.Mutable            as VM
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import GHC.Exts
import System.IO
import System.IO.Unsafe

-------------------------------------------------------------------------------
debug   = False
{-# INLINE debug #-}

checkIx :: String -> Int -> Int -> a -> a
checkIx  str len ix a
 | debug        = checkIx' str len ix a
 | otherwise    = a
{-# INLINE checkIx #-}

checkIx' str len ix a
 | ix >= len
 = unsafePerformIO
 $ do   hFlush stdout
        hFlush stderr
        error $ unlines
                [ "repa-flow.checkIx index out of range in '" ++ str ++ "'"
                , "    length = " ++ show len
                , "    index  = " ++ show ix ]

 | otherwise
 = a
{-# NOINLINE checkIx' #-}


-- Boxed ----------------------------------------------------------------------
vnew    = VM.new
{-# INLINE vnew #-}

vfreeze = V.unsafeFreeze
{-# INLINE vfreeze #-}

vindex str vec ix
 = checkIx str (V.length vec) ix
 $ V.unsafeIndex vec ix
{-# INLINE vindex #-}

vread str vec ix
 = checkIx str (VM.length vec) ix
 $ VM.unsafeRead vec ix
{-# INLINE vread #-}

vwrite  str vec ix val  
 = checkIx str (VM.length vec) ix 
 $ VM.unsafeWrite vec ix val
{-# INLINE vwrite #-}


-- Unboxed --------------------------------------------------------------------
unew    = UM.new
{-# INLINE unew #-}

ufreeze = U.unsafeFreeze
{-# INLINE ufreeze #-}

uindex  str vec ix
 = checkIx str (U.length vec) ix
 $ U.unsafeIndex vec ix
{-# INLINE uindex #-}

uread str vec ix
 = checkIx str (UM.length vec) ix
 $ UM.unsafeRead vec ix
{-# INLINE uread #-}

uwrite str vec ix val  
 = checkIx str (UM.length vec) ix
 $ UM.unsafeWrite vec ix val
{-# INLINE uwrite #-}

uslice  = U.unsafeSlice
{-# INLINE uslice #-}


-- Unboxed Integer ------------------------------------------------------------
inew  len 
        = UM.new len
{-# INLINE inew #-}

iread :: String -> UM.IOVector Int -> Int# -> IO Int
iread str vec ix
 = checkIx str (UM.length vec) (I# ix)
 $ do   !x      <- UM.unsafeRead vec (I# ix)
        return x
{-# INLINE iread #-}

iwrite :: String -> UM.IOVector Int -> Int# -> Int# -> IO ()
iwrite str vec ix x 
 = checkIx str (UM.length vec) (I# ix)
 $ UM.unsafeWrite vec (I# ix) (I# x)
{-# INLINE iwrite #-}

