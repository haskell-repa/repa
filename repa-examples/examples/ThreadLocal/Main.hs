
-- | Example of how to use one of Repa's low level array filling functions
--   with a thread local state (or other IO action).
--
import Data.Array.Repa.Eval
import Data.Vector.Unboxed              as V
import Data.Vector.Unboxed.Mutable      as VM
import Data.IORef

main
 = do   vec     <- doIt 100
        print vec

-- | Generate a vector of the given length.
{-# NOINLINE doIt #-}
doIt :: Int -> IO (Vector (Int, Int))
doIt len
 = do   mvec    <- VM.new len

        fillChunkedIOP
                len 
                (VM.unsafeWrite mvec)
                mkGetElem

        V.unsafeFreeze mvec


-- | Make a function to compute each element of the array.
--   In the first IO action you can do some per-thread initialisation.
{-# INLINE mkGetElem #-}
mkGetElem :: Int -> IO (Int -> IO (Int, Int))
mkGetElem threadId
 = newIORef (threadId * 1000) >>= \ref
 -> let 
        -- The per-thread element function can use the per-thread
        --  value stored in the IORef in its closure.

        -- Write this as a separate binding with an INLINE pragma to
        --   ensure it is inlined into the unfolding of 'fillChunkedIOP'.
        {-# INLINE getElem #-}
        getElem :: Int -> IO (Int, Int)
        getElem ix
         = do   z'       <- readIORef ref
                writeIORef ref (z' + 1)
                return (z', ix)

    in  return getElem