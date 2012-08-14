

module GrowBuffer 
        ( Buffer
        , new
        , grow
        , length
        , size
        , extend)
where
import Data.IORef
import Control.Monad
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as MV
import Prelude                          hiding (length)

data Buffer a 
        = Buffer
        { bufferContents        :: IORef (MV.IOVector a)
        , bufferTop             :: IORef Int 
        , bufferRatio           :: Float }


dead    = error "GrowBuffer: uninitialised"


-- | Construct a new buffer.
new     :: Int          -- Initial length of buffer.
        -> Float        -- Grow ratio when buffer is full. > 1.
        -> IO (Buffer a)

new length ratio
 = do   mvec    <- MV.replicate length dead
        mvecR   <- newIORef mvec
        topR    <- newIORef 0
        return 
         $ Buffer
                { bufferContents = mvecR
                , bufferRatio    = ratio
                , bufferTop      = topR }


-- | Grow a buffer by the given ratio.
grow    :: Buffer a -> Float -> IO ()
grow buffer ratio
 = do   mvec     <- readIORef (bufferContents buffer)

        let len' = truncate $ (fromIntegral $ MV.length mvec)
                            * bufferRatio buffer

        putStrLn $ "grow to " ++ show len'

        mvec'    <- MV.grow mvec len'
        writeIORef (bufferContents buffer) mvec'


-- | Get the number of elements contained in the buffer.
length  :: Buffer a -> IO Int
length buffer
        = readIORef (bufferTop buffer)


-- | Get the current internal size of the buffer.
size    :: Buffer a -> IO Int
size buffer
        = liftM MV.length $ readIORef $ bufferContents buffer


-- | Extend the buffer with a new element.
extend  :: Buffer a -> a -> IO ()
extend buffer x
 = do   length' <- length buffer
        size'   <- size   buffer

        when (length' == size')
         $ grow buffer (bufferRatio buffer)

        mvec    <- readIORef (bufferContents buffer)
        top     <- readIORef (bufferTop      buffer)
        MV.write mvec top x
        writeIORef (bufferTop buffer) (top + 1)


-- | Freeze the buffer into a vector.
unsafeFreeze  :: Buffer a -> IO (V.Vector a)
unsafeFreeze buffer
 = do   mvec    <- readIORef (bufferContents buffer)
        top     <- readIORef (bufferTop      buffer)
        V.unsafeFreeze (MV.slice 0 top mvec)

