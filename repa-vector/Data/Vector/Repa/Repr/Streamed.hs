
module Data.Vector.Repa.Repr.Streamed
where
import Data.Vector.Repa.Base
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Eval.Gang
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Fusion.Stream              as V
import qualified Data.Vector.Fusion.Stream.Monadic      as VM
import qualified Data.Vector.Fusion.Stream.Size         as V
import GHC.Exts
import System.IO.Unsafe

data R 


-- Multistreams, 
-- We know the total length of the stream.
instance U.Unbox e => Source R e where
 data Array R sh e
        = AStreamed
                sh                              -- Overall extent of stream
                (Int# -> (Int, V.Stream e))    -- Starting point and stream for each thread.
                (Vector U e)                    -- Cache of unstreamed elements.

 extent (AStreamed ex _ _) 
  = ex
 {-# INLINE extent #-}

 deepSeqArray (AStreamed ex _ _) x
  = ex `deepSeq` x
 {-# INLINE deepSeqArray #-}


-- Get the starting point for a chunk.
sstart  :: Int# -> Int# -> Int#
sstart len c
 = let  !(I# chunks)    = gangSize theGang
        chunkLen        = len `quotInt#` chunks
        chunkLeftover   = len `remInt#`  chunks

        getStart c'
         | c' <# chunkLeftover = c' *# (chunkLen +# 1#)
         | otherwise           = c' *# chunkLen  +# chunkLeftover
        {-# NOINLINE getStart #-}

  in    getStart c
{-# NOINLINE sstart #-}


-- | Convert an arbitrary vector to a stream.
stream :: Source r e => Vector r e -> Vector R e
stream vec
 = let  !(I# len)       = vlength vec

        getStream c
         = let  !start  = sstart len c
                !end    = sstart len (c +# 1#)
           in   ( I# start
                , VM.Stream step (I# start, I# end) 
                                 (V.Exact  (I# (end -# start))))
        {-# INLINE getStream #-}

        step (ix, end)
         | ix >= end    = return $ V.Done
         | otherwise    = return $ V.Yield (vec `unsafeLinearIndex` ix) 
                                           (ix + 1, end)
        {-# INLINE step #-}

  in    AStreamed (extent vec) 
                getStream 
                (error "no unstream")
{-# INLINE stream #-}


-- | Convert a stream back to a vector.
unstream :: Target r e => Vector R e -> Vector r e
unstream (AStreamed sh getStream _)
 = unsafePerformIO
 $ do   let (Z :. len)  =  sh
        mvec2           <- newMVec len
        fillChunks mvec2 
        unsafeFreezeMVec (Z :. len) mvec2

 where  fillChunks mvec
         = gangIO theGang 
         $ \(I# thread) 
                -> let  (start', ss)    = getStream thread
                        !(I# start)     = start'
                   in   fillChunk mvec start (V.liftStream ss)
        {-# INLINE fillChunks #-}

        fillChunk mvec start (VM.Stream mkStep s0 _)
         = fill start s0
         where  fill !ix s
                 = do   step1   <- mkStep s
                        case step1 of
                         VM.Yield x1 s' 
                          -> do unsafeWriteMVec mvec (I# ix) x1
                                fill (ix +# 1#) s'

                         VM.Skip s' -> fill ix s'
                         VM.Done    -> return ()
        {-# INLINE fillChunk #-}

{-# INLINE unstream #-}
