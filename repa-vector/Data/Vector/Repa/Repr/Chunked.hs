
module Data.Vector.Repa.Repr.Chunked
        ( H
        , vsplit

        , Map(..)

          -- * Evaluation
        , vjoinP
        , vjoinUnboxedP
        , joinChunkedP
        , joinSlicedP)
where
import Data.Vector.Repa.Base
import Data.Vector.Repa.Repr.Sliced
import Data.Array.Repa.Eval
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U
import Prelude  hiding (length)
import Prelude                          as P
import GHC.Exts
import System.IO.Unsafe

-- Source ---------------------------------------------------------------------
-- | Chunked arrays are split into several physical chunks that may
--   have different sizes. Indexing is linear in the number of chunks.
--
data H r
instance Source r e => Source (H r) e where
 data Array (H r) sh e
  = VChunked 
        sh                       -- extent.
        Int#                     -- number of chunks.
        (Int# -> Int#)           -- get the starting point of this chunk
                                 -- in the overall linear vector.
        (Int# -> Array r DIM1 e) -- the linear array holding this chunk.

 extent (VChunked shape _ _ _)
  = shape
 {-# INLINE extent #-}

 linearIndex (VChunked _ _ _ chunk) (I# ix)
  = go 0# 0#
  where go !c !start
         = let  !(I# len) = vlength (chunk c)
                !end      = start +# len
           in   if ix <# end 
                 then linearIndex (chunk c) (I# (ix -# start))
                 else go (c +# 1#) end
 {-# INLINE linearIndex #-}


 deepSeqArray (VChunked shape _ _ _) x
  = shape `deepSeq` x


-------------------------------------------------------------------------------
instance Map r e => Map (H r) e where
 type TM (H r) = H (TM r)

 vmap f (VChunked shape chunks getStart getChunk)
  = VChunked shape chunks getStart getChunk'
  where getChunk' chunk = vmap f (getChunk chunk)
        {-# INLINE getChunk' #-}
 {-# INLINE [4] vmap #-}


-- Zips -----------------------------------------------------------------------
--instance Zip2 r     a b 
--      => Zip2 (H r) a b where
-- type TZ2 (H r) = H (TZ2 r)
-- vzip2  (VChunked sh1 cs start1 chunk1) 
--        (VChunked _   _  _      chunk2)
--        = VChunked sh1 cs start1
--        $ \c -> vzip2 (chunk1 c) (chunk2 c)
-- {-# INLINE [4] vzip2 #-}




-------------------------------------------------------------------------------
-- | Split a vector into chunks.
vsplit  :: Source r e => Vector r e -> Vector (H (S r)) e
vsplit vec
 = VChunked (Z :. len') chunks getStart getChunk
 where
        -- Decide now to split the work across the threads.
        -- If the length of the vector doesn't divide evenly among the threads,
        -- then the first few get an extra element.
        !len'           = vlength vec
        !(I# len)       = len'

        !(I# chunks)    = gangSize theGang

        !chunkLen       = len `quotInt#` chunks
        !chunkLeftover  = len `remInt#`  chunks

        getStart c
         | c <# chunkLeftover   = c *# (chunkLen +# 1#)
         | otherwise            = c *# chunkLen  +# chunkLeftover
        {-# NOINLINE getStart #-}

        getChunk c
         = vslice (I# (getStart c)) (I# (getStart (c +# 1#))) vec
        {-# INLINE getChunk #-}
{-# INLINE [4] vsplit #-}


-- Join -----------------------------------------------------------------------
-- | Join a chunked vector.
vjoinP   :: (Source r1 e, Target r2 e)
        => Vector (H (S r1)) e
        -> Vector r2     e

vjoinP vec1
 = vec1 `deepSeqArray`
   unsafePerformIO
 $ do   let !len = vlength vec1
        mvec2   <- newMVec len
        joinSlicedP (unsafeWriteMVec mvec2) vec1
        unsafeFreezeMVec (Z :. len) mvec2
{-# INLINE [4] vjoinP #-}


vjoinUnboxedP 
        :: (U.Unbox e, Source r1 e)
        => Vector (H (S r1)) e
        -> Vector U e

vjoinUnboxedP = vjoinP
{-# INLINE [4] vjoinUnboxedP #-}


-- Join Sliced ----------------------------------------------------------------
joinSlicedP 
        :: Source r a
        => (Int -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> Vector (H (S r)) a
        -> IO ()

joinSlicedP write 
        (VChunked _ _ getStart getChunk)
 = gangIO theGang
 $ \thread' ->
   let  !(I# thread)    = thread'
        !(ASliced sstart sshape sarr) = getChunk thread

        -- Start point in destination.
        !dstStart       = getStart thread
        !dstEnd         = getStart (thread +# 1#)

        -- Start and end point in source
        !(I# srcStart)  = toIndex sshape sstart
        !srcEnd         = srcStart +# (dstEnd -# dstStart)

   in   fill sarr dstStart srcStart srcEnd

 where  fill sarr !ixDst' !ixSrc' ixSrcEnd
         = go ixDst' ixSrc'
         where go ixDst ixSrc
                | ixSrc >=# ixSrcEnd = return ()
                | otherwise
                = do   write (I# ixDst) (unsafeLinearIndex sarr (I# ixSrc))
                       go (ixDst +# 1#) (ixSrc +# 1#)
               {-# INLINE go #-}
        {-# INLINE fill #-}

{-# INLINE [0] joinSlicedP #-}



-- Join Chunked ---------------------------------------------------------------
-- | Compute a chunked array and write elements to a contiguous vector.
--   This version repeatedly calls getStart, so doesn't produce good code.
joinChunkedP
     :: (Int -> a -> IO ())     -- ^ Update function to write into result buffer.
     -> (Int# -> Int#)          -- ^ Fn to get the length of a chunk.
     -> (Int# -> Int#)          -- ^ Fn to get the chunk starting index in the result vector.
     -> (Int# -> Int# -> a)     -- ^ Fn to get the value from a chunk and index.
     -> IO ()

joinChunkedP write getStart getLength getElem
 = gangIO theGang
 $ \thread' -> 
    let !(I# thread) = thread'
        !start  = getStart  thread
        !end    = getLength thread
    in  fill thread start 0# (end -# start)

 where  fill !thread !ixDst !ixSrc !ixEnd
         = let go ixDst' ixSrc'
                | ixSrc' >=# ixEnd = return ()
                | otherwise
                = do    write (I# ixDst') (getElem thread ixSrc')
                        go (ixDst' +# 1#) (ixSrc' +# 1#)
               {-# INLINE go #-}
           in  go ixDst ixSrc
        {-# INLINE fill #-}
{-# INLINE [0] joinChunkedP #-}



