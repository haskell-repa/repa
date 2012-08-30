
module Data.Vector.Repa.Repr.Chained
        ( N
        , Step (..)
        , Chain(..)
        , Array(..)
        , chain
        , unchainP)
where
import Data.Vector.Repa.Base
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Eval.Gang
import GHC.Exts
import System.IO.Unsafe
import qualified Data.Vector.Unboxed                    as U


-- Multichains.
--   As opposed to streams:
--      Chains are always in 'distributed' form, ready for parallel evaluation.
--      We know the exact number of elements that will be yielded.
--      Chains can't skip elements, so no filtering operations are supported.
--      A chain exposes its internal element counter.
--      Locked zip-with operations are more efficent with chains than streams,
--       because we only need to keep one loop counter for all zipped chains.
--   
data N


-- | A new seed and an element.
data Step s a
        = Step s a
        | Update s 

data Chain a
        = forall s
        . Chain Int                     -- Start position of this chain fragment.
                Int                     -- End   position of this chain fragment.
                s                       -- Starting state.
                (Int# -> s -> Step s a) -- Function to produce a new element.


instance U.Unbox e => Source N e where
 data Array N sh e
        = AChained
                sh                      -- Overall extent of chain.
                (Int# -> Chain e)       -- Chain for each thread.
                (Vector U e)            -- Cache of unchained elements.

 extent (AChained ex _ _) 
  = ex
 {-# INLINE extent #-}

 linearIndex _
  = error "linearIndex: not finished"

 deepSeqArray (AChained ex _ _) x
  = ex `deepSeq` x
 {-# INLINE deepSeqArray #-}


-- Maps ----------------------------------------------------------------------
instance Map N a where
 type TM N   = N

 vmap f (AChained sh1 mkChain _)
  = AChained sh1 mkChain' (error "vmap no unstream")
  where mkChain' c
         | Chain start end s0 mkStep <- mkChain c
         = let
                mkStep' ix s
                 | Step s' x    <- mkStep ix s
                 = Step s' (f x)
                {-# INLINE mkStep' #-}

           in Chain start end s0 mkStep'
        {-# INLINE mkChain' #-}
 {-# INLINE vmap #-}


-- nstart ----------------------------------------------------------------------
-- Get the starting point for a chunk.
nstart  :: Int# -> Int# -> Int#
nstart len c
 = let  !(I# chunks)    = gangSize theGang
        chunkLen        = len `quotInt#` chunks
        chunkLeftover   = len `remInt#`  chunks

        getStart c'
         | c' <# chunkLeftover = c' *# (chunkLen +# 1#)
         | otherwise           = c' *# chunkLen  +# chunkLeftover
        {-# NOINLINE getStart #-}

  in    getStart c
{-# NOINLINE nstart #-}


-- | Convert an arbitrary vector to a chain.
chain :: Source r e => Vector r e -> Vector N e
chain vec
 = let  !(I# len)       = vlength vec

        getChain c
         = let  start  = I# (nstart len c)
                end    = I# (nstart len (c +# 1#))
           in   Chain start end (0 :: Int) step 
        {-# INLINE getChain #-}

        step ix _
         = Step 0 (vec `unsafeLinearIndex` (I# ix))
        {-# INLINE step #-}

  in    AChained (extent vec) 
                 getChain
                 (error "no chain")
{-# INLINE chain #-}

--data SPEC = SPEC
-- ANN type SPEC ForceSpecConstr #

-- | Convert a chain back to a vector.
unchainP :: Target r e => Vector N e -> Vector r e
unchainP (AChained sh getChain _)
 = unsafePerformIO
 $ do   let (Z :. len)  =  sh
        mvec2           <- newMVec len
        fillChunks mvec2 
        unsafeFreezeMVec (Z :. len) mvec2

 where  fillChunks mvec
         = gangIO theGang 
         $ \(I# thread) -> fillChunk mvec (getChain thread)
        {-# INLINE fillChunks #-}

        fillChunk mvec (Chain (I# start) (I# end) s0 mkStep)
         = fill start s0
         where  fill !ix !s 
                 | ix >=# end = return ()
                 | Step s' x    <- mkStep ix s
                 = do unsafeWriteMVec mvec (I# ix) x
                      fill (ix +# 1#) s'
                {-# INLINE fill #-}
        {-# INLINE fillChunk #-}
{-# INLINE unchainP #-}
