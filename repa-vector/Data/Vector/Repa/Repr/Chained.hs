
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

data Chain a
        = forall s
        . Chain Int                     -- Start position of this chain fragment.
                Int                     -- End   position of this chain fragment.
                s                       -- Startint state.
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


data T2 a b = T2 !a !b


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


-- Zips ----------------------------------------------------------------------
instance Zip D N a b where
 type TZ D N            = N
 vzip !arr1 !arr2       = vzip (chain arr1) arr2
 {-# INLINE [4] vzip #-}

instance Zip N D a b where
 type TZ N D            = N
 vzip !arr1 !arr2       = vzip arr1 (chain arr2)
 {-# INLINE [4] vzip #-}


instance U.Unbox a => Zip U N a b where
 type TZ U N            = N
 vzip !arr1 !arr2       = vzip (chain arr1) arr2
 {-# INLINE [4] vzip #-}

instance U.Unbox b => Zip N U a b where
 type TZ N U            = N
 vzip !arr1 !arr2       = vzip arr1 (chain arr2)
 {-# INLINE [4] vzip #-}


instance Zip N N a b where
 type TZ N N    = N
 vzip (AChained sh1 mkChain1 _) 
      (AChained _   mkChain2 _)
  =    AChained sh1 mkChain (error "vzip no unstream")

  where mkChain c 
         | Chain start1   end1 s10 mkStep1 <- mkChain1 c
         , Chain _start2 _end2 s20 mkStep2 <- mkChain2 c
         = let  
                mkStep ix (T2 s1 s2)
                 | Step s1' x1  <- mkStep1 ix s1
                 , Step s2' x2  <- mkStep2 ix s2
                 = Step (T2 s1' s2') (x1, x2)
                {-# INLINE mkStep #-}

           in   Chain start1 end1 (T2 s10 s20) mkStep
        {-# INLINE mkChain #-}


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
