module Data.Vector.Repa.Repr.Chained
        ( N
        , Step (..)
        , Frag (..)
        , Array(..)
        , chain
        , unchainP
        , unchainS
	, nstart)
where
import Data.Vector.Repa.Base
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Eval.Gang
import GHC.Exts
import System.IO.Unsafe
import Control.Monad.ST
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
        = Yield  s a
        | Update s 


data Frag s a
        = Frag  Int#                    -- Start position of this chain fragment.
                Int#                    -- End   position of this chain fragment.
                s                       -- Starting state.
                (Int# -> s -> Step s a) -- Function to produce a new element.


instance U.Unbox e => Source N e where
 data Array N sh e
        = forall s
        . AChained
                sh                      -- Overall extent of chain.
                Int#                    -- Number of fragments in chain
                (Int# -> Frag s e)      -- Fragment for each thread.
                (Vector U e)            -- Cache of unchained elements.


 extent (AChained ex _ _ _) 
  = ex
 {-# INLINE extent #-}

 linearIndex _
  = error "linearIndex: not finished"

 deepSeqArray (AChained ex _ _ _) x
  = ex `deepSeq` x
 {-# INLINE deepSeqArray #-}


-- Maps ----------------------------------------------------------------------
instance Map N a where
 type TM N   = N

 vmap f (AChained sh1 frags mkFrag _)
  = AChained sh1 frags mkFrag' (error "vmap no unstream")

  where mkFrag' c
         | Frag start end s0 mkStep <- mkFrag c
         = let
                mkStep' ix s
                 | Yield s' x    <- mkStep ix s
                 = Yield s' (f x)

                 | otherwise
                 = error "vmap: broken, got an update"
                {-# INLINE mkStep' #-}

           in Frag start end s0 mkStep'
        {-# INLINE mkFrag' #-}
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
        !(I# frags)     = gangSize theGang
        getFrag c
         = let  start  = nstart len c
                end    = nstart len (c +# 1#)
           in   Frag start end (0 :: Int) step 
        {-# INLINE getFrag #-}

        step ix _
         = Yield 0 (vec `unsafeLinearIndex` (I# ix))
        {-# INLINE step #-}

  in    AChained (extent vec) 
                frags
                getFrag
                (error "no chain")
{-# INLINE chain #-}


-- | Convert a chain to a manifest vector representation,
--   in parallel.
unchainP :: Target r e => Vector N e -> Vector r e
unchainP (AChained sh _ getChain _)
 = unsafePerformIO
 $ do   let (Z :. len)  =  sh
        mvec           <- newMVec len
        fillChunks (unsafeWriteMVec mvec)
        unsafeFreezeMVec (Z :. len) mvec

 where  fillChunks write
         = gangIO theGang 
         $ \(I# thread) -> fillChunk write (getChain thread)
        {-# INLINE [0] fillChunks #-}

        fillChunk write (Frag start end s0 mkStep)
         = fill start s0

         where  fill !ix !s 
                 | ix >=# end = return ()

		 | otherwise
		 = case mkStep ix s of
			Yield s' x 
                         -> do  write (I# ix) x
				fill (ix +# 1#) s'

			Update s' 
                         ->     fill ix s'
        {-# INLINE [0] fillChunk #-}
{-# INLINE unchainP #-}


-- | Convert a chain to a manifest vector representation, 
--   sequentially.
unchainS :: Target r a => Vector N a -> Vector r a
unchainS (AChained sh frags mkFrag _)
 = unsafePerformIO
 $ do   let (Z :. len)  = sh
        mvec            <- newMVec len
        fillFrags (unsafeWriteMVec mvec)
        unsafeFreezeMVec (Z :. len) mvec

 where  fillFrags write 
         = fillFrags' 0#
         where  
                fillFrags' c
                 | c >=# frags
                 = return ()

                 | otherwise
                 = do   fillFrag   write c (mkFrag c)
                        fillFrags' (c +# 1#)
        {-# INLINE [0] fillFrags #-}

        fillFrag write c (Frag start end s0 mkStep)
         = fillFrag' start s0
         where  
                fillFrag' !ix !s
                 | ix >=# end
                 = return ()

                 | otherwise
                 = case mkStep c s of
                        Yield  s' x
                         -> do  write (I# ix) x
                                fillFrag' (ix +# 1#) s'

                        Update s'
                         ->     fillFrag' ix s'
        {-# INLINE [0] fillFrag #-}
{-# INLINE [1] unchainS #-}
