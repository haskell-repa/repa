module Data.Vector.Repa.Repr.Chain
        ( N
        , Step (..)
        , Frag (..)
        , Array(..)
        , chain
        , unchainP,     unchainUnboxedP
        , unchainS
	, nstart)
where
import Data.Vector.Repa.Base
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Eval.Gang
import GHC.Exts
import System.IO.Unsafe
import qualified Data.Vector.Unboxed            as U
import qualified Data.Array.Repa                as R


-- | A delayed array defined by chain fragments.
--
--      Chains are like streams, except they are logically divided into several
--      fragments when created. When we finally compute them, each fragment
--      can be evaluated in a different thread.
--
--      We also know how many elements will be produced before evaluating it,
--      unlike streams where this is not known. This implies that chains do
--      not support filtering operatons, but means that computed elements can
--      be written directly into the target vector, instead of needing a copying
--      join.
data N

-- | A chain step.
--   This is like the Step type from stream fusion,
--   except there is no Skip constructor.
data Step s a
        = Yield  s a    -- ^ Yield a new state and an element.
        | Update s      -- ^ Update the state.


-- | A chain fragment.
data Frag s a
        = Frag  
        { -- | Start position of this fragment in the result.
          fragStart      :: Int#

          -- | End position of this fragment in the result.
        , fragEnd        :: Int# 

          -- | Starting state.
        , fragStateStart :: s

          -- | Function to produce a chain step.
          --   It takes the index into the result vector, and the current state.
        , fragMkStep     :: Int# -> s -> Step s a
        } 


-- | Chained arrays.
---
--   This contains a lazy cache of the unchained elements, which would be
--   produced if we were to evaluate the whole chain with unchain{P,S}.
--
--   Random access indexing operations can use this to force evaluation 
--   of the chain at this particular point, and re-computing chain prefixes
--   for every element accessed.
--
instance U.Unbox e => Source N e where
 data Array N sh e
        =  forall s r
        .  Source r e
        => AChained
                !sh                  -- Overall extent of chain.
                Int#                -- Number of fragments in chain, 
                                    --   equals number of gang threads.
                (Int# -> Frag s e)  -- Get the fragment for a thread.
                (Vector r e)        -- A LAZY cache of the unchained elements.


 extent (AChained ex _ _ _) 
  = ex
 {-# INLINE extent #-}

 -- Note how we're using the cache here.
 -- The first time we index into the vector all elements will be computed,
 -- but then successive operations will use the same cache.
 linearIndex (AChained _ _ _ vec) ix
  = linearIndex vec ix
 {-# INLINE linearIndex #-}

 deepSeqArray (AChained ex _ _ _) x
  = ex `deepSeq` x
 {-# INLINE deepSeqArray #-}


-- Maps ----------------------------------------------------------------------
instance Map N a where
 type TM N   = N

 vmap f (AChained sh1 frags mkFrag vec)
  = AChained sh1 frags mkFrag' (R.map f vec)

  where mkFrag' c
         | Frag start end s0 mkStep <- mkFrag c
         = let
                mkStep' ix s
                 = case mkStep ix s of
                        Yield  s' x -> Yield s' (f x)
                        Update s'   -> Update s'
                {-# INLINE mkStep' #-}

           in Frag start end s0 mkStep'
        {-# INLINE mkFrag' #-}
 {-# INLINE vmap #-}


-- nstart ---------------------------------------------------------------------
-- | Get the starting point for a chain fragment.
-- 
nstart  :: Int#         -- ^ Total length of result.
        -> Int#         -- ^ Fragment (thread) number
        -> Int#         -- ^ Starting point of this fragment.

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


-- chain ----------------------------------------------------------------------
-- | Convert an arbitrary vector to a chain.
-- 
--   Demainding elements of the chain will read them directly from the 
--   source vector.
--
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
                vec
{-# INLINE chain #-}


-- unchain --------------------------------------------------------------------
-- | Convert a chain to a manifest vector representation, in parallel.
--
--   Each chain fragment is evaluated on its own thread, and the elements
--   are written directly to the target vector with no intermediate copying.
--
--   For the chained vector representation, this function should produce 
--   better code than using `computeP`.
--
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
                         -> do  _ <- write (I# ix) x
				fill (ix +# 1#) s'

			Update s' 
                         ->     fill ix s'
        {-# INLINE [0] fillChunk #-}
{-# INLINE unchainP #-}


-- | Like `unchainP` but with a more specific type.
unchainUnboxedP :: U.Unbox e => Vector N e -> Vector U e
unchainUnboxedP = unchainP
{-# INLINE unchainUnboxedP #-}


-- | Convert a chain to a manifest vector representation,  sequentially.
--
--   Elements are written directly to the target vector, 
--   with no intermediate copying.
--
--   For the chained vector representation, this function should produce
--   better code than using `computeS`.
--
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
                         -> do  _ <- write (I# ix) x
                                fillFrag' (ix +# 1#) s'

                        Update s'
                         ->     fillFrag' ix s'
        {-# INLINE [0] fillFrag #-}
{-# INLINE [1] unchainS #-}
