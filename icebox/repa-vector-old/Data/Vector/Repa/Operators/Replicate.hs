
module Data.Vector.Repa.Operators.Replicate
        ( vreplicate
        , vreplicates)
where
import Data.Vector.Repa.Repr.Chain
import Data.Vector.Repa.Base
import Data.Array.Repa
import Data.Array.Parallel.Unlifted.Parallel.UPSegd                     (UPSegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Primitive.DT  as D
import qualified Data.Vector.Unboxed                                    as U
import GHC.Exts


-- | Replicate a single element several times.
vreplicate :: Int -> e -> Vector D e
vreplicate !n !x
 = ADelayed (Z :. n) get
 where  get _ = x
        {-# INLINE get #-}
{-# INLINE [4] vreplicate #-}


-- | Replicate each element such that the resulting array such corresponds to
--   the given segment descriptor.
vreplicates
        :: U.Unbox e
        => Source r e
        => UPSegd
        -> Vector r e
        -> Vector N e

vreplicates !upsegd !vec
 = let  
        -- Total elements
        !(I# len) = UPSegd.takeElements  upsegd
        
        -- Distribute segd into a chunk for each thread to work on
        !dists    = UPSegd.takeDistributed upsegd

        -- Number of chunks in the distributed segment descriptor.
        !(I# chunks)    = D.sizeD dists

        -- Function to distribute work over gang,
        -- 'c' is thread number
        getFrag !c
         = Frag start end state0 step
         where  -- TODO: Uh oh, this is assuming segd.takeDistributed works
                --              the same as nstart...
                --              We should use the offsets from takeDistributed instead.
                !start  = nstart len c
                !end    = nstart len (c +# 1#)
        
                -- Get current chunk of segment.
                -- Segment offset describes where segment 0 in segd corrseponds in upsegd
                !((!segd, I# seg_off0), _) 
                        = D.indexD "replicates" dists (I# c)
        
                -- State: current segment -1 and remaining 0,
                -- so first step will calculate real values
                !state0 = RepState seg_off0 (-1#) 0#

                -- Inner step function
                --  This is produces the actual elements.
                step _ (RepState seg_off seg_cur remain)
                 -- Go to next segment.
                 -- Don't need to handle end case because caller stops looping.
                 | remain ==# 0#
                 = let  
                        -- Advance to the next segment.
                        !seg_cur'        = seg_cur +# 1#

                        -- Get the length of the current segment.
                        -- This tells us how many copies of the current value to write.
                        !(I# remain', _) = USegd.getSeg segd (I# seg_cur')

                   in   Update $ RepState seg_off seg_cur' remain'

                 -- Return a value and decrement the count remaining   
                 | otherwise
                 = let  !state' = RepState seg_off seg_cur (remain -# 1#)
                        !x      = vec `unsafeLinearIndex` (I# (seg_off +# seg_cur))
                   in Yield state' x
                {-# INLINE [1] step #-}
        {-# INLINE [2] getFrag #-}

        -- A version of the result without the suspended cache.
        vecCache = AChained 
                        (ix1 (I# len))
                        chunks
                        getFrag
                        (error "replicates: no chain" :: Vector D e)
  in    AChained 
                (ix1 (I# len))
                chunks 
                getFrag 
                (unchainUnboxedP vecCache)
{-# INLINE vreplicates #-}

data RepState
        = RepState 
        { _stateSegOff    :: Int# 
        , _stateCurSeg    :: Int#
        , _stateRemaining :: Int# }

