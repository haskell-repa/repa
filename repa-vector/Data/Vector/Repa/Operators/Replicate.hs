
module Data.Vector.Repa.Operators.Replicate
        ( vreplicate
        , vreplicates)
where
import Data.Vector.Repa.Repr.Chained
import Data.Vector.Repa.Base
import Data.Array.Repa
import Data.Array.Parallel.Unlifted.Parallel.UPSegd                     (UPSegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Primitive.DT  as D
import GHC.Exts


-- | Replicate a single element several times.
vreplicate :: Int -> e -> Vector D e
vreplicate !n !x
 = ADelayed (Z :. n) get
 where  get _ = x
        {-# INLINE get #-}
{-# INLINE [4] vreplicate #-}


-- | Replicate each element such that the resulting array such corresponds to
--   the given segment descriptor e.g.:
--     replicates (Segd [1,3,1]) [1,2,3] = [1,2,2,2,3]
vreplicates
        :: Source r e
        => UPSegd
        -> Vector r e
        -> Vector N e

vreplicates upsegd vec
 = let  -- Total elements
        !(I# len) = UPSegd.takeElements  upsegd
        
        -- Distribute segd into a chunk for each thread to work on
        !dists    = UPSegd.takeDistributed upsegd

        -- Function to distribute work over gang,
        -- 'c' is thread number
        getChain !c
         = let  -- TODO: Uh oh, this is assuming segd.takeDistributed works
                --              the same as nstart...
                --              We should use the offsets from takeDistributed instead.
                !start  = I# (nstart len c)
                !end    = I# (nstart len (c +# 1#))
        
                -- Get current chunk of segment.
                -- Segment offset describes where segment 0 in segd corrseponds in upsegd
                ((segd, seg_off), _)    = D.indexD "replicates" dists (I# c)
        
                -- State: current segment -1 and remaining 0,
                -- so first step will calculate real values
                !state  = (segd, seg_off, -1, 0)

           in   Chain start end state step 
        {-# INLINE getChain #-}


        -- Stepper is called until the entire buffer is filled
        -- Using boxed arithmetic here for clarity, but it won't be hard to change
        step _ (segd, seg_off, seg_cur, remain)
         = segd `seq` seg_off `seq` seg_cur `seq` remain `seq`
         -- Go to next segment.
         -- Don't need to handle end case because caller stops looping      
           if remain == 0
            then 
             let !seg_cur'       = seg_cur + 1

                 -- Find out how many copies we need to fill.
                 (remain', _)    = USegd.getSeg segd seg_cur'

                 -- Don't return a value, just update state .
             in  Update (segd, seg_off, seg_cur', remain')

            -- Return a value and decrement the count remaining   
            else 
             Step (segd, seg_off, seg_cur, remain - 1)
                  (vec `unsafeLinearIndex` (seg_off + seg_cur))
        {-# INLINE step #-}

  in    AChained (ix1 (I# len))
                getChain
                (error "replicates: no chain")
{-# INLINE vreplicates #-}

