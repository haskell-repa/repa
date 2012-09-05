
module Data.Vector.Repa.Operators.Chains
        ( indexs
		, replicates
		, appendSUP )
where
import Data.Vector.Repa.Repr.Chained

import Data.Vector.Repa.Base
import Data.Array.Repa
import GHC.Exts

-- import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Sequential.USegd (USegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd  (UPSegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd			as USegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd			as UPSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Primitive.DT	as D

import qualified Data.Array.Parallel.Unlifted.Sequential.Vector			as Seq

-- | Replicate each element such that the resulting array such corresponds to
--   the given segment descriptor e.g.:
--     replicates (Segd [1,3,1]) [1,2,3] = [1,2,2,2,3]
replicates
	:: (Source r e)
	=> UPSegd.UPSegd
	-> Vector r e
	-> Vector N e
replicates upsegd vec
 = let	
		-- Total elements
		!(I# len)       = UPSegd.takeElements	 upsegd
		-- Distribute segd into a chunk for each thread to work on
		dists			= UPSegd.takeDistributed upsegd

		-- Function to distribute work over gang,
		-- 'c' is thread number
		getChain c
		 = let  -- Uh oh, this is assuming segd.takeDistributed works the same as nstart...
				start	= I# (nstart len c)
				end		= I# (nstart len (c +# 1#))
				-- Get current chunk of segment. Segment offset describes where segment 0 in segd corrseponds in upsegd
				((segd, seg_off), _)	= D.indexD "replicates" dists (I# c)
				-- State: current segment -1 and remaining 0, so first step will calculate real values
				state					= (segd, seg_off, -1, 0)
		   in   Chain start end state step 
		{-# INLINE getChain #-}

		-- Stepper is called until the entire buffer is filled
		-- Using boxed arithmetic here for clarity, but it won't be hard to change
		step _ (segd, seg_off, seg_cur, remain)
		 = if remain == 0
		   then -- Go to next segment. Don't need to handle end case because caller stops looping
				let
					seg_cur'		= seg_cur + 1
					-- Find out how many copies we need to fill
					(remain', _)	= USegd.getSeg segd seg_cur'
					-- Don't return a value, just update state 
				in  Update	(segd, seg_off, seg_cur', remain')
		   else
			    -- Return a value & decrement the count remaining
				Step	(segd, seg_off, seg_cur, remain - 1)
						(vec `unsafeLinearIndex` (seg_off + seg_cur))
		{-# INLINE step #-}

  in    AChained (ix1 (I# len))
				 getChain
				 (error "replicates: no chain")
{-# INLINE replicates #-}





-- Append ---------------------------------------------------------------------
-- | Segmented append.
appendSUP
        :: (Source r e)
        => UPSegd		-- ^ segment descriptor of result
        -> UPSegd		-- ^ left-hand segd
        -> Vector r e	-- ^ left-hand data
        -> UPSegd		-- ^ right-hand segd
        -> Vector r e	-- ^ right-hand data
        -> Vector N e

appendSUP segd !xd !xs !yd !ys
 = let
		!(I# xlen) = UPSegd.takeElements xd
		!(I# ylen) = UPSegd.takeElements yd
		len 	   = xlen +# ylen

		-- Distribute segd into a chunk for each thread to work on
		dists			= UPSegd.takeDistributed segd

		-- Distribute work over the gang
		getChain c
		 = let  -- Uh oh, this is assuming segd.takeDistributed works the same as nstart...
				start	= I# (nstart len c)
				end		= I# (nstart len (c +# 1#))
				-- Get current chunk of segment. Segment offset describes where segment 0 in segd corrseponds in upsegd
				((segd', seg_off), el_off)	= D.indexD "replicates" dists (I# c)

			in   appendSegS (UPSegd.takeUSegd xd) xs
							(UPSegd.takeUSegd yd) ys
							(USegd.takeElements segd')
							seg_off el_off
							start end

	in	AChained (ix1 (I# len))
			 getChain
			 (error "appendSUP: no chain")
{-# INLINE appendSUP #-}


-- append ---------------------------------------------------------------------
appendSegS
        :: (Source r e)
        => USegd        -- ^ Segment descriptor of first array.
        -> Vector r e   -- ^ Data of first array
        -> USegd        -- ^ Segment descriptor of second array.
        -> Vector r e   -- ^ Data of second array.
        -> Int          -- ^ How many elements to return
        -> Int          -- ^ Segment offset
        -> Int          -- ^ Element offset
		-> Int			-- ^ Chain start
		-> Int			-- ^ Chain end
        -> Chain e

appendSegS !xd !xs !yd !ys !n seg_off el_off start end
  = Chain start end state next
  where
    !xlens = USegd.takeLengths xd
    !ylens = USegd.takeLengths yd

    -- Index into useg data - lengths, indices, etc
    {-# INLINE index1 #-}
    index1  = Seq.index "appendSegS"

	-- index into xd or yd
    {-# INLINE index2 #-}
    index2  = unsafeLinearIndex

    {-# INLINE unbox #-}
    unbox (I# i) = i
    
    state
      -- Nothing to return
      | n == 0 = ASDo
            { as_takefrom = 0#
            , as_seg_off  = 0#
            , as_xs_index = 0#
            , as_ys_index = 0#
            , as_next_swap= 0#
            , as_remain   = 0# }

      -- Start returning data from xs
      | el_off < xlens `index1` seg_off
      = let xi   = (USegd.takeIndices xd `index1` seg_off) + el_off
            yi   =  USegd.takeIndices yd `index1` seg_off
            swap = (USegd.takeLengths xd `index1` seg_off) - el_off
        in  ASDo
            -- start reading from xs, then read from ys at end of this xs segment
            { as_takefrom = 0#
            , as_seg_off  = unbox seg_off
            , as_xs_index = unbox xi
            , as_ys_index = unbox yi
            , as_next_swap= unbox swap
            , as_remain   = unbox n }

      -- Start with ys
      | otherwise
      = let -- NOTE: *not* indicesUSegd xd ! (seg_off+1) since seg_off+1
            -- might be out of bounds
            xi      = (USegd.takeIndices xd `index1` seg_off) + (USegd.takeLengths xd `index1` seg_off)
            el_off' = el_off - USegd.takeLengths xd `index1` seg_off
            yi      = (USegd.takeIndices yd `index1` seg_off) + el_off'
            swap    = (USegd.takeLengths yd `index1` seg_off) - el_off'
        in  ASDo
            { as_takefrom = 1#
            , as_seg_off  = unbox seg_off
            , as_xs_index = unbox xi
            , as_ys_index = unbox yi
            , as_next_swap= unbox swap
            , as_remain   = unbox n }

    {-# INLINE next #-}
	-- ignore end - unchain will take care of that
    --next _ ASDo{as_remain=0#} = return Done

    -- Reading from xs
    next _ s@ASDo{as_takefrom=0#}
      -- Done reading xs, so read the rest of this segment from ys.
      | as_next_swap s  ==# 0#
      = Update (s{as_takefrom=1#, as_next_swap= unbox (ylens `index1` I# (as_seg_off s))})

      -- Grab a value from xs
      | otherwise  =  Step (inc s) (xs `index2` I# (as_xs_index s))

    -- Reading from ys; takefrom nonzero
    next _ s
      -- Done reading ys, so we need to look at the next segment's xs
      | as_next_swap s  ==# 0#
      = let seg' = as_seg_off s +# 1#
        in   Update (s {as_takefrom=0#, as_seg_off=seg', as_next_swap= unbox (xlens `index1` I# seg')})

      -- Grab a value from ys
      | otherwise =  Step (inc s) (ys `index2` I# (as_ys_index s))

    {-# INLINE inc #-}
    -- Move data pointer forward, and decrease remaining and swap
    inc s@ASDo{as_takefrom=0#, as_xs_index=xi, as_next_swap=swap, as_remain=n'}
      = s{as_xs_index=xi +# 1#, as_next_swap=swap -# 1#, as_remain=n' -# 1#}

    -- Takefrom is nonzero: reading from ys
    inc s@ASDo{as_ys_index=yi, as_next_swap=swap, as_remain=n'}
      = s{as_ys_index=yi +# 1#, as_next_swap=swap -# 1#, as_remain=n' -# 1#}
{-# INLINE appendSegS #-}

data AppendState
        = ASDo
        { as_takefrom :: Int#   -- ^ 0 = xs, nonzero = ys
        , as_seg_off  :: Int#   -- ^ current segment
        , as_xs_index :: Int#   -- ^ pointer into xs data
        , as_ys_index :: Int#   -- ^ pointer into ys data
        , as_next_swap:: Int#   -- ^ toggle takefrom in this many elements
        , as_remain   :: Int#   -- ^ how many left
        }




indexs
	:: (Source r1 Int, Source r2 e)
	=> Vector r1 Int
	-> Vector r2 e
	-> Vector N  e
indexs ixs vec
 = let  !(I# len)       = vlength ixs

        getChain c
         = let  start  = I# (nstart len c)
                end    = I# (nstart len (c +# 1#))
           in   Chain start end (0 :: Int) step 
        {-# INLINE getChain #-}

        step ix _
         = Step 0 (vec `unsafeLinearIndex` (ixs `unsafeLinearIndex` (I# ix)))
        {-# INLINE step #-}

  in    AChained (extent ixs) 
                 getChain
                 (error "no chain")
{-# INLINE indexs #-}


