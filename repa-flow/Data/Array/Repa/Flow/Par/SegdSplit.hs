
module Data.Array.Repa.Flow.Par.SegdSplit
        (splitSegsOnElems)
where
import Data.Array.Repa.Flow.Par.Distro
import Control.Monad.ST
import Control.Monad
import Data.Bits                                (shiftR)
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import GHC.Exts

---------------------------------------------------------------------------------------------------
-- | Split a segment descriptor across the gang, element wise.
--   We try to put the same number of elements on each thread, which means
--   that segments are sometimes split across threads.
--
--   Each thread gets a slice of segment descriptor, the segid of the first 
--   slice, and the offset of the first slice in its segment.
--   
--   Example:
--    In this picture each X represents 5 elements, and we have 5 segements in total.
--
-- @  segs:    ----------------------- --- ------- --------------- -------------------
--    elems:  |X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|
--            |     thread1     |     thread2     |     thread3     |     thread4     |
--    segid:  0                 0                 3                 4
--    offset: 0                 45                0                 5
--
--    pprp $ splitSegdOnElemsD theGang4
--          $ lengthsToUSegd $ fromList [60, 10, 20, 40, 50 :: Int]
--
--     segd:    DUSegd lengths:  DVector lengths: [1,3,2,1]
--                                        chunks:  [[45],[15,10,20],[40,5],[45]]
--                     indices:  DVector lengths: [1,3,2,1]
--                                        chunks:  [[0], [0,15,25], [0,40],[0]]
--                    elements:  DInt [45,45,45,45]
--
--     segids: DInt [0,0,3,4]     (segment id of first slice on thread)
--    offsets: DInt [0,45,0,5]    (offset of that slice in its segment)
-- @
--
splitSegsOnElems 
        :: U.Vector Int                 -- ^ Segment lengths.
        -> U.Vector Int                 -- ^ Segment starting indices.
        -> Int                          -- ^ Total number of elements.
        -> Int                          -- ^ Total number of threads.
        -> Int                          -- ^ Get the chunk for this thread
        -> (U.Vector Int, Int, Int, Int)
                -- ^ Segment lengths for this thread,
                --   Total number of elements on this thread,
                --   Id of first segment on the thread,
                --   Offset into the first segment.
splitSegsOnElems lens idxs (I# nElemsTotal) (I# nThreads) (I# ixThread)
 = let  
        -- Starting element number for this thread
        !ixStart  = balancedFragStart nElemsTotal nThreads ixThread

        -- Starting element number for the next thread.
        !ixStart' = balancedFragStart nElemsTotal nThreads (ixThread +# 1#)

        -- Number of elements assigned to this thread.
        !nElems   = ixStart' -# ixStart

        -- Whether this is the last therad
        !isLast   = ixThread ==# (nThreads -# 1#)

   in   case getChunk lens idxs (I# ixStart) (I# nElems) isLast of
         (# lensSlice, ixFirstSeg, offset #) 
           -> (lensSlice, (I# nElems), ixFirstSeg, offset)
{-# NOINLINE splitSegsOnElems #-}


---------------------------------------------------------------------------------------------------
-- | Determine what elements go on a thread.
--   The 'chunk' refers to the a chunk of the flat array, and is defined
--   by a set of segment slices. 
--
--   Example:
--    In this picture each X represents 5 elements, and we have 5 segements in total.
--
-- @
--    segs:    ----------------------- --- ------- --------------- -------------------
--    elems:  |X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|
--            |     thread1     |     thread2     |     thread3     |     thread4     |
--    segid:  0                 0                 3                 4
--    offset: 0                 45                0                 5
--    k:               0                 1                 3                 5
--    k':              1                 3                 5                 5
--    left:            0                 15                0                 45
--    right:           45                20                5                 0
--    left_len:        0                 1                 0                 1
--    left_off:        0                 45                0                 5
--    n':              1                 3                 2                 1
-- @
getChunk
        :: U.Vector Int    -- ^ Lengths of input segmenents.
        -> U.Vector Int    -- ^ Starting indices of input segments.
        -> Int             -- ^ Index into the flat array for the first element on this thread.
        -> Int             -- ^ Number of elements in for this thread.
        -> Bool            -- ^ Whether this is the last thread in the gang.
        -> (# U.Vector Int --   Lengths of segment slices, 
            , Int          --     segid of first slice,
            , Int #)       --     offset of first slice.

getChunk !lens !idxs !nStart !nElems is_last
  = (# lens', k-left_len, left_off #)
  where
    -- Running example:
    --   lens  = [60, 10, 20, 40, 50]
    --   ixdxs = [0, 60, 70, 90, 130]
    
    -- Total number of segments defined by segment descriptor.
    -- eg: 5
    !n  = U.length lens

    -- Segid of the first seg that starts after the left of this chunk.
    !k  = search nStart idxs

    -- Segid of the first seg that starts after the right of this chunk.
    !k'       | is_last     = n
              | otherwise   = search (nStart + nElems) idxs

    -- The length of the left-most slice of this chunk.
    !left     | k == n      = nElems
              | otherwise   = min ((idxs U.! k) - nStart) nElems

    -- The length of the right-most slice of this chunk.
    !right    | k' == k     = 0
              | otherwise   = nStart + nElems - (idxs U.! (k'-1))

    -- Whether the first element in this chunk is an internal element of
    -- of a segment. Alternatively, indicates that the first element of 
    -- the chunk is not the first element of a segment.            
    !left_len | left == 0   = 0
              | otherwise   = 1

    -- If the first element of the chunk starts within a segment, 
    -- then gives the index within that segment, otherwise 0.
    !left_off | left == 0   = 0
              | otherwise   = nStart - (idxs U.! (k-1))

    -- How many segments this chunk straddles.
    !n' = left_len + (k'-k)

    -- Create the lengths for this chunk by first copying out the lengths
    -- from the original segment descriptor. If the slices on the left
    -- and right cover partial segments, then we update the corresponding
    -- lengths.
    !lens' 
     = runST (do
            -- Create a new array big enough to hold all the lengths for this chunk.
            !mlens' <- UM.new n'

            -- If the first element is inside a segment, 
            --   then update the length to be the length of the slice.
            when (left /= 0) 
             $ UM.write mlens' 0 left

            -- Copy out array lengths for this chunk.
            U.copy (UM.drop  left_len mlens')
                    (U.slice k (k'-k) lens)

            -- If the last element is inside a segment, 
            --   then update the length to be the length of the slice.
            when (right /= 0)
             $ UM.write mlens' (n' - 1) right

            U.unsafeFreeze mlens')
{-# NOINLINE getChunk #-}


---------------------------------------------------------------------------------------------------
-- O(log n). Given a monotonically increasing vector of `Int`s,
--           find the first element that is larger than the given value.
-- 
-- eg  search 75 [0, 60, 70, 90, 130] = 90
--     search 43 [0, 60, 70, 90, 130] = 60
--
search :: Int -> U.Vector Int -> Int
search !x ys = go 0 (U.length ys)
  where
    go i n | n <= 0        = i

           | ys U.! mid < x  
           = go (mid + 1) (n - half - 1)

           | otherwise     = go i half
      where
        half = n `shiftR` 1
        mid  = i + half
{-# NOINLINE search #-}
