
module Data.Array.Repa.Flow.Par.Segd
        ( -- * Segment Descriptors
          Segd          (..)
        , empty
        , fromLengths

          -- * Split Segment Descriptors
        , SplitSegd     (..)
        , Chunk         (..)
        , splitSegd
        , distroOfSplitSegd)
where
import Data.Array.Repa.Flow.Par.SegdSplit
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Eval.Gang                  as Gang
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import GHC.Exts


-- | Segment descriptor.
data Segd
        = Segd
        { -- | Number of elements in the flat array.
          elements      :: Int# 

        -- | Length of each segment.
        , lengths       :: !(U.Vector Int)

          -- | Starting index of each segment.
        , indices       :: !(U.Vector Int) }
        deriving Show


-- Constructors ---------------------------------------------------------------
-- | Construct an empty segment descriptor, 
--   with no elements or segments.
empty :: Segd
empty   = Segd
        { elements      = 0#
        , lengths       = U.empty 
        , indices       = U.empty }
{-# INLINE [1] empty #-}


-- | Construct a segment descriptor from a lengths vector.
--
--   TODO: computation of indices doesn't run in parallel.
--
fromLengths :: U.Vector Int -> Segd
fromLengths lens
 = let !(I# len)        = U.sum lens
   in  Segd
        { elements      = len
        , lengths       = lens
        , indices       = U.scanl (+) 0 lens }
{-# INLINE [1] fromLengths #-}


-- Split Segds ----------------------------------------------------------------
-- | Split segment descriptors describe the segmentation of an array in a way
--   that helps segmented operators distribute the work between several threads.
data SplitSegd
        = SplitSegd
        { -- | The original, unsplit segment descriptor.
          splitOriginal :: !Segd

         -- | Total number of elements described by the Segd.
        , splitElems    :: Int#

        -- | Number of chunks this Segd is split into.
        , splitChunks   :: Int#

          -- | Vector of Segd chunks.
        , splitChunk    :: !(V.Vector Chunk) }
        deriving Show

data Chunk
        = Chunk
        { -- | Number of elements in this chunk @(sum chunkLengths)@
          chunkElems    :: Int#

          -- | Id of first segment in the chunk.
        , chunkStart    :: Int#

          -- | Starting offset of the first segment, 
          --   if this chunk starts in the middle of one of the original segments.
        , chunkOffset   :: Int# 

          -- | Segment lengths in this chunk.
        , chunkLengths  :: !(U.Vector Int) }
        deriving Show


-- | Split a `Segd` into chunks, 
--   trying to assign the same number of elements to each chunk.
splitSegd :: Segd -> SplitSegd 
splitSegd segd
 = SplitSegd 
        segd
        (elements segd)
        chunks
        (V.map makeChunk $ V.enumFromN 0 (I# chunks))
 where  
        !(I# chunks)
         = Gang.gangSize $ Gang.theGang

        makeChunk ix     
         = let  !(lens, I# count, I# first, I# offset) 
                 = splitSegsOnElems 
                        (lengths segd) 
                        (indices segd) 
                        (I# (elements segd))
                        (I# chunks)
                        ix
           in   Chunk 
                { chunkElems    = count
                , chunkStart    = first
                , chunkOffset   = offset 
                , chunkLengths  = lens }
{-# INLINE [1] splitSegd #-}


-- | Take the `Distro` of a `SplitSegd`.
--   This tells us how the array elements should be distributed on the gang.
distroOfSplitSegd :: SplitSegd -> Distro BB
distroOfSplitSegd (SplitSegd (Segd _ _ ixs) nElems nChunks chunks)
        = DistroBalanced 
        { distroBalancedFrags      = nChunks
        , distroBalancedLength     = nElems

        , distroBalancedFragLength 
           = \ix -> chunkElems (V.unsafeIndex chunks (I# ix))

        , distroBalancedFragStart  
           = \ix ->
                let chunk        = V.unsafeIndex chunks (I# ix)
                    seg          = chunkStart chunk
                    !(I# seg_ix) = ixs `U.unsafeIndex` (I# seg)
                    offset       = chunkOffset chunk
                in  offset +# seg_ix }
{-# INLINE [1] distroOfSplitSegd #-}


