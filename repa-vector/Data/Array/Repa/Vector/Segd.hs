{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Vector.Segd
        ( -- * Segment Descriptors
          Segd          (..)
        , empty

          -- * Split Segment Descriptors
        , SplitSegd     (..)
        , Chunk         (..)
        , splitSegd
        , distroOfSplitSegd

          -- * Segmented operations
        , vreplicates
        , vreplicatesSplit)
where
import Data.Array.Repa.Repr.Chain       
import Data.Array.Repa.Vector.Segd.Split
import Data.Array.Repa                            as R
import Data.Array.Repa.Vector.Base                as R
import Data.Array.Repa.Eval.Gang                  as R
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import qualified Data.Array.Repa.Chain.Replicate  as C
import GHC.Exts


-- | Segment descriptor.
data Segd r1 r2
        = Segd
        { -- | Length of each segment.
          lengths       :: Vector r1 Int

          -- | Starting index of each segment.
        , indices       :: Vector r2 Int

          -- | Number of elements in the flat array.
        , elements      :: Int# }

deriving instance (Show (Vector r1 Int), Show (Vector r2 Int))
       => Show (Segd r1 r2)


-- Constructors ---------------------------------------------------------------
-- | Construct an empty segment descriptor, 
--   with no elements or segments.
empty :: Segd D D
empty   = Segd
        { lengths       = R.fromFunction (Z :. 0) (const 0)
        , indices       = R.fromFunction (Z :. 0) (const 0)
        , elements      = 0# }


-- Split Segds ----------------------------------------------------------------
-- | Split segment descriptors describe the segmentation of an array in a way
--   that helps segmented operators distribute the work between several threads.
data SplitSegd
        = SplitSegd
        { -- | Number of chunks this Segd is split into.
          splitChunks   :: Int#

          -- | Total number of elements described by the Segd.
        , splitElems    :: Int#

          -- | Vector of Segd chunks.
        , splitChunk    :: V.Vector Chunk }
        deriving Show

data Chunk
        = Chunk
        { -- | Segment lengths in this chunk.
          chunkLengths  :: Vector U Int

          -- | Number of elements in this chunk @(sum chunkLengths)@
        , chunkElems    :: Int#

          -- | Id of first segment in the chunk.
        , chunkStart    :: Int#

          -- | Starting offset of the first segment, 
          --   if this chunk starts in the middle of one of the original segments.
        , chunkOffset   :: Int# }
        deriving Show


-- | Split a `Segd` into chunks, 
--   trying to assign the same number of elements to each chunk.
splitSegd :: Segd U U -> SplitSegd 
splitSegd segd
 = SplitSegd 
        chunks
        (elements segd)
        (V.map makeChunk $ V.enumFromN 0 (I# chunks))
 where  
        !(I# chunks)
         = R.gangSize $ R.theGang

        makeChunk ix     
         = let  !(lens, I# count, I# first, I# offset) 
                 = splitSegsOnElems 
                        (R.toUnboxed $ lengths segd) 
                        (R.toUnboxed $ indices segd) 
                        (I# (elements segd))
                        (I# chunks)
                        ix
           in   Chunk 
                { chunkLengths  = R.fromUnboxed (Z :. U.length lens) lens
                , chunkElems    = count
                , chunkStart    = first
                , chunkOffset   = offset }


-- | Take the `Distro` of a `SplitSegd`.
--   This tells us how the array elements should be distributed on the gang.
distroOfSplitSegd :: SplitSegd -> Distro
distroOfSplitSegd (SplitSegd nChunks nElems chunks)
        = Distro 
        { distroLength          = nElems
        , distroFrags           = nChunks
        , distroFragLength      = \ix -> chunkElems (chunks V.! (I# ix))
        , distroFragStart       = \ix -> chunkStart (chunks V.! (I# ix)) }


-- | Segmented replicate. 
--   Each element of the vector is replicated according to the length of
--   the segment in the segment descriptor.
--
--   The vector must contain as many elements as there are segments in the 
--   descriptor, else undefined.
--
-- @
--  vreplicates (fromLengths [3, 1, 2]) [5, 6, 7] 
--   = [5, 5, 5, 6, 7, 7]
-- @
--
vreplicates
        :: U.Unbox a
        => Segd U U
        -> Vector U a
        -> Vector N a
vreplicates segd vec
        = vreplicatesSplit (splitSegd segd) vec
{-# INLINE [1] vreplicates #-}


-- | Segmented replicate. 
--   Like `vreplicates` except that it takes a pre-split segment descriptor.
vreplicatesSplit
        :: U.Unbox a
        => SplitSegd
        -> Vector U a
        -> Vector N a

vreplicatesSplit segd vec
 = vcacheChain 
 $ C.replicatesD 
        (distroOfSplitSegd segd)
        getFragSegLength
        getFragSegStart
        getElem
 where
        -- Get the lengths of segments assigned in this fragment.
        getFragSegLength frag
         = getSegLength
         where !lens = chunkLengths $ V.unsafeIndex (splitChunk segd) (I# frag) 
               getSegLength seg 
                     = let !(I# i) = R.unsafeLinearIndex lens (I# seg) 
                       in  i

        -- Get the index of the starting segment in this fragment.
        getFragSegStart frag
         = chunkStart (V.unsafeIndex (splitChunk segd) (I# frag))

        -- Get the element to replicate for this segment.
        getElem seg
         = R.unsafeLinearIndex vec (I# seg)
{-# INLINE [1] vreplicatesSplit #-}
