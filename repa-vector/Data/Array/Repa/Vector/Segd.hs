
module Data.Array.Repa.Vector.Segd
        ( -- * Segment Descriptors
          Segd          (..)
        , empty

          -- * Split Segment Descriptors
        , SplitSegd     (..)
        , Chunk         (..)
        , splitSegd
        , distroOfSplitSegd)
where
import Data.Array.Repa.Vector.Segd.Split
import Data.Array.Repa                  as R
import Data.Array.Repa.Vector           as R
import Data.Array.Repa.Eval.Gang        as R
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U
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


-- Constructors ---------------------------------------------------------------
-- | Construct an empty segment descriptor, 
--   with no elements or segments.
empty :: Segd D D
empty   = Segd
        { lengths       = R.fromFunction (Z :. 0) (const 0)
        , indices       = R.fromFunction (Z :. 0) (const 0)
        , elements      = 0# }


-- Split Segds ----------------------------------------------------------------
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

          -- | The index of the starting element, 
          --   relative to the flat array.
        , chunkStart    :: Int#

          -- | Starting offset of the first segment, 
          --   if this chunk starts in the middle of one of the original segments.
        , chunkOffset   :: Int# }
        deriving Show


-- | Split a `Segd` into chunks, 
--   assigning almost the same number of elements to each chunk.
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
