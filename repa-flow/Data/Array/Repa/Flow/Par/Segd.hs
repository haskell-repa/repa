
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
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Bulk.Gang
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
        , indices       = U.init $ U.scanl (+) 0 lens }
{-# INLINE [1] fromLengths #-}


-- Split Segds ----------------------------------------------------------------
-- | Split segment descriptors describe the segmentation of an array in a way
--   that helps segmented operators distribute the work between several threads.
data SplitSegd
        = SplitSegd
        { -- | Segment descriptor is split across this gang.
          splitGang     :: Gang

          -- | The original, unsplit segment descriptor.
        , splitOriginal :: !Segd

          -- | Vector of Segd chunks.
        , splitChunks   :: !(V.Vector Chunk) }
        deriving Show

data Chunk
        = Chunk
        { -- | The segment id of the original descriptor
          --   that this chunk starts in.
          chunkStart    :: Int#

          -- | Starting offset of the first segment relative to the original
          --   discriptor. This is used when the chunk starts in the middle
          --   of one of the original segments.
        , chunkOffset   :: Int# 

          -- | Local `Segd` for the chunk.
        , chunkSegd     :: Segd }
        deriving Show


-- | Split a `Segd` into chunks, 
--   trying to assign the same number of elements to each chunk.
splitSegd :: Gang -> Segd -> SplitSegd 
splitSegd !gang segd
 = SplitSegd 
        gang
        segd
        (V.map makeChunk $ V.enumFromN 0 (I# chunks))
 where  
        !chunks
         = gangSize gang

        makeChunk ix     
         = let  !(lens, _, I# first, I# offset) 
                 = splitSegsOnElems 
                        (lengths segd) 
                        (indices segd) 
                        (I# (elements segd))
                        (I# chunks)
                        ix
           in   Chunk 
                { chunkStart    = first
                , chunkOffset   = offset 
                , chunkSegd     = fromLengths lens }
{-# INLINE [1] splitSegd #-}


-- | Take the `Distro` of a `SplitSegd`.
--   This tells us how the array elements should be distributed on the gang.
distroOfSplitSegd :: SplitSegd -> Distro BB
distroOfSplitSegd (SplitSegd gang (Segd nElems _ ixs) chunks)
 = let here = "par.distroOfSplitSegd"
   in  DistroBalanced 
        { distroBalancedFrags      = gangSize gang
        , distroBalancedLength     = nElems

        , distroBalancedFragLength 
           = \ix -> elements (chunkSegd (vindex here chunks (I# ix)))

        , distroBalancedFragStart  
           = \ix ->
                let chunk        = vindex here chunks (I# ix)
                    seg          = chunkStart chunk

                    !(I# seg_ix) 
                        | U.length ixs == 0     = 0
                        | otherwise             = uindex here ixs (I# seg)

                    offset       = chunkOffset chunk
                in  offset +# seg_ix }
{-# INLINE [1] distroOfSplitSegd #-}


