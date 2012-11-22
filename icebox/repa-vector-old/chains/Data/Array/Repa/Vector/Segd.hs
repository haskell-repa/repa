{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Vector.Segd
        ( -- * Segment Descriptors
          Segd          (..)
        , empty
        , fromLengths

          -- * Split Segment Descriptors
        , SplitSegd     (..)
        , Chunk         (..)
        , splitSegd
        , distroOfSplitSegd

          -- * Segmented Replicate
        , vreplicates
        , vreplicatesSplit

          -- * Segmented Append
        , vappends
        , vappendsSplit

          -- * Segmented Pack
        , vpacks)
where
import Data.Array.Repa.Repr.Chain       
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Segd.Split
import Data.Array.Repa.Vector.Operators.Pack
import Data.Array.Repa.Vector.Operators.Zip
import Data.Array.Repa                            as R
import Data.Array.Repa.Vector.Base                as R
import Data.Array.Repa.Eval.Gang                  as R
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import qualified Data.Array.Repa.Chain            as C
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


-- | Construct a segment descriptor from a lengths vector.
--   TODO: Make this more general wrt array representations.
--   TODO: computation of indices doesn't run in parallel.
fromLengths :: Vector U Int -> Segd U U
fromLengths lens
 = let !(I# len)        = U.sum (R.toUnboxed lens)
   in  Segd
        { lengths       = lens

        , indices       = R.fromUnboxed  (Z :. vlength lens)
                        $ U.scanl (+) 0 (R.toUnboxed lens)

        , elements      = len }


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
distroOfSplitSegd :: Segd U U -> SplitSegd -> Distro
distroOfSplitSegd (Segd _ ixs _) (SplitSegd nChunks nElems chunks)
        = Distro 
        { distroLength          = nElems
        , distroFrags           = nChunks
        , distroFragLength      = \ix -> chunkElems (chunks V.! (I# ix))
        , distroFragStart       = \ix ->
                let chunk  = chunks V.! (I# ix)
                    seg    = chunkStart chunk
                    !(I# seg_ix) = ixs `R.unsafeLinearIndex` (I# seg)
                    offset = chunkOffset chunk
                in  offset +# seg_ix }



-- Replicates -----------------------------------------------------------------
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
        = vreplicatesSplit segd (splitSegd segd) vec
{-# INLINE [1] vreplicates #-}


-- | Segmented replicate. 
--   Like `vreplicates` except that it takes a pre-split segment descriptor.
vreplicatesSplit
        :: U.Unbox a
        => Segd U U
        -> SplitSegd
        -> Vector U a
        -> Vector N a

vreplicatesSplit flatsegd segd vec
 = vcacheChain 
 $ C.replicatesD 
        (distroOfSplitSegd flatsegd segd)
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


-- Appends --------------------------------------------------------------------
-- | Segmented append
vappends
        :: U.Unbox a
        => Segd U U                     -- ^ Segment descriptor of result.
        -> Segd U U                     -- ^ Segment descriptor of first array.
        -> Vector U a                   -- ^ Elements of first array.
        -> Segd U U                     -- ^ Segment descriptor of second array.
        -> Vector U a                   -- ^ Elements of second array.
        -> Vector N a

vappends segdResult segd1 vec1 segd2 vec2
        = vappendsSplit 
                segdResult
                (splitSegd segdResult)
                segd1 vec1
                segd2 vec2


-- | Segmented append.
--   Like `vappends` except that it takes a pre-split result segment descriptor.
vappendsSplit
        :: U.Unbox a
        => Segd U U                     -- ^ Flat segment descriptor of result.
        -> SplitSegd                    -- ^ Split segment descriptor of result.
        -> Segd U U                     -- ^ Segment descriptor of first array.
        -> Vector U a                   -- ^ Elements of first array.
        -> Segd U U                     -- ^ Segment descriptor of second array.
        -> Vector U a                   -- ^ Elements of second array.
        -> Vector N a

vappendsSplit 
        flatsegd
        segdResult
        (Segd lens1 indices1 _) vec1
        (Segd lens2 indices2 _) vec2
 = vcacheChain 
 $ C.DistChain (distroOfSplitSegd flatsegd segdResult) frag
 where  
        unbox (I# i) = i
        segLen1 i = unbox (vindex lens1    (I# i))
        segIdx1 i = unbox (vindex indices1 (I# i))
        elem1   i = vindex vec1 (I# i)

        segLen2 i = unbox (vindex lens2    (I# i))
        segIdx2 i = unbox (vindex indices2 (I# i))
        elem2   i = vindex vec2 (I# i)

        frag i
         = let  chunk   = V.unsafeIndex (splitChunk segdResult) (I# i) 
           in   C.appendSegs 
                        segLen1 segIdx1 elem1
                        segLen2 segIdx2 elem2
                        (chunkElems  chunk)
                        (chunkStart  chunk)
                        (chunkOffset chunk)


-- Packs ----------------------------------------------------------------------
-- | Segmented Pack
vpacks  :: U.Unbox a
        => Vector U Bool
        -> Segd U U
        -> Vector U a
        -> Vector S a

vpacks flags segd vec
 = let  flags'  = vreplicates segd flags
   in   vpack $ vzip flags' vec



