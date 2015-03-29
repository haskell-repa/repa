
-- | Material arrays are represented as concrete data in memory.
--
--  For performance reasons, random access indexing into these layouts
--  is not bounds checked. However, all bulk operators like @map@ and @concat@
--  are guaranteed to be safe.
--
--  * `A`  -- Type directed automatic layout.
--
--  * `F`  -- Foreign memory buffers.
--
--  * `N`  -- Nested arrays.
--
--  * `B`  -- Boxed vectors, via "Data.Vector.Boxed"
--
--  * `U`  -- Adaptive unboxed vectors, via "Data.Vector.Unboxed"
--
module Data.Repa.Array.Material
        ( Material

        , Name  (..)
        , Array (..)

          -- * Auto arrays
        , A     (..)

          -- * Foreign arrays
        , F (..)
        , fromForeignPtr,       toForeignPtr
        , fromByteString,       toByteString
        , fromStorableVector,   toStorableVector

          -- * Nested arrays
        , N (..)
        , fromLists
        , fromListss

          -- * Boxed arrays
        , B     (..)
        , fromBoxed,            toBoxed

          -- * Unboxed arrays
        , U     (..)
        , Unbox
        , fromUnboxed,          toUnboxed

          -- * Material operators
          -- | These operators work on particular material representations, 
          --   rather than being generic like the ones in "Data.Repa.Array.Generic"

          -- ** Mapping
        , mapElems

          -- ** Filtering
        , decimate

          -- ** Slicing
        , slices

          -- ** Partitioning
        , partition
        , partitionBy
        , partitionByIx

          -- ** Concatenation
        , concats

          -- ** Splitting
        , segment
        , segmentOn

        , dice
        , diceSep

          -- ** Trimming
        , trims
        , trimEnds
        , trimStarts

          -- ** Transpose
        , ragspose3)
where
import Data.Repa.Array.Internals.Operator.Partition
import Data.Repa.Array.Material.Auto
import Data.Repa.Array.Material.Boxed
import Data.Repa.Array.Material.Unboxed
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Nested
import Data.Repa.Array.Meta.Window
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target


-- | Classes supported by all material representations.
--
--   We can index them in a random-access manner, 
--   window them in constant time, 
--   and use them as targets for a computation.
-- 
--   In particular, delayed arrays are not material as we cannot use them
--   as targets for a computation.
--
type Material l a
        = (Bulk l a, Windowable l a, Target l a)

