
module Data.Repa.Array.Material
        ( Name  (..)
        , Array (..)

          -- * Auto arrays
        , A     (..)

          -- * Boxed arrays
        , B     (..)
        , fromBoxed,            toBoxed

          -- * Nested arrays
        , N (..)
        , fromLists
        , fromListss

          -- * Unboxed arrays
        , U     (..)
        , Unbox
        , fromUnboxed,          toUnboxed

          -- * Foreign arrays
        , F (..)
        , fromForeignPtr,       toForeignPtr
        , fromByteString,       toByteString
        , fromStorableVector,   toStorableVector

          -- * Material operators
          -- | These operators work on particular material representations, 
          --   rather than being generic like the ones in "Data.Repa.Array.Generic"

          -- ** Mapping
        , mapElems

          -- ** Filtering
        , decimate

          -- ** Slicing
        , slices

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
        , ragspose3

          -- ** Ingestion
        , module Data.Repa.Convert.Format
        , packForeign
        , unpackForeign)
where
import Data.Repa.Array.Material.Auto
import Data.Repa.Array.Material.AutoUnpack
import Data.Repa.Array.Material.Boxed
import Data.Repa.Array.Material.Unboxed
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Nested
import Data.Repa.Convert.Format


