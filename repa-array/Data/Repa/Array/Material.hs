
module Data.Repa.Array.Material
        ( Name  (..)
        , Array (..)

          -- * Auto arrays
        , A     (..)

          -- * Boxed arrays
        , B     (..)
        , fromBoxed,            toBoxed
        , decimate

          -- * Nested arrays
        , N (..)

          -- * Unboxed arrays
        , U     (..)
        , Unbox
        , fromUnboxed,          toUnboxed

          -- * Foreign arrays
        , F (..)
        , fromForeignPtr,       toForeignPtr
        , fromByteString,       toByteString
        , fromStorableVector,   toStorableVector

          -- ** Ingestion
        , module Data.Repa.Binary.Format
        , Packable
        , packForeign
        , unpackForeign

          -- ** Conversion
        , fromLists
        , fromListss

          -- ** Mapping
        , mapElems

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
        , ragspose3)
where
import Data.Repa.Array.Material.Auto
import Data.Repa.Array.Material.Boxed
import Data.Repa.Array.Material.Unboxed
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Nested
import Data.Repa.Binary.Format


