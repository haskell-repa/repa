
module Data.Repa.Array.Material
        ( Name  (..)
        , Array (..)

          -- * Boxed arrays
        , B     (..)
        , fromBoxed,            toBoxed

          -- * Unboxed arrays
        , U     (..)
        , Unbox
        , fromUnboxed,          toUnboxed

          -- * Foreign arrays
        , F (..)
        , fromForeignPtr,       toForeignPtr
        , fromByteString,       toByteString


          -- * Nested arrays
        , N (..)

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
import Data.Repa.Array.Material.Boxed
import Data.Repa.Array.Material.Unboxed
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Nested



