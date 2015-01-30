
module Data.Repa.Array.Material
        ( Name  (..)
        , Array (..)

          -- * Unboxed arrays
        , U     (..)
        , Unbox

          -- ** Conversion
        , fromVector
        , toVector


          -- * Foreign arrays
        , F (..)

          -- ** Conversion
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
        , segment,      segmentOn
        , dice,         diceOn

          -- ** Trimming
        , trims
        , trimEnds
        , trimStarts

          -- ** Transpose
        , ragspose3)
where
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Nested
import Data.Repa.Array.Material.Unboxed



