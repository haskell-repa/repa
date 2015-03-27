
module Data.Repa.Array.Material.Foreign
        ( F      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)

        -- * Format conversion
        , unsafeCast
        , fromForeignPtr,       toForeignPtr
        , fromStorableVector,   toStorableVector
        , fromByteString,       toByteString

        -- * Int conversion
        , readIntFromOffset,    readIntFromOffset#

        -- * Double conversion
        , readDouble,           readDoubleFromBytes
        , showDouble,           showDoubleAsBytes
        , showDoubleFixed,      showDoubleFixedAsBytes)
where
import Data.Repa.Array.Material.Foreign.Base
import Data.Repa.Array.Material.Foreign.Convert
