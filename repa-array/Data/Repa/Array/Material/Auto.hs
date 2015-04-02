
module Data.Repa.Array.Material.Auto
        ( A             (..)
        , Name          (..)
        , Array         (..)
        , Buffer        (..)

        -- * Date utils
        , rangeDate32
        , prettyDate32)
where
import Data.Repa.Array.Material.Auto.Base
import Data.Repa.Array.Material.Auto.InstFloat
import Data.Repa.Array.Material.Auto.InstInt
import Data.Repa.Array.Material.Auto.InstWord
import Data.Repa.Array.Material.Auto.InstDate32
import Data.Repa.Array.Internals.Target
