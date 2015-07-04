
module Data.Repa.Array.Material.Auto
        ( A             (..)
        , Name          (..)
        , Array         (..)
        , Buffer        (..)

        -- * Date utils
        , rangeDate32)
where
import Data.Repa.Array.Material.Auto.Base
import Data.Repa.Array.Material.Auto.InstArray
import Data.Repa.Array.Material.Auto.InstBox
import Data.Repa.Array.Material.Auto.InstChar
import Data.Repa.Array.Material.Auto.InstDate32
import Data.Repa.Array.Material.Auto.InstFloat
import Data.Repa.Array.Material.Auto.InstInt
import Data.Repa.Array.Material.Auto.InstList
import Data.Repa.Array.Material.Auto.InstMaybe
import Data.Repa.Array.Material.Auto.InstProduct
import Data.Repa.Array.Material.Auto.InstTuple
import Data.Repa.Array.Material.Auto.InstText
import Data.Repa.Array.Material.Auto.InstUnit
import Data.Repa.Array.Material.Auto.InstWord
import Data.Repa.Array.Internals.Target
