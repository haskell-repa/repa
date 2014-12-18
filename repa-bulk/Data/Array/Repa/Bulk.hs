
module Data.Array.Repa.Bulk
        ( -- * Bulk arrays
          Bulk (..)
        , (!)

          -- * Array construction
        , Target    (..)
        , Load      (..)
        , LoadRange (..)

          -- * Conversion
        , arrayOfList
        , listOfArray
        , listOfArrays
        , listOfArrayss)
where
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Bulk.Load


