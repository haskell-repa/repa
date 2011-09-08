
module Data.Array.Repa
        ( module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , module Data.Array.Repa.Slice
        , Array
        , Repr(..)
        , Load(..)
        
        -- * Delayed representation
        , D, fromFunction, toFunction
        , delay, copy
                
        -- * Unboxed vector representation
        , U, fromUnboxed, toUnboxed
        
        -- * List representation
        , L, fromList, toList)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Data.Array.Repa.Repr.Unboxed
