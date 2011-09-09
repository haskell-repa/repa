
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
        , force
        
        -- * List representation
        , L, fromList, toList
        
        -- * Mapping
        , map
        , zipWith
        , (+^), (-^), (*^), (/^)
        
        -- * Traversal
        , traverse, unsafeTraverse)
        
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Prelude ()
