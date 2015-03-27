
module Data.Repa.Array.Auto.Base
        ( Array
        , Elem 
        , Build)
where
import Data.Repa.Array.Material.Auto                    (A(..))
import qualified Data.Repa.Array.Generic                as G
import qualified Data.Repa.Array.Meta.Window            as A
import qualified Data.Repa.Array.Internals.Target       as G
import qualified Data.Repa.Fusion.Unpack                as F



-- | Arrays of elements using an automatic layout.
type Array a    =  G.Array A a

-- | Class of elements that can be automatically organised into arrays.
type Elem  a    
        = ( G.Bulk  A a
          , A.Windowable A a)

-- | Class of elements of which arrays of those elements can be built
--   in parallel.
type Build a t
        = ( G.Bulk   A a
          , G.Target A a
          , F.Unpack (G.IOBuffer A a) t)
