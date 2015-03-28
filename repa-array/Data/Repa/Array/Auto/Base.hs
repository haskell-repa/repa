
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



-- | Arrays of elements that are automatically layed out into some
--   efficient runtime representation.
--
--   The implementation uses type families to chose unboxed representations
--   for all elements that can be unboxed. In particular: arrays of unboxed
--   tuples are represented as tuples of unboxed arrays, and nested arrays
--   are represented using a segment descriptor and a single single flat
--   vector containing all the elements.
--

type Array a    
        =  G.Array A a

-- | Class of elements that can be automatically organised into arrays.
type Elem  a    
        = ( G.Bulk  A a
          , A.Windowable A a)

-- | Class of elements where arrays of those elements can be constructed
--   in arbitrary order.
type Build a t
        = ( G.Bulk   A a
          , G.Target A a
          , F.Unpack (G.IOBuffer A a) t)
