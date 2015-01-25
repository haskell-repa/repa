
module Data.Repa.Array.Material.Safe.Base
        ( B (..)
        , U (..)
        , F (..))
where


-- | Representation tag for arrays of Boxed elements.
data B = B


-- | Representation tag for arrays of Unboxed elements.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
data U = U


-- | Representation tag for Foreign arrays.
data F = F

