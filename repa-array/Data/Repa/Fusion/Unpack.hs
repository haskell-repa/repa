
module Data.Repa.Fusion.Unpack
        (Unpack (..))
where


-- | Unpack the pieces of a structure into a tuple.
--
--   This is used in a low-level fusion optimisation to ensure that
--   intermediate values are unboxed.
--
class Unpack a t | a -> t where
 unpack :: a -> t
 repack :: a -> t -> a

