
module Data.Array.Repa.Vector.Distro


-- TODO: should only be able to apply length when the length
--       is known before computing the vector.
--       Should be dependent on the balance mode.

-- | Get the length of a vector.
-- length :: Source r e => Vector r e -> Int
-- length !v
--  = case extent v of
--         Z :. len        -> len
-- {-# INLINE [4] length #-}
