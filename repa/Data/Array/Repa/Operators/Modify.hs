{-# OPTIONS_HADDOCK hide #-}

module Data.Array.Repa.Operators.Modify 
        ( -- * Bulk updates
         (//))
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base

{-
stage :: String
stage = "Data.Array.Repa.Operators.Modify"
-}

-- Bulk updates ----------------------------------------------------------------
-- | For each pair @(sh, a)@ from the list of index/value pairs, replace the
-- element at position @sh@ by @a@.
--
-- > update <5,9,2,7> [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
{-# INLINE (//) #-}
(//) :: (Shape sh, Elt a) => Array sh a -> [(sh,a)] -> Array sh a
(//) arr us 
        = fromFunction
                (extent arr) 
                (\sh -> case lookup sh us of
                            Just a  -> a
                            Nothing -> index arr sh)

{-
-- For each pair @(sh, a)@ from the array of index/value pairs, replace the
-- element at position @sh@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
{-# INLINE update #-}
update :: Shape sh
       => Array sh a            -- ^ initial array
       -> Array sh (sh, a)      -- ^ array of shape/value pairs
       -> Array sh a
update _arr _us = error $ stage ++ ".update: not defined yet"


-- Same as 'update', but without bounds checks
--
{-# INLINE unsafeUpdate #-}
unsafeUpdate :: Shape sh
             => Array sh a
             -> Array sh (sh, a)
             -> Array sh a
unsafeUpdate _arr _us = error $ stage ++ ".unsafeUpdate: not defined yet"
-}
