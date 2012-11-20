
module Data.Array.Repa.Vector.Repr.Delayed
        ( D, Array(..)
        , fromFunction
        , defaultFromFunction
        , checkedFromFunction
        , toFunction
        , delay)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Bulk

-- | Delayed arrays are represented as functions from the index to element value.
--
--   Every time you index into a delayed array the element at that position 
--   is recomputed.
data D

-- | Compute elements of a delayed array.
data instance Array D sh a
        = ADelayed  
                !sh 
                (sh -> a) 

instance Bulk D a where
 index       (ADelayed _  f) ix  
        = f ix
 {-# INLINE index #-}

 linearIndex (ADelayed sh f) ix  
        = f (fromIndex sh ix)
 {-# INLINE linearIndex #-}

 extent (ADelayed sh _)
        = sh
 {-# INLINE extent #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an element producing function as a delayed array.
--
--   The first argument gives the nominal extent of the array.
--
--   To preserve execution sanity in the presence of out-of-bounds indexing, 
--   the provided element function must return a deterministic default value at
--   any possible index, not just the values in the given extent.
--   If the element function does /not/ perform its own bounds checks
--   then use `checkedFromFunction` instead.
-- 
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction sh f 
        = ADelayed sh f 
{-# INLINE [4] fromFunction #-}


-- | O(1). Wrap an element producing function as a delayed array, 
--         and perform bounds checks.
--
--   If the resulting array is indexed out of bounds then return
--   the default value instead.
defaultFromFunction
        :: Shape sh
        => a 
        -> sh 
        -> (sh -> a) -> Array D sh a

defaultFromFunction def sh f
 = ADelayed sh get
 where !sz      = size sh
       get ix
        = let !lix      = toIndex sh ix
          in  if lix < 0 || lix >= sz 
                then def
                else f ix
{-# INLINE [4] defaultFromFunction #-}


-- | O(1). Wrap an element producing function as a delayed array, 
--         and perform bounds checks.
--
--   If the resulting array is indexed out of bounds then this will
--   invoke `error` and the provided string will be printed.
--
checkedFromFunction 
        :: (Shape sh, Bulk r a)
        => String -> sh 
        -> (sh -> a) -> Array D sh a

checkedFromFunction msg sh f
 = defaultFromFunction 
        (error $ "checkedFromFunction out of range -- " ++ show msg)
        sh f
{-# INLINE [4] checkedFromFunction #-}


-- | O(1). Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction 
        :: (Shape sh, Bulk r a)
        => Array r sh a -> (sh, sh -> a)
toFunction arr
 = case delay arr of
        ADelayed sh f -> (sh, f)
{-# INLINE [4] toFunction #-}


-- | O(1). Delay an array.
--   This wraps the internal representation to be a function from
--   indices to elements, so consumers don't need to worry about
--   what the previous representation was.
--
delay   :: (Shape sh, Bulk r a) 
        => Array  r sh a -> Array D sh a
delay arr = ADelayed (extent arr) (index arr)
{-# INLINE [4] delay #-}
