{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Vector.Sel
        ( Sel2(..)
        , fromTags
        , fromFlags)
where
import Data.Array.Repa.Vector.Operators.Scan
import Data.Array.Repa.Vector.Base
import Data.Array.Repa                  as R
import GHC.Exts
import qualified Data.Vector.Unboxed    as U


-- | O(1). Construct a selector.
--
--   A selector is a description of how to perform a `combine` operation.
--
--   Suppose we are evaluating the following expression:
--
--    @combine [F F T F T T] [1 2 3] [4 5 6] = [4 5 1 6 2 3]@
--
--   This is difficult to parallelise. For each element in the result, the
--   source array we get this element from depends on the tag values associated
--   with all previous elements.
--
--   However, if we going to apply `combine` several times with the same flags
--   array, we can precompute a selector that tells us where to get each element. 
--   The selector contains the original flags, as well as the source index
--   telling us where to get each element for the result array.
--
--   For example:
--
--   @tagsToIndices2 [F F T F T T]   -- tags
--             = [0 1 0 2 1 2]   -- indices
--   @
--
--    This says get the first element from index 0 in the second array, 
--     then from index 1 in the second array,
--     then index 0 in the first array ...
--  
--    The selector then consists of both the @tag@ and @indices@ arrays.
--
data Sel2 r1 r2
        = Sel2
        { tags          :: Vector r1 Int
        , indices       :: Vector r2 Int
        , elements0     :: Int#
        , elements1     :: Int# }


deriving instance (Show (Vector r1 Int), Show (Vector r2 Int))
        => Show (Sel2 r1 r2)


-- | Construct a selector from a vector of tags.
--
--   TODO: make this parallel.
--
--   TODO: count both elements in the one pass.
--
fromTags :: Vector U Int -> Sel2 U U
fromTags tags'@(AUnboxed _ vec)
 = let  !(I# elts0)     = U.sum $ U.map (const 1) $ U.filter (== 0) vec
        !(I# elts1)     = U.sum $ U.map (const 1) $ U.filter (== 1) vec
        indices'        = indicesOfTags tags'
   in   Sel2 tags' indices' elts0 elts1


-- | Construct a selector from a vector of boolean flags.
-- 
--   TODO: fix fusion
fromFlags :: Vector U Bool -> Sel2 U U
fromFlags flags
 = fromTags
 $ computeUnboxedS 
 $ R.map tagOfFlag flags
 where  tagOfFlag True  = 1
        tagOfFlag False = 0


-- | Produce the selector indices from its tags.
indicesOfTags  :: Vector U Int -> Vector U Int
indicesOfTags tags'
 = vmapAccum add (0, 0) tags'
 where  add (i, j) 0    = ((i + 1, j), i)
        add (i, j) _    = ((i, j + 1), j)


