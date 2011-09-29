{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns, ExplicitForAll, TypeOperators #-}

module Data.Array.Repa.Operators.Reduction
	( fold, foldAll
	, sum,  sumAll)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Shape		        as S
import qualified Data.Vector.Unboxed	        as V
import qualified Data.Vector.Unboxed.Mutable    as M
import Prelude				        hiding (sum)

import Data.Array.Repa.Eval.Reduction
import System.IO.Unsafe


-- | Reduction of the innermost dimension of an arbitrary rank array. The first
--   argument needs to be an /associative/ operator. The starting element must
--   be neutral with respect to the operator, for example @0@ is neutral with
--   respect to @(+)@ as @0 + a = a@. These restrictions are required to support
--   parallel evaluation, as the starting element may be used multiple
--   times depending on the number of threads.

--   Combine this with `transpose` to fold any other dimension.
fold 	:: (Shape sh, Elt a, Unbox a, Repr r a)
	=> (a -> a -> a)
	-> a
	-> Array r (sh :. Int) a
	-> Array U sh a
{-# INLINE [0] fold #-}
fold f z arr 
 = let  sh@(sz :. n) = extent arr
   in   case rank sh of
           -- specialise rank-1 arrays, else one thread does all the work. We can't
           -- match against the shape constructor, otherwise type error: (sz ~ Z)
           --
           1 -> let !vec = V.singleton $ foldAll f z arr
                in  fromUnboxed sz vec

           _ -> unsafePerformIO 
              $ do mvec   <- M.unsafeNew (S.size sz)
                   foldP mvec (\ix -> arr `unsafeIndex` fromIndex sh ix) f z n
                   !vec   <- V.unsafeFreeze mvec
                   return $ fromUnboxed sz vec


-- | Reduction of an array of arbitrary rank to a single scalar value. The first
--   argument needs to be an /associative/ operator. The starting element must
--   be neutral with respect to the operator, for example @0@ is neutral with
--   respect to @(+)@ as @0 + a = a@. These restrictions are required to support
--   parallel evaluation, as the starting element may be used multiple
--   times depending on the number of threads.
foldAll :: (Shape sh, Elt a, Unbox a, Repr r a)
	=> (a -> a -> a)
	-> a
	-> Array r sh a
	-> a
{-# INLINE [0] foldAll #-}
foldAll f z arr 
 = let  sh = extent arr
        n  = size sh
   in   unsafePerformIO $ foldAllP (\ix -> arr `unsafeIndex` fromIndex sh ix) f z n


-- | Sum the innermost dimension of an array.
sum	:: (Shape sh, Num a, Elt a, Unbox a, Repr r a)
	=> Array r (sh :. Int) a
	-> Array U sh a
{-# INLINE sum #-}
sum arr	= fold (+) 0 arr


-- | Sum all the elements of an array.
sumAll	:: (Shape sh, Elt a, Unbox a, Num a, Repr r a)
	=> Array r sh a
	-> a
{-# INLINE sumAll #-}
sumAll arr = foldAll (+) 0 arr
