{-# LANGUAGE BangPatterns, ExplicitForAll, TypeOperators, MagicHash #-}

module Data.Array.Repa.Operators.Reduction
	( foldS,        foldP
	, foldAllS,     foldAllP
	, sumS,         sumP
	, sumAllS,      sumAllP)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Shape		        as S
import qualified Data.Vector.Unboxed	        as V
import qualified Data.Vector.Unboxed.Mutable    as M
import Prelude				        hiding (sum)
import qualified Data.Array.Repa.Eval.Reduction as E
import System.IO.Unsafe
import GHC.Exts

-- foldS ------------------------------------------------------------------------------------------
-- | Sequential reduction of the innermost dimension of an arbitrary rank array.
--
--   Combine this with `transpose` to fold any other dimension.
foldS 	:: (Shape sh, Elt a, Unbox a, Repr r a)
	=> (a -> a -> a)
	-> a
	-> Array r (sh :. Int) a
	-> Array U sh a
{-# INLINE [2] foldS #-}
foldS f z arr
 = let  sh@(sz :. n') = extent arr
        !(I# n)       = n'
   in unsafePerformIO
    $ do mvec   <- M.unsafeNew (S.size sz)
         E.foldS mvec (\ix -> arr `unsafeIndex` fromIndex sh (I# ix)) f z n
         !vec   <- V.unsafeFreeze mvec
         return $ fromUnboxed sz vec


-- | Parallel reduction of the innermost dimension of an arbitray rank array.
--
--   The first argument needs to be an /associative/ operator. The starting element must
--   be neutral with respect to the operator, for example @0@ is neutral with
--   respect to @(+)@ as @0 + a = a@. These restrictions are required to support
--   parallel evaluation, as the starting element may be used multiple
--   times depending on the number of threads.
foldP 	:: (Shape sh, Elt a, Unbox a, Repr r a)
	=> (a -> a -> a)
	-> a
	-> Array r (sh :. Int) a
	-> Array U sh a
{-# INLINE [2] foldP #-}
foldP f z arr 
 = let  sh@(sz :. n) = extent arr
   in   case rank sh of
           -- specialise rank-1 arrays, else one thread does all the work. We can't
           -- match against the shape constructor, otherwise type error: (sz ~ Z)
           --
           1 -> let !vec = V.singleton $ foldAllP f z arr
                in  fromUnboxed sz vec

           _ -> unsafePerformIO 
              $ do mvec   <- M.unsafeNew (S.size sz)
                   E.foldP mvec (\ix -> arr `unsafeIndex` fromIndex sh ix) f z n
                   !vec   <- V.unsafeFreeze mvec
                   return $ fromUnboxed sz vec


-- foldAll ----------------------------------------------------------------------------------------
-- | Sequential reduction of an array of arbitrary rank to a single scalar value.
--
foldAllS :: (Shape sh, Elt a, Unbox a, Repr r a)
	=> (a -> a -> a)
	-> a
	-> Array r sh a
	-> a
{-# INLINE [2] foldAllS #-}
foldAllS f z arr 
        = E.foldAllS 
                (\ix -> arr `unsafeIndex` fromIndex (extent arr) ix) 
                f 
                z 
                (size (extent arr))


-- | Parallel reduction of an array of arbitrary rank to a single scalar value.
--
--   The first argument needs to be an /associative/ operator.
--   The starting element must be neutral with respect to the operator, for exampl
--   @0@ is neutral with respect to @(+)@ as @0 + a = a@. These restrictions are
--   required to support parallel evaluation, as the starting element may be used multiple
--   times depending on the number of threads.
foldAllP :: (Shape sh, Elt a, Unbox a, Repr r a)
	 => (a -> a -> a)
	 -> a
	 -> Array r sh a
	 -> a
{-# INLINE [2] foldAllP #-}
foldAllP f z arr 
 = let  sh = extent arr
        n  = size sh
   in   unsafePerformIO $ E.foldAllP (\ix -> arr `unsafeIndex` fromIndex sh ix) f z n


-- sum --------------------------------------------------------------------------------------------
-- | Sequential sum the innermost dimension of an array.
sumS	:: (Shape sh, Num a, Elt a, Unbox a, Repr r a)
	=> Array r (sh :. Int) a
	-> Array U sh a
{-# INLINE [2] sumS #-}
sumS arr 
 = let  result   = foldS (+) 0 arr
   in   result `seq` result


-- | Sequential sum the innermost dimension of an array.
sumP	:: (Shape sh, Num a, Elt a, Unbox a, Repr r a)
	=> Array r (sh :. Int) a
	-> Array U sh a
{-# INLINE [2] sumP #-}
sumP arr 
 = let  result   = foldP (+) 0 arr
   in   result `seq` result


-- sumAll -----------------------------------------------------------------------------------------
-- | Sequential sum of all the elements of an array.
sumAllS	:: (Shape sh, Elt a, Unbox a, Num a, Repr r a)
	=> Array r sh a
	-> a
{-# INLINE [2] sumAllS #-}
sumAllS arr = foldAllS (+) 0 arr


-- | Parallel sum all the elements of an array.
sumAllP	:: (Shape sh, Elt a, Unbox a, Num a, Repr r a)
	=> Array r sh a
	-> a
{-# INLINE [2] sumAllP #-}
sumAllP arr = foldAllP (+) 0 arr
