{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}
-- 3D FFT
-- Based on:
--  	On the Effectiveness of Functional Language Features: NAS benchmark FT, 1997
--	by J. Hammes, S. Sur, W. Böhm , W. B Ohm
--
module FFTCArray 
	( CArray, DIM3, Complex
	, calcRofu
	, fft3D, fft3DS, fft3DC, fft, fftS
	, dft)
where

import Data.Array.Parallel.Base ( (:*:)(..) )
import CArray 			as CA
import qualified Array		as A
import Array			((:.)(..), DIM1, DIM3, Shape(..), Subshape(..))

import Control.Exception ( assert )
import StrictComplex
import Prelude			as P


-- fft3D ------------------------------------------------------------------------------------------
fft3D:: Int -> CArray DIM3 Complex -> CArray DIM3 Complex 
fft3D it m@(CArray (sh :. n) _) =
  fft3d it (calcRofu (sh :. size)) m
  where
    size ::  Int
    size = n `div` 2

fft3d:: Int -> CArray DIM3 Complex -> CArray DIM3 Complex -> CArray DIM3 Complex
fft3d it rofu  m  
    | it < 1    = m
    | otherwise = fft3d (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = transpose . (fft rofu) 
    transpose arr = traverseCArray arr 
            (\(() :. k :. l :. m) -> (() :. l :. m :. k)) 
            (\f (() :. l :. m :. k) -> f (() :. k :. l :. m)) 




-- fft3DS -----------------------------------------------------------------------------------------
fft3DS:: Int -> CArray DIM3 Complex -> CArray DIM3 Complex 
fft3DS it m@(CArray (sh :. n) _) =
  fft3dS it (calcRofu (sh :. size)) m
  where
    size ::  Int
    size = n `div` 2

fft3dS:: Int -> CArray DIM3 Complex -> CArray DIM3 Complex -> CArray DIM3 Complex
fft3dS it rofu  m
    | it < 1    = m
    | otherwise = fft3dS (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = transpose . (fftS rofu) 
    transpose arr@(CArray (() :. k :. l :. m) _) = 
      backpermute arr (() :. m :. k :. l)
            (\(() :. m' :. k' :. l') -> (() :. k' :. l' :. m')) 



fftS	:: Array.Shape dim 
	=> CArray (dim :. Int) Complex 
	-> CArray (dim :. Int) Complex 
	-> CArray (dim :. Int) Complex 

fftS rofu@(CArray ( _ :. s) _ )  v@(CArray sh@(_ :. n) _) 
  | n > 2     = 
      append  (CA.zipWith (+) fft_left fft_right)
              (CA.zipWith (-) fft_left fft_right)
    
  | n == 2    = assert (2 * s == n) $
    traverseCArray v id f'
  where 
    f' f (sh :. 0) = f (sh :. 0) + f (sh :. 1)
    f' f (sh :. 1) = f (sh :. 0) - f (sh :. 1)
    f' f (sh :. x) = error ("error in fft - f:" ++ (show x) ++ "/" ++ (show sh))

    rofu'  = splitEvens rofu
    fft_left  = forceCArray $ fftS rofu' (splitEvens v)
    fft_right = forceCArray $ CA.zipWith (*) rofu (fftS rofu' (splitOdds v))



-- fft3DC -----------------------------------------------------------------------------------------
fft3DC	:: Int 					-- ^ Number of iterations to run for.
	-> CArray DIM3 Complex 			-- ^ Input data.
	-> CArray DIM3 Complex 

fft3DC it m@(CArray (sh :. n) _) =
  fft3dC it (calcRofu (sh :. size)) m
  where
    size ::  Int
    size = n `div` 2

fft3dC	:: Int 
	-> CArray DIM3 Complex 
	-> CArray DIM3 Complex
	-> CArray DIM3 Complex

fft3dC it rofu  m  
    | it < 1    = m
    | otherwise = fft3dC (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans 	= transpose . (fftC rofu)

    transpose arr@(CArray (() :. k :. l :. m) _) = 
      backpermute arr (() :. m :. k :. l)
            (\(() :. m' :. k' :. l') -> (() :. k' :. l' :. m')) 


fftC	:: Subshape dim dim 
	=> CArray (dim :. Int) Complex
        -> CArray (dim :. Int) Complex
        -> CArray (dim :. Int) Complex 

fftC rofu v
  | n <= 16 	= fft rofu v
  | n > 2   	= appendWithShape 
		     (CA.zipWith (+) fft_left fft_right)
                     (CA.zipWith (-) fft_left fft_right) sh
  where
    sh     	= carrayShape v
    _ :. n 	= sh

    rofu' 	= splitEvens rofu
    fft_left	= forceCArray $ CA.zipWith (*) rofu (fftC rofu' (splitEvens v))
    fft_right	= forceCArray $ fftC rofu' (splitOdds v)



-- FFT --------------------------------------------------------------------------------------------
-- | Compute the FFT of a vector
fft	:: Subshape dim  dim 
	=> CArray (dim :. Int) Complex		-- ^ Roots of unity.
	-> CArray (dim :. Int) Complex		-- ^ Input values.
        -> CArray (dim :. Int) Complex

fft rofu v
  | vLen > 2     
  = appendWithShape fft_l fft_r (carrayShape v)

  | vLen == 2    
  = assert (2 * rLen == vLen) 
  $ traverseCArray v id vFn'

  where 
    (_ :. vLen)		= carrayShape v
    (_ :. rLen)		= carrayShape rofu
    vFn' vFn (sh :. 0)  = vFn (sh :. 0) + vFn (sh :. 1)
    vFn' vFn (sh :. 1)  = vFn (sh :. 0) - vFn (sh :. 1)
    vFn' _   (sh :. x)  = error $ "error in fft - f:" ++ show x ++ "/" ++ show sh

    fft_lr 		= forceCArray $ fft splitRofu splitV -- par

    splitRofu 
	= traverseCArray rofu
        	(\(rSh :. rLen) 	-> rSh :. (2::Int) :. (rLen `div` 2))
        	(\rFn (sh :. _ :. i) 	-> rFn (sh :. 2*i))
 
    splitV    
	= traverseCArray v
		(\(vSh :. vLen) 	-> vSh :. 2 :. (vLen `div` 2)) 
		vFn'
        where 
		vFn' vFn (sh :. 0 :. i) = vFn (sh :. 2*i)
		vFn' vFn (sh :. 1 :. i) = vFn (sh :. 2*i+1)


    fft_l = traverse2CArray fft_lr rofu 
 		(\(sh :. 2 :. n) _ 	-> sh :. n)
		(\f r (sh :. i) 	-> f (sh :. 0 :. i) + r (sh :. i) * f (sh :. 1 :. i))

    fft_r = traverse2CArray fft_lr rofu 
		(\(sh :. 2 :. n) _ 	-> sh :. n)
		(\f r (sh :. i) 	-> f (sh :. 0 :. i) - r (sh :. i) * f (sh :. 1 :. i))


-- DFT --------------------------------------------------------------------------------------------
-- | Compute the DFT of an vector. (a non-fast FFT)
dft	:: Shape dim
	=> CArray (dim :. Int) Complex		-- ^ Roots of unity for this vector length.
	-> CArray (dim :. Int) Complex		-- ^ Input vector.
	-> CArray (dim :. Int) Complex

dft rofu arr
 = traverseCArray arr id (\_ k -> dftK rofu arr k)
 

-- | Compute one value of the DFT.
dftK	:: Shape dim
	=> CArray (dim :. Int) Complex 		-- ^ Roots of unity for this vector length.
	-> CArray (dim :. Int) Complex		-- ^ Input vector.
	-> (dim :. Int)				-- ^ Index of the value we want.
	-> Complex

dftK rofu arrX (_ :. k)
 = CA.sumAll $ CA.zipWith (*) arrX wroots
 where	sh@(_ :. len)	= carrayShape arrX

	-- All the roots we need to multiply with.
	wroots		= CArray sh $ Left elemFn
	elemFn (sh :. n) 
		= rofu !: (sh :. (k * n) `mod` len)


-- Roots of Unity ---------------------------------------------------------------------------------
-- | Fill a vector with roots of unity (Rofu)
calcRofu 
	:: Shape dim 
	=> (dim :. Int) 			-- ^ Length of resulting vector.
	-> CArray (dim :. Int) Complex

calcRofu sh@(_ :. n) 
 = forceCArray (genCArray sh f)
 where
    f :: Shape dim => (dim :. Int) -> Complex
    f (_ :. i) =      (cos  (2 * pi * (fromIntegral i) / len))
		:*: (- sin  (2 * pi * (fromIntegral i) / len))

    len	= fromIntegral $ A.size sh


-- Splitting --------------------------------------------------------------------------------------
split 	:: Shape sh 
	=> CArray (sh :. Int) Complex 
	-> (Int -> Int)
        -> CArray (sh :. Int) Complex
{-# INLINE split #-}
split arr sel 
	= traverseCArray arr 
		(\(sh :. i)   -> sh :. (i `div` 2))
                (\f (sh :. i) -> f (sh :. sel i))

splitEvens, splitOdds 
	:: Shape sh
        => CArray (sh :. Int) Complex 
	-> CArray (sh :. Int) Complex

{-# INLINE splitEvens #-}
{-# INLINE splitOdds #-}
splitEvens arr = split arr (2*)
splitOdds  arr = split arr (\i -> 2*i+1)

