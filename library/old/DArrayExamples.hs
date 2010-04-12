{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, TypeSynonymInstances #-}


module DArrayExamples ( 
    transpose
  , mmMult
  , mmMult'
  , mmMultP'
  , relaxMS
  , relaxShift
  , redBlack
  , redBlackChecker2D
  , calcRofu
  , fft
  , fft3D 
  , fft3DS
  , fft3DC
  , Complex (..)
  ) where

import qualified Data.Array.Parallel.Unlifted as U
import  Data.Array.Parallel.Unlifted ((:*:)(..))

import qualified Array 
import Array ((:.)(..))
import DArray

import Prelude hiding (map, zip, zipWith, replicate, sum)
import qualified Prelude (map, sum)

import Debug.Trace
import Control.Exception (assert)


mmMult' m1 m2 = -- fromDArray $ map (\(x :. y) -> x+y) $ calcRofu ((() :. 1000 :. 5000):: ():. Int :. Int)
    fromDArray $ mmMult m1 m2 


mmMultP' m1 m2 = fromDArray $ mmMultP m1 m2 


-- the polymorhic version is significantly slower (not surprisingly), but so is the (pragma) specialised version. Why???
{-# SPECIALIZE mmMult::
   DArray (() :. Int :. Int)  Double -> DArray (() :. Int :. Int)  Double -> DArray (() :. Int :. Int)  Double  
  #-}


--mmMult:: (Array.Shape dim) => 
--  DArray (dim :. Int :. Int)  Double -> DArray (dim :. Int :. Int)  Double -> DArray (dim :. Int :. Int)  Double  
mmMult::
   DArray (() :. Int :. Int)  Double -> DArray (() :. Int :. Int)  Double -> DArray (() :. Int :. Int)  Double  
mmMult arr1@(DArray (sh :. m1 :. n1) fn1) arr2@(DArray (sh' :. m2 :. n2) fn2) = 
  assert ((m1 == n2) && (sh == sh')) $ 
    fold (+) 0 (arr1Ext * arr2Ext)
  where
    arr2T   = forceDArray $ transpose arr2  -- forces evaluation of 'transpose'
    arr1Ext = replicateSlice arr1 ((Array.Any sh) :. Array.All :. m2 :. Array.All)
    arr2Ext = replicateSlice arr2T ((Array.Any sh) :. n1 :. Array.All :. Array.All)



mmMultP:: 
  DArray (() :. Int :. Int)  Double -> DArray (() :. Int :. Int)  Double -> DArray (() :. Int :. Int)  Double  
mmMultP arr1 arr2 =
  fold (+) 0 arr'
  where 
    arrT = forceDArray $ transpose arr2
    arr' = traverse2DArray arr1 arrT 
      (\(sh :. m1 :. n1) -> \(_ :. n2 :. m2) -> (sh :. m1 :. n2 :. n1))
      (\f1 -> \f2 -> \(sh :. i :. j :. k) -> f1 (sh :. i :. k) * f2 (sh :. j :. k))

{-
    arr' = DArray (sh :. m1 :. n2 :.n1) 
       (\(sh :. i :. j :. k) -> (index arr1 (sh :. i :. k)) * (index arrT (sh :. j :. k)))
 -}




relaxShift:: DArray Array.DIM2 Double -> Array.Array Array.DIM2 Double
{-# INLINE relaxShift #-}
relaxShift arr =  fromDArray $ map (\(a :*: b :*: c :*: d :*: e) -> (a+b+c+d+e)/5) $ 
    zip (zip (zip (zip shiftu shiftd) shiftl) shiftr) arr
  where
    s@(DArray ((() :. n) :. m) _) = arr
    shiftu = shift arr 0 ((():. 1   :.0)::Array.DIM2)
    shiftd = shift arr 0 ((():.(-1) :.0)::Array.DIM2)
    shiftl = shift arr 0 ((():. 0   :.1)::Array.DIM2)
    shiftr = shift arr 0 ((():. 0   :.(-1))::Array.DIM2)


relaxMS 0 arr = arr
relaxMS n arr = relaxMS (n-1) (forceDArray $ relaxMS' arr)


relaxMS':: Array.Shape dim => DArray (dim :. Int :. Int) Double -> DArray (dim :. Int :. Int) Double
{-# INLINE relaxMS #-}
relaxMS' arr@(DArray (sh :. n :.m) fn) = 
  stencilFn arr 
  where
    (d :.n :. m)  = darrayShape arr
    isBorder (d :. i :. j) = ((i * j) == 0)  || 
      (i >= (m-1)) || (j >= (n-1))

    stencilFn (DArray sh f)   = 
        DArray sh (\d@(sh :. n :. m) -> if (isBorder d)
                                                 then f d 
                                                 else ((f (sh :. n :. m+1) + 
                                                         f (sh :. n :. m-1) +
                                                           f (sh :. n+1 :. m) +
                                                             f (sh :. n-1 :. m)))/4)


{-
  mapStencil border (() :. 5) stencil id sumD arr
  where
    sumD arr = (toScalar $ (fold (+) 0 arr))/5
--    border:: Array.DIM2 -> Bool
    border  (_ :. n' :. m') = (n' == 0) || (n' >= n) ||
                                (m' == 0) || (m' >= m) 

--    stencil:: Array.DIM2 -> Array.DIM1 -> Array.DIM2
    stencil (sh :. i :.j) (sh' :. 0) = (sh :. (i-1) :. j)
    stencil (sh :. i :.j) (sh' :. 1) = (sh :. i :. (j-1))
    stencil (sh :. i :.j) (sh' :. 2) = (sh :. (i+1) :. j)
    stencil (sh :. i :.j) (sh' :. 3) = (sh :. i :. (j+1))
    stencil (sh :. i :.j) (sh' :. 4) = (sh :. i :. j)
  -}  


--  Red/Black 3d relaxation
--  -----------------------

redBlack:: Double -> Double -> DArray (() :. Int :. Int :. Int) Double ->
             DArray  (() :. Int :. Int :. Int) Double -> DArray (() :. Int :. Int :. Int) Double
redBlack factor hsq f arr@(DArray (d :. l :. n :. m) fn)  = 
    applyFactor $ stencilFn odd $ forceDArray $ applyFactor $ stencilFn even arr 
  where
    applyFactor = zipWith (\fi -> \si -> factor *  (hsq * fi + si)) f
    
    isBorder (d :. h :. i :. j) = ((h * i * j) == 0)  || 
      (h >= (l-1)) || (i >= (m-1)) || (j >= (n-1))

    stencilFn p (DArray sh f)   = 
        DArray sh (\d@(sh :. k :. n :. m) -> if (p m || isBorder d)
                                                 then f d 
                                                 else (f (sh :. k :. n :. m+1) + 
                                                         f (sh :. k  :. n :. m-1) +
                                                           f (sh :. k :. n+1 :. m) +
                                                             f (sh :. k :. n-1 :. m) +
                                                               f (sh :. k+1 :. n :. m) +
                                                                 f (sh :. k-1 :. n :. m)))


redBlackChecker2D:: Int -> Double -> Double -> DArray (() :. Int :. Int) Double ->
             DArray  (() :. Int :. Int) Double -> DArray (() :. Int :. Int) Double
redBlackChecker2D 0 factor hsq f arr =  arr
redBlackChecker2D n factor hsq f arr = 
  applyFactor $ stencilFn odd $ forceDArray $ applyFactor $ stencilFn even arr 
  where
    (d :.n :. m)  = darrayShape arr
    applyFactor = zipWith (\fi -> \si -> factor *  (hsq * fi + si)) f
    
    isBorder (d :. i :. j) = ((i * j) == 0)  || 
      (i >= (m-1)) || (j >= (n-1))

    stencilFn p (DArray sh f)   = 
        DArray sh (\d@(sh :. n :. m) -> if (p (m+n) || isBorder d)
                                                 then f d 
                                                 else (f (sh :. n :. m+1) + 
                                                         f (sh :. n :. m-1) +
                                                           f (sh :. n+1 :. m) +
                                                             f (sh :. n-1 :. m)))

    


--  FFT example
-- -------------

type Complex = (Double :*: Double)

instance Num Complex where
  (r :*: i) + (r' :*: i') = (r+r' :*: i+i')
  (r :*: i) - (r' :*: i') = (r-r' :*: i-i')
  (r :*: i) * (r' :*: i') = (r*r' - i*i' :*: r*i' + r'*i)
  fromInteger n = (fromInteger n :*: 0.0)


calcRofu:: Array.Shape dim =>  (dim :. Int) -> DArray (dim :. Int) Complex
calcRofu sh@(_ :. n) = forceDArray $ mkDArray sh f
  where
    f :: Array.Shape dim => (dim :. Int) -> Complex
    f (_ :. n) = ((cos (2 * pi/ ((fromIntegral n)+1))) :*: (sin  (2 * pi / ((fromIntegral n)+1))))


--  Three versions of fft, which only differ in the degree to which the main fft function exploits parallelism
-- fftS: only intra-function parallelism
-- fft: intra and inter (recursive calls to fft done in par)
-- fftC: switches between the former and the latte depending on the size of the input vector

-- Calculates a vector of unity roots and calls 3D fft
fft3D:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex 
fft3D it m =
  fft3d it (calcRofu (sh :. size)) m
  where
    (sh :. n) = darrayShape m
    size ::  Int
    size = n `div` 2

fft3d:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
fft3d it rofu  m  
    | it < 1    = m
    | otherwise = fft3d (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = transpose' . (fft rofu) 
    transpose' darr = traverseDArray darr 
            (\(() :. k :. l :. m) -> (() :. l :. m :. k)) 
            (\f -> \(() :. l :. m :. k) -> f (() :. k :. l :. m)) 

fft:: Array.Subshape dim  dim => 
  DArray (dim :. Int) Complex -> DArray (dim :. Int) Complex -> DArray (dim :. Int) Complex 
fft rofu   v
  | vLen > 2     = 
      append fft_l fft_r  (darrayShape v)
  | vLen == 2    = assert (2 * rLen == vLen) $ 
        traverseDArray v id vFn'
  | otherwise =  error ("error in fft  - length < 2" ++ (show vLen) ++ " \n")
  where 
    (_ :. vLen) = darrayShape v
    (_ :. rLen) = darrayShape rofu
    vFn' vFn (sh :. 0)  = vFn (sh :. 0) + vFn (sh :. 1)
    vFn' vFn (sh :. 1)  = vFn (sh :. 0) - vFn (sh :. 1)
    vFn' _   (sh :. x)  = error ("error in fft - f:" ++ (show x) ++ "/" ++ (show sh))


    fft_lr = forceDArray $ fft splitRofu splitV -- par

    splitRofu = 
      traverseDArray rofu (\(rSh :. rLen) -> (rSh :. (2::Int) :. ((rLen `div` 2)::Int)))
        (\rFn -> (\(sh :. _ :. i) -> rFn (sh :. 2*i)))
 
    splitV = traverseDArray v
      (\(vSh :. vLen) -> (vSh :. 2 :. (vLen `div` 2))) vFn'
       where 
         vFn' vFn (sh :. 0 :. i) = vFn (sh :. 2*i)
         vFn' vFn (sh :. 1 :. i) = vFn (sh :. 2*i+1)


    fft_l = traverse2DArray fft_lr rofu 
             (\(sh :. 2 :. n) -> \_ -> (sh :. n))
             (\f -> \r -> \(sh :. i) -> f (sh:. 0 :. i) + r (sh :. i) * f (sh :. 1 :. i))

    fft_r = traverse2DArray fft_lr rofu 
             (\(sh :. 2 :. n) -> \_ -> (sh :. n))
             (\f -> \r -> \(sh :. i) -> f (sh:. 0 :. i) - f (sh :. 1 :. i))

--
-- Calculates a vector of unity roots and calls 3D fft
fft3DS:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex 
fft3DS it m@(DArray (sh :. n) _) =
  fft3dS it (calcRofu (sh :. size)) m
  where
    size ::  Int
    size = n `div` 2

fft3dS:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
fft3dS it rofu  m | it < 1    = m
                  | otherwise = fft3dS (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = transpose' . (fftS rofu) 
    transpose' darr@(DArray (() :. k :. l :. m) _) = 
      backpermute darr (() :. m :. k :. l)
            (\(() :. m' :. k' :. l') -> (() :. k' :. l' :. m')) 


fftS:: Array.Shape dim => 
  DArray (dim :. Int) Complex -> DArray (dim :. Int) Complex -> DArray (dim :. Int) Complex 
fftS rofu@(DArray ( _ :. s) _ )  v@(DArray sh@(_ :. n) f) 
  | n > 2     = 
      append' (fft_left + fft_right) (fft_left - fft_right) -- (s)
--      append (fft_left + fft_right) (fft_left - fft_right) sh -- (s)
    
  | n == 2    = assert (2 * s == n) $ 
    DArray sh f'
  where 
    f' (sh :. 0) = f (sh :. 0) + f (sh :. 1)
    f' (sh :. 1) = f (sh :. 0) - f (sh :. 1)
    f' (sh :. x) = error ("error in fft - f:" ++ (show x) ++ "/" ++ (show sh))

    rofu'  = split rofu (\(sh:. i) -> (sh :. 2*i))
    fft_left  = forceDArray $ (fftS rofu' (split v (\(sh:. i) -> (sh :. (2*i)))))
    fft_right = forceDArray $ (rofu * (fftS rofu' (split v (\(sh:. i) -> (sh :. (2*i+1))) )))

    split (DArray (sh :. n) f) sel =
       DArray (sh :. (n `div` 2)) (\sh -> f (sel sh)) 



fft3DC:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex 
fft3DC it m@(DArray (sh :. n) _) =
  fft3dC it (calcRofu (sh :. size)) m
  where
    size ::  Int
    size = n `div` 2

fft3dC:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
fft3dC it rofu  m  
    | it < 1    = m
    | otherwise = fft3dC (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = transpose' . (fftC rofu) 
    transpose' darr@(DArray (() :. k :. l :. m) _) = 
      backpermute darr (() :. m :. k :. l)
            (\(() :. m' :. k' :. l') -> (() :. k' :. l' :. m')) 


fftC:: Array.Subshape dim  dim => 
  DArray (dim :. Int) Complex -> DArray (dim :. Int) Complex -> DArray (dim :. Int) Complex 
fftC rofu@(DArray ( _ :. s) _ )  v@(DArray sh@(_ :. n) f) 
  | n <=  16  = fft rofu v
  | n > 2     = 
      append (fft_left + fft_right) (fft_left - fft_right) sh -- (s)
  where 

    rofu'  = split rofu (\(sh:. i) -> (sh :. 2*i))
    fft_left  = forceDArray $ rofu * (fftC rofu' (split v (\(sh:. i) -> (sh :. (2*i)))))
    fft_right = forceDArray $ fftC rofu' (split v (\(sh:. i) -> (sh :. (2*i+1))) )

    split (DArray (sh :. n) f) sel =
       DArray (sh :. (n `div` 2)) (\sh -> f (sel sh)) 


append':: Array.Shape sh => DArray (sh :. Int) e -> DArray (sh :. Int) e -> DArray (sh :. Int) e 
append'  arr1 arr2 =
  traverse2DArray arr1 arr2 shFn f
  where
    (_ :. n) = darrayShape arr1
    (_ :. m) = darrayShape arr2
    shFn = \(sh :. n) -> \(_  :. m) -> (sh :. (n+m)) 
    f = \f1 -> \f2 -> \(sh :. i) ->  
          if (i < n) 
            then f1 (sh :. i)
            else f2 (sh :. (i-n))