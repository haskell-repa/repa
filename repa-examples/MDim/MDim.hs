{-# LANGUAGE TypeOperators #-}
module MDim where

import Data.Array.Repa as R
import Data.Array.Parallel.Unlifted (Elt)
import Data.Array.Repa.Algorithms.Matrix as R
import Data.List as L
import Data.STRef
import Control.Monad(forM_)
import Control.Monad.ST(runST)

type Matrix a = [[a]]

type Correlation = Double

matrixProd :: Matrix Double -> Matrix Double -> Matrix Double
matrixProd [] _ = []
matrixProd (a:b) c = (L.map (skalarProd a) (transMatrix c)):(matrixProd b c)

skalarProd :: [Double] -> [Double] -> Double
skalarProd [] [] = 0
skalarProd (a1:b1) (a2:b2) = a1*a2 + skalarProd b1 b2

transMatrix :: Matrix Double -> Matrix Double
transMatrix ([]:_)= []
transMatrix a = (L.map head a):(transMatrix (L.map tail a))

cholesky :: Matrix Correlation -> Matrix Double
cholesky [[]]= [[]]
cholesky [[a]]= [[sqrt a]]
cholesky a = (q11:[0|x<-[2..l]]):(L.zipWith (:) q21 c)
  where
    l = length a
    c = cholesky s
    s = L.zipWith (\x y -> L.zipWith (-) x y) a22 (L.map (\x -> L.map (x*) a12) a21)
    a11 = head (head a)
    q11 = sqrt a11
    a12 = tail (head a)
    a21 = L.map (\x -> x/a11) a12
    q21 = L.map (\x -> x/q11) a12
    a22 = tail (L.map tail a)

invertMatrix :: Matrix Double -> Matrix Double
invertMatrix [[]] = [[]]
invertMatrix [[a]]= [[1/a]]
invertMatrix a = (q11:[0|x<-[2..l]]):(L.zipWith (++) q12 q22)
  where
    l = length a
    a11 = head (head a)
    q11 = 1/a11
    a12 = tail (L.map head a)
    q12 = matrixProd q22 (L.map (\x -> [-q11*x]) a12)
    a22 = tail (L.map tail a)
    q22 = invertMatrix a22


sigmaM :: [(Double, Double)] -> Matrix Correlation
          -> Matrix Correlation
sigmaM vola corr = matrixProd vn (matrixProd corr vn)
  where
    l = length vola
    v = L.map (\x -> snd x) vola
    vn = [hilfe x v | x<-[0..(l-1)]]
    hilfe n a = [0 | x<-[1..n]] ++ [a !! n] ++ [0 | x<-[(n+1)..(l-1)]]

-- step 2: decoupling X into Y
decouple :: Array DIM1 Double -> Array DIM1 Double
decouple x = y
  where
    -- Steffis Diss, S. 130 sagt Y = G^(-1) * X
    -- in Qians Code wird log auf X angewendet, warum?
    -- Begründung: S. 124 unten
    y       = dimDown $ R.multiplyMM gG $ dimUp $ R.map log x

dimUp :: Elt a => R.Array DIM1 a -> R.Array DIM2 a
dimUp arr = R.reshape arr (Z:.2:.1)

dimDown :: Elt a => R.Array DIM2 a -> R.Array DIM1 a
dimDown arr = R.reshape arr (Z:.2)


-- step 3: forward step
evaluation n = do
    -- preparations

    let alpha = 2

    -- step 2
    let y  = decouple startPrices

    -- step 3
    let yn = R.force $ R.fromFunction (Z:.n:.2) (initYn y)
    let temp = R.fromFunction (Z:.2:.2:.n) (initTemp yn)

    -- step 4
    let sn = R.fromFunction (Z:.n:.n:.2) (initSn temp)
    vr <- newSTRef $ R.fromFunction (Z:.n:.n) (initV sn g)

    -- step 5
    forM_ [n-1,n-2..1] $ \ k -> do
        v <- readSTRef vr
        let v' = R.fromFunction (Z:.k:.k) (newV v)
        vr `writeSTRef` v'

    -- return result
    v <- readSTRef vr
    return $ v !: (Z:.0:.0)

  where

    initTemp :: R.Array DIM2 Double
             -> DIM3
             -> Double
    initTemp yn (Z:.0:.0:.k) = (gg !: (Z:.0:.0)) * (yn !: (Z:.k:.0))
    initTemp yn (Z:.1:.0:.k) = (gg !: (Z:.1:.0)) * (yn !: (Z:.k:.0))
    initTemp yn (Z:.1:.1:.k) = (gg !: (Z:.1:.1)) * (yn !: (Z:.k:.1))

    initSn :: R.Array DIM3 Double
           -> DIM3
           -> Double
    initSn temp (Z:.k0:._ :.0) = exp $ temp!:(Z:.0:.0:.k0)
    initSn temp (Z:.k0:.k1:.1) = exp $ temp!:(Z:.1:.0:.k0) + temp!:(Z:.1:.1:.k0)

    initV :: R.Array DIM3 Double
          -> (Double -> Double -> Double)
          -> DIM2
          -> Double
    initV sn g (Z:.k0:.k1) = g (sn!:(Z:.k0:.k1:.0)) (sn!:(Z:.k0:.k1:.1))

    newV v (Z:.l0:.l1) = 0.25 * ((v!:(Z:.l0+1:.l1+1)) + (v!:(Z:.l0+1:.l1  ))
                               + (v!:(Z:.l0  :.l1+1)) + (v!:(Z:.l0  :.l1  )))

initYn :: R.Array (Z:.Int) Double
       -> (Z:.Int:.Int)
       -> Double
initYn y (Z:.0:.0) = y !: (Z:.0)
initYn y (Z:.0:.1) = y !: (Z:.1)
initYn y (Z:.k:.0) = (initYn y (Z:.(k-1):.0)) + u_y_0 + d_y_0
initYn y (Z:.k:.1) = (initYn y (Z:.(k-1):.1)) + u_y_1 + d_y_1
-- parameter

-- kombinierte Payoff-Funktion
g s0 s1 = s0 + s1

volatilities, startPrices :: R.Array (Z :. Int) Double
volatilities = R.fromList (Z:.2) [0.2, 0.25]
startPrices = R.fromList (Z:.2) [22, 20]

correlation :: R.Array DIM2 Double
correlation = R.fromList (Z:.2:.2) $ concat correlationL
correlationL = [ [1, 0.5], [0.5, 1]]

gg, gG :: Array DIM2 Double
gg = R.fromList (Z:.2:.2) . concat $
    cholesky
        (sigmaM (L.zipWith (,) (R.toList startPrices) (R.toList volatilities)) correlationL)
gG = R.fromList (Z:.2:.2) $ concat $ invertMatrix $ (\[a,b,c,d] -> [[a,b],[c,d]]) $ R.toList gg

rate = 0.1
lifeTime = 1
date = 10

rTime = lifeTime
tStep = date
deltaT = rTime / (toEnum tStep) :: Double
qDeltaT = sqrt deltaT

u_y_0 = 0.1
d_y_0 = 1/u_y_0
u_y_1 = 0.1
d_y_1 = 1/u_y_1

main = print $ runST $ evaluation 10

