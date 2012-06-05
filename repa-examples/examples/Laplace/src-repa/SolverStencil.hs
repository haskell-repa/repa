{-# LANGUAGE BangPatterns, TemplateHaskell, QuasiQuotes #-}
module SolverStencil
	(solveLaplace)
where	
import Data.Array.Repa				as A
import Data.Array.Repa.Stencil			as A
import Data.Array.Repa.Stencil.Dim2		as A
import qualified Data.Array.Repa.Shape		as S
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Solver for the Laplace equation.
solveLaplace
	:: Monad m
        => Int			-- ^ Number of iterations to use.
	-> Array U DIM2 Double	-- ^ Boundary value mask.
	-> Array U DIM2 Double	-- ^ Boundary values.
	-> Array U DIM2 Double	-- ^ Initial state.
	-> m (Array U DIM2 Double)

solveLaplace !steps !arrBoundMask !arrBoundValue !arrInit
 = go steps arrInit
 where 	go 0 !arr = return arr
	go n !arr 
         = do   arr'    <- relaxLaplace arr
                go (n - 1) arr'

        relaxLaplace arr
         = computeP
         $ A.szipWith (+) arrBoundValue
         $ A.szipWith (*) arrBoundMask
         $ A.smap (/ 4)
         $ mapStencil2 (BoundConst 0)
            [stencil2|   0 1 0
                         1 0 1 
                         0 1 0 |] arr

{-# NOINLINE solveLaplace #-}
