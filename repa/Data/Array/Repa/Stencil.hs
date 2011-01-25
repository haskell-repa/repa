{-# LANGUAGE 	MagicHash, PatternGuards, BangPatterns, TemplateHaskell, QuasiQuotes, 
		ParallelListComp, TypeOperators #-}
{-# OPTIONS -Wnot #-}

-- | Efficient computation of stencil based convolutions.
--   TODO: Also handle stencils larger than 5x5.
module Data.Array.Repa.Stencil
	( Stencil	(..)
	, Boundary	(..)
	, makeStencil2
	, mapStencil2
	, stencil2)
where
import Data.Array.Repa			as R
import qualified Data.Array.Repa.Shape	as S
import qualified Data.Vector.Unboxed	as V
import Data.List			as List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Exts
import Debug.Trace

-- | Represents a stencil we can apply to an array.
data Stencil sh a b
	= StencilStatic
	{ stencilExtent	:: !sh
	, stencilZero	:: !b 
	, stencilAcc	:: !(sh -> a -> b -> b) }


-- | What values to use when the stencil is partly outside the input image.
data Boundary a
	-- | Treat points outside as having a constant value.
	= BoundConst a	

	-- | Treat points outside as having the same value as the edge pixel.
	| BoundClamp

makeStencil2
	:: (Elt a, Num a)
	=> Int -> Int		-- ^ extent of stencil
	-> (DIM2 -> Maybe a)	-- ^ Get the coefficient at this index.
	-> Stencil DIM2 a a

{-# INLINE makeStencil2 #-}
makeStencil2 height width getCoeff
 = makeStencil (Z :. height :. width) getCoeff


-- | Make a stencil from a function yielding coefficients at each index.
makeStencil
	:: (Elt a, Num a) 
	=> sh			-- ^ Extent of stencil.
	-> (sh -> Maybe a) 	-- ^ Get the coefficient at this index.
	-> Stencil sh a a

{-# INLINE makeStencil #-}
makeStencil ex getCoeff
 = StencilStatic ex 0 
 $ \ix val acc
	-> case getCoeff ix of
		Nothing		-> acc
		Just coeff	-> acc + val * coeff

-- | Apply a stencil to every element of an array.
--   This is specialised for stencils of extent up to 5x5.
mapStencil2 
	:: (Elt a, Elt b)
	=> Boundary a
	-> Stencil DIM2 a b
	-> Array DIM2 a -> Array DIM2 b

{-# INLINE mapStencil2 #-}
mapStencil2 boundary stencil@(StencilStatic sExtent zero load) arr
 = let	(_ :. aHeight :. aWidth) = extent arr
	(_ :. sHeight :. sWidth) = sExtent

	sHeight2	= sHeight `div` 2
	sWidth2		= sWidth  `div` 2

	-- minimum and maximum indicies of values in the inner part of the image.
	xMin		= sWidth2
	yMin		= sHeight2
	xMax		= aWidth  - sWidth2  - 1
	yMax		= aHeight - sHeight2 - 1
	
	-- range of values where we don't need to worry about the border
	rngInternal	= ( Z :. yMin :. xMin
			  , Z :. yMax :. xMax )

	-- range of values where some of the data needed by the stencil is outside the image.
	rngsBorder
	 = 	[ ((Z :. 0        :. 0),        (Z :. yMin -1        :. aWidth - 1))	-- bot 
	   	, ((Z :. yMax + 1 :. 0),        (Z :. aHeight - 1    :. aWidth - 1)) 	-- top
		, ((Z :. yMin     :. 0),        (Z :. yMax           :. xMin - 1))	-- left
	   	, ((Z :. yMin     :. xMax + 1), (Z :. yMax           :. aWidth - 1)) ]  -- right
			
	{-# INLINE getBorder' #-}
	getBorder' ix	= unsafeAppStencilBorder2   boundary stencil arr ix
			
	{-# INLINE getInner' #-}	
	getInner' ix	= unsafeAppStencilInternal2 stencil arr ix
						
   in	Partitioned (extent arr) (const False)
		rngsBorder  getBorder'
		rngInternal getInner'


-- | Apply a stencil to a single, internal position in an image.
--	Applying it too close to the border yields badness.
--	TODO: force delayed arrays before processing them.
unsafeAppStencilInternal2
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b -> Array DIM2 a -> DIM2 -> b

{-# INLINE unsafeAppStencilInternal2 #-}
unsafeAppStencilInternal2 
	stencil@(StencilStatic sExtent zero load)
	    arr@(Manifest aExtent vec) 
	     ix@(Z :. y :. x)

	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 5, sWidth <= 5
	= let	-- We want to go access the vector directly here so we don't 
		-- have to calculate incides for every access of the source array.
		!center	= x + y * aWidth

		-- Build a function to pass data from the array to our stencil.
		{-# INLINE oload #-}
		oload oy ox	
		 = let	!ix'	= Z :. oy :. ox
		   in	load ix' (vec `V.unsafeIndex` (center + ox + oy * aWidth))
	
	   in	template5x5 oload zero
	
	| otherwise
	= error "unsafeAppStencil2: finish this for larger stencils"
		

-- | Apply a stencil to a single position in an image.
--	This version can be applied close to the border, or even outside the image.
unsafeAppStencilBorder2
	:: (Elt a, Elt b)
	=> Boundary a
	-> Stencil DIM2 a b
	-> Array DIM2 a
	-> DIM2 -> b

{-# INLINE unsafeAppStencilBorder2 #-}
unsafeAppStencilBorder2	boundary
	 stencil@(StencilStatic  sExtent zero load)
	     arr@(Manifest aExtent vec)
	ix@(Z :. y :. x)
	
	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 5, sWidth <= 5
	= let
		{-# INLINE oload #-}
		-- takes the offset into the stencil, produces data from the input array.
		oload oy ox
		 = let	!yy		= y + oy
			!xx 		= x + ox
			!ixStencil	= Z :. oy :. ox
			!ixArray	= Z :. yy :. xx
			
			result
			 | False	<- isOutside2 ixArray arr
			 = load ixStencil (arr `unsafeIndex` ixArray)
			
			 | BoundConst bZero	<- boundary
			 = load ixStencil bZero
			
			 | BoundClamp		<- boundary
			 , ixArray_clamped	<- clampIxToBorder2 (extent arr) ixArray
			 = load ixStencil (arr `unsafeIndex` ixArray_clamped)
			
		   in	result		

	  in	template5x5 oload zero
	
	| otherwise
	= error "unsafeAppStencil2: finish this for larger stencils"


-- | Check if an index lies outside the given array.
isOutside2 :: DIM2 -> Array DIM2 a -> Bool
{-# INLINE isOutside2 #-}
isOutside2 (_ :. yy :. xx) arr
	| yy < 0		= True
	| yy >= height arr	= True
	| xx < 0		= True
	| xx >= width arr	= True
	| otherwise		= False


-- | Given the extent of an array, clamp the components of an index so they
--   lie within the given array. Outlying indices are clamped to the index
--   of the nearest border element.
clampIxToBorder2 
	:: DIM2 	-- ^ Extent of array.
	-> DIM2		-- ^ Index to clamp.
	-> DIM2
clampIxToBorder2 (_ :. yLen :. xLen) (sh :. j :. i)
 = clampX j i
 where 	{-# INLINE clampX #-}
	clampX !y !x
	  | x < 0	= clampY y 0
	  | x >= xLen	= clampY y (xLen - 1)
	  | otherwise	= clampY y x
		
	{-# INLINE clampY #-}
	clampY !y !x
	  | y < 0	= sh :. 0	   :. x
	  | y >= yLen	= sh :. (yLen - 1) :. x
	  | otherwise	= sh :. y	   :. x



-- split this out somewhere else ------------------------------------------------------------------
width :: Array (sh :. Int) a -> Int
{-# INLINE width #-}
width arr
 = let	_ :.  width		= extent arr
   in	width


height :: Array (sh :. Int :. Int) a -> Int
{-# INLINE height #-}
height arr
 = let	_ :. height :. _	= extent arr
   in	height


depth :: Array (sh :. Int :. Int :. Int) a -> Int
{-# INLINE depth #-}
depth arr
 = let	_ :. depth :. _ :. _	= extent arr
   in	depth
	


-- | Data template for stencils up to 5x5.
--   TODO: we probably don't need to statically define this if we're using TH to defined the stencils.
template5x5
	:: (Int -> Int -> a -> a)
	-> a -> a

{-# INLINE template5x5 #-}
template5x5 f zero
 	= f (-2) (-2)  $  f (-2) (-1)  $  f (-2)   0  $  f (-2)   1  $  f (-2)   2 
	$ f (-1) (-2)  $  f (-1) (-1)  $  f (-1)   0  $  f (-1)   1  $  f (-1)   2 
	$ f   0  (-2)  $  f   0  (-1)  $  f   0    0  $  f   0    1  $  f   0    2  
	$ f   1  (-2)  $  f   1  (-1)  $  f   1    0  $  f   1    1  $  f   1    2 
	$ f   2  (-2)  $  f   2  (-1)  $  f   2    0  $  f   2    1  $  f   2    2 
	$ zero



-- Template Haskell Stuff -------------------------------------------------------------------------
stencil2 :: QuasiQuoter
stencil2 = QuasiQuoter 
		{ quoteExp	= parseStencil2
		, quotePat	= undefined
		, quoteType	= undefined
		, quoteDec	= undefined }

-- | Parse a stencil definition.
--   TODO: make this more robust.
parseStencil2 :: String -> Q Exp
parseStencil2 str 
 = let	line1 : _	= lines str

	sizeX		= fromIntegral $ length $ lines str
	sizeY		= fromIntegral $ length $ words line1
	
	minX		= negate (sizeX `div` 2)
	minY		= negate (sizeY `div` 2)
	maxX		= sizeX `div` 2
	maxY		= sizeY `div` 2

	coeffs		= List.map read $ words str

	-- using scaled coeffs makes things slower because we were relying on values being == 1
	-- scaled coeffs would be better if we don't have a lot of 1s.
	sumCoeffs	= List.sum coeffs
--	scaledCoeffs	= List.map (/ sumCoeffs) coeffs
	
   in	makeStencil2' sizeX sizeY
	 $ filter (\(x, y, v) -> v /= 0)
	 $ [ (fromIntegral y, fromIntegral x, toRational v)
		| y	<- [minX, minX + 1 .. maxX]
		, x	<- [minY, minY + 1 .. maxY]
		| v	<- coeffs ]


makeStencil2'
	:: Integer -> Integer
	-> [(Integer, Integer, Rational)]
	-> Q Exp

makeStencil2' sizeX sizeY coeffs
 = do	let makeStencil' = mkName "makeStencil2"
	let dot'	 = mkName ":."
	let just'	 = mkName "Just"
	ix'		<- newName "ix"
	z'		<- [p| Z |]
	
	return 
	 $ AppE  (VarE makeStencil' `AppE` (LitE (IntegerL sizeX)) `AppE` (LitE (IntegerL sizeY)))
	 $ LamE  [VarP ix']
	 $ CaseE (VarE ix') 
	 $   [ Match	(InfixP (InfixP z' dot' (LitP (IntegerL oy))) dot' (LitP (IntegerL ox)))
			(NormalB $ ConE just' `AppE` LitE (RationalL v))
			[] | (oy, ox, v) <- coeffs ]
	  ++ [Match WildP 
			(NormalB $ ConE (mkName "Nothing"))
			[]]
		

