
module ColorRamp
	(rampColorHotToCold)
where


-- Color Ramps  -----------------------------------------------------------------------------------
-- | Standard Hot -> Cold hypsometric color ramp.
--	Sequence is red, yellow, green, cyan, blue.
rampColorHotToCold 
	:: (Ord a, Floating a) 
	=> a 
	-> a 
	-> a 
	-> (a, a, a)
	
{-# INLINE rampColorHotToCold #-}
rampColorHotToCold vmin vmax vNotNorm
 = let	
	v	| vNotNorm < vmin	= vmin
	 	| vNotNorm > vmax	= vmax
		| otherwise		= vNotNorm
	
	dv	= vmax - vmin	

	result	| v < vmin + 0.25 * dv
		= ( 0
		  , 4 * (v - vmin) / dv
		  , 1.0)
		
		| v < vmin + 0.5 * dv
		= ( 0
		  , 1.0
		  , 1 + 4 * (vmin + 0.25 * dv - v) / dv)
		
		| v < vmin + 0.75 * dv
		= ( 4 * (v - vmin - 0.5 * dv) / dv
		  , 1.0
		  , 0.0)
		
		| otherwise
		= ( 1.0
		  , 1 + 4 * (vmin + 0.75 * dv - v) / dv
		  , 0)
		
  in	result

