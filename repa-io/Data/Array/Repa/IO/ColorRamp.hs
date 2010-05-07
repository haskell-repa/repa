{-# LANGUAGE RankNTypes #-}

-- | Hyprometric color ramps, for making pretty images from scalar data.
module	Data.Array.Repa.IO.ColorRamp
	(rampColorHotToCold)
where

-- | Standard Hot to Cold hypsometric color ramp.
--	Color sequence is red, yellow, green, cyan, blue.
rampColorHotToCold 
	:: forall a
	.  (Ord a, Floating a) 
	=> a 	-- ^ Minimum value of range.
	-> a 	-- ^ Maximum value of range.
	-> a 	-- ^ Data value.
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

