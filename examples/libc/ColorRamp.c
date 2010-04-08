

// Color Ramps ------------------------------------------------------------------------------------
// Standard Hot -> Cold hypsometric color ramp.
//	Sequence is red, yellow, green, cyan, blue.
//	All values are clamped to [vmin .. vmax]
void rampColorHotToCold
	 (double v
	, double vmin		
	, double vmax
	, double* r		// color component outputs
	, double* g
	, double* b)
{
	if (v < vmin)	v = vmin;
	if (v > vmax)	v = vmax;
	double dv = vmax - vmin;

	if (v < (vmin + 0.25 * dv)) {
		*r = 0;
		*g = 4 * (v - vmin) / dv;
		*b = 1;
	} 
	else if (v < (vmin + 0.5 * dv)) {
		*r = 0;
		*g = 1;
		*b = 1 + 4 * (vmin + 0.25 * dv - v) / dv;
	}
	else if (v < (vmin + 0.75 * dv)) {
		*r = 4 * (v - vmin - 0.5 * dv) / dv;
		*g = 1;
		*b = 0;
	} 
	else {
		*r = 1;
		*g = 1 + 4 * (vmin + 0.75 * dv - v) / dv;
		*b = 0;
	}
}
