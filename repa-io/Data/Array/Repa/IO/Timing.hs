module Data.Array.Repa.IO.Timing
	( Time
	, milliseconds, cpuTime, wallTime
	, time, minus, plus
	, showTime
	, prettyTime)
where
import GHC.Exts	(traceEvent)
import System.CPUTime
import System.Time


-- Time -----------------------------------------------------------------------
-- | Abstract representation of process time.
data Time 
	= Time 
	{ cpu_time  :: Integer
        , wall_time :: Integer
        }

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 wall1) (Time cpu2 wall2) 
	= Time (f cpu1 cpu2) (f wall1 wall2)

-- | Subtract second time from the first.
minus :: Time -> Time -> Time
minus = zipT (-)


-- | Add two times.
plus :: Time -> Time -> Time
plus  = zipT (+)


-- TimeUnit -------------------------------------------------------------------
-- | Conversion 
type TimeUnit 
	= Integer -> Integer

milliseconds :: TimeUnit
milliseconds n = n `div` 1000000000

cpuTime :: TimeUnit -> Time -> Integer
cpuTime f = f . cpu_time

wallTime :: TimeUnit -> Time -> Integer
wallTime f = f . wall_time


-- | Get the current time.
getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)


-- | Show a time as a string, in milliseconds.
showTime :: Time -> String
showTime t = (show $ wallTime milliseconds t)
          ++ "/"
          ++ (show $ cpuTime  milliseconds t)

-- | Pretty print the times, in milliseconds.
prettyTime :: Time -> String
prettyTime t
	= unlines
	[ "elapsedTimeMS   = " ++ (show $ wallTime milliseconds t)
	, "cpuTimeMS       = " ++ (show $ cpuTime  milliseconds t) ]

-- Timing benchmarks ----------------------------------------------------------

-- | Time some IO action.
--   Make sure to deepseq the result before returning it from the action. If you
--   don't do this then there's a good chance that you'll just pass a suspension
--   out of the action, and the computation time will be zero.
time :: IO a -> IO (a, Time)
{-# NOINLINE time #-}
time p = do
           start <- getTime
           traceEvent "Bench.Benchmark: start timing"
           x     <- p
           traceEvent "Bench.Benchmark: finished timing"
           end   <- getTime
           return (x, end `minus` start)
