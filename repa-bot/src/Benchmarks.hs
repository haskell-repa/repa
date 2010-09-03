
module Benchmarks where
import Config
import BuildBox	
import Control.Monad

-- | Repa benchmark configuration.
benchmarksRepa :: Config -> [Benchmark]
benchmarksRepa config
 = let	systemWithTimings' = systemWithTimings (configVerbose config)
   in	
	-- mmult
	[ let	mmult 	= "repa-examples/dist/build/repa-mmult/repa-mmult"
	  in	Benchmark
			"repa-mmult"
			(return ())
			(systemWithTimings' $ mmult ++ " -random 1024 1024 -random 1024 1024 +RTS -N4 -qg")
			(return ())
	
	-- laplace
	, let	laplace = "repa-examples/dist/build/repa-laplace/repa-laplace"
		input	= "repa-examples/Laplace/data/pls-400x400.bmp"
		inputgz	= input ++ ".gz"

	  in  	Benchmark
		 	"repa-laplace"
			(do	ensureDir "output"
				check $ HasExecutable laplace
				whenM (test $ HasFile inputgz)
				 $ qssystem $ "gzip -d " ++ inputgz)

			(systemWithTimings' $ laplace ++ " 1000 " ++ input ++ " output/laplace.bmp +RTS -N4 -qg")
			(return ())

	-- fft2d-highpass
	, let	fft2d	= "repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		input	= "repa-examples/FFT/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	  in	Benchmark 
			"repa-fft2d-highpass"
			(do	ensureDir "output"
				check $ HasExecutable fft2d
				whenM (test $ HasFile inputgz)
				 $ qssystem $ "gzip -d " ++ inputgz)

			(systemWithTimings' $ fft2d ++ " 1 " ++ input ++ " output/fft2d.bmp +RTS -N4 -qg")
			(return ())

	-- fft3d-highpass
	, let	fft3d	= "repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass"
	  in	Benchmark
			"repa-fft3d-highpass"
			(ensureDir "output/fft3d")
			(systemWithTimings' $ fft3d ++ " 128 " ++ " output/fft3d/slice +RTS -N4 -qg")
			(return ())			
	]


-- | DPH benchmark configuation.
benchmarksDPH :: Config -> [Benchmark]
benchmarksDPH config
 = let	systemWithTimings' = systemWithTimings (configVerbose config)
   in	
	-- quickhull
	[ let	quickhull = "dph-examples/dist/build/dph-quickhull/dph-quickhull"
	  in	Benchmark
			"dph-quickhull"
			(return ())
			(systemWithTimings' $ quickhull ++ " 1000000 -N4")
			(return ())
	]


-- | Run a system command, expecing it to print the kernel timings to stdout.
systemWithTimings :: Bool -> String -> Build (Maybe Timing)
systemWithTimings verbose cmd
 = do	when verbose
	 $ outLn $ "\n    " ++ cmd
	result	<- qssystemOut cmd
	return	$ Just $ parseTimings result


-- | Parse kernel timings from a repa example program.
--   Format is  elapsedTime/systemTime  in milliseconds.
parseTimings :: String -> Timing
parseTimings str
 = let	(lElapsed : _)	= lines str
	elapsedTime	= tail $ dropWhile (/= '=') lElapsed
   in	Timing	(Just $ (read elapsedTime) / 1000)
		Nothing
		Nothing
