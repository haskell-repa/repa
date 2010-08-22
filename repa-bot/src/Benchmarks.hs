
module Benchmarks where
import Config
import BuildBox	
import Control.Monad

-- | Repa benchmark configuration.
benchmarks :: Config -> [Benchmark]
benchmarks config
 = let	systemWithTimings' = systemWithTimings (configVerbose config)
   in	
	-- mmult
	[ let	mmult 	= "repa-examples/dist/build/repa-mmult/repa-mmult"
	  in	Benchmark
			"mmult"
			(test $ HasExecutable mmult)
			(systemWithTimings' $ mmult ++ " -random 1024 1024 -random 1024 1024 +RTS -N4 -qg")
			(return True)
	
	-- laplace
	, let	laplace = "repa-examples/dist/build/repa-laplace/repa-laplace"
		input	= "repa-examples/Laplace/data/pls-400x400.bmp"
		inputgz	= input ++ ".gz"

	  in  	Benchmark
		 	"laplace"
			(do	makeDirIfNeeded "output"
				check $ HasExecutable laplace
				whenM (test $ HasFile inputgz)
				 $ system $ "gzip -d " ++ inputgz
				test $ HasFile input)								

			(systemWithTimings' $ laplace ++ " 1000 " ++ input ++ " output/laplace.bmp +RTS -N4 -qg")
			(return True)

	-- fft2d-highpass
	, let	fft2d	= "repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		input	= "repa-examples/FFT/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	  in	Benchmark 
			"fft2d-highpass"
			(do	makeDirIfNeeded "output"
				check $ HasExecutable fft2d
				whenM (test $ HasFile inputgz)
				 $ system $ "gzip -d " ++ inputgz
				test $ HasFile input)

			(systemWithTimings' $ fft2d ++ " 1 " ++ input ++ " output/fft2d.bmp +RTS -N4 -qg")
			(return True)

	-- fft3d-highpass
	, let	fft3d	= "repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass"
	  in	Benchmark
			"fft3d-highpass"
			(do	makeDirIfNeeded "output/fft3d"
				test $ HasExecutable fft3d)
			(systemWithTimings' $ fft3d ++ " 128 " ++ " output/fft3d/slice +RTS -N4 -qg")
			(return True)			
	]


-- | Run a system command, expecing it to print the kernel timings to stdout.
systemWithTimings :: Bool -> String -> Build (Maybe Timing)
systemWithTimings verbose cmd
 = do	when verbose
	 $ outLn $ "\n    " ++ cmd
	result	<- systemWithStdout cmd
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
