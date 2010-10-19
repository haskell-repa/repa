
module Benchmarks where
import Config
import BuildBox	
import Control.Monad
import qualified BuildBox.Data.Log	as Log

-- | DPH benchmark configuation.
benchmarksDPH :: Config -> [Benchmark]
benchmarksDPH config
 =	
 	[ -- dot product
	  bench config
		"dph-dotp"
		"dph-examples/dist/build/dph-dotp/dph-dotp dph 10000000 +RTS -N4"
		
	, bench config
		"dph-dotp[vector-seq]"
		"dph-examples/dist/build/dph-dotp/dph-dotp vector 10000000 +RTS -N4"

	  -- sum of squares
	, bench config
		"dph-sumsq"
		"dph-examples/dist/build/dph-sumsq/dph-sumsq dph 100000000 +RTS -N4"
		
	, bench config
		"dph-sumsq[vector-seq]"
		"dph-examples/dist/build/dph-sumsq/dph-sumsq vector 100000000 +RTS -N4"

	  -- evens
        , bench config
		"dph-evens"
		"dph-examples/dist/build/dph-evens/dph-evens 10000000 +RTS -N4"
	
	  -- quicksort
	, bench config 
		"dph-quicksort"
		"dph-examples/dist/build/dph-quicksort/dph-quicksort 100000 +RTS -N4"

	  -- quickhull 
	, bench config 
		"dph-quickhull"
		"dph-examples/dist/build/dph-quickhull/dph-quickhull 1000000 +RTS -N4"

	, bench config
		"dph-quickhull[vector-seq]"
		"dph-examples/dist/build/dph-quickhull-vector/dph-quickhull-vector vector 1000000"

	, bench config
		"dph-quickhull[vector-forkIO]"
		"dph-examples/dist/build/dph-quickhull-vector/dph-quickhull-vector io 1000000 +RTS -N4"

	, benchUp config
	 	"dph-quickhull[c-seq]"
		(inDir "dph-examples/spectral/QuickHull/c" $ qssystem "make")
		"dph-examples/spectral/QuickHull/c/quickhull 1000000"				
	]


-- | Repa benchmark configuration.
benchmarksRepa :: Config -> [Benchmark]
benchmarksRepa config
 =	-- mmult
	[ bench config
		"repa-mmult"
		"repa-examples/dist/build/repa-mmult/repa-mmult -random 1024 1024 -random 1024 1024 +RTS -N4"
	
	, benchUp config
		"repa-mmult[c-seq]"
		(inDir "repa-examples/MMult/legacy" $ qssystem "make")
		"repa-examples/MMult/legacy/mmult -random 1024 1024 -random 1024 1024"
	
	-- laplace
	, let	laplace = "repa-examples/dist/build/repa-laplace/repa-laplace"
		input	= "repa-examples/Laplace/data/pls-400x400.bmp"
		inputgz	= input ++ ".gz"

	  in  	benchUp config
		 	"repa-laplace"
			(do	ensureDir "output"
				check $ HasExecutable laplace
				whenM (test $ HasFile inputgz)
				 $ qssystem $ "gzip -d " ++ inputgz)

			(laplace ++ " 1000 " ++ input ++ " output/laplace.bmp +RTS -N4 -qg")

	, benchUp config
		"repa-laplace[c-seq]"
		(inDir "repa-examples/Laplace/legacy" $ qssystem "make")
		"repa-examples/Laplace/legacy/laplace 400 400 1000 output/laplace_c-seq.ppm"

	-- blur
	, let	blur	= "repa-examples/dist/build/repa-blur/repa-blur"
		input	= "repa-examples/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	  in	benchUp config
			"repa-blur"
			(do	ensureDir "output"
				check $ HasExecutable blur
				whenM (test $ HasFile inputgz)
				 $ qssystem $ "gzip -d " ++ inputgz)
			(blur ++ " 5 " ++ input ++ " output/lena-blur.bmp +RTS -N4 -qg")

	-- edgedetect
	, let	edgedetect = "repa-examples/dist/build/repa-edgedetect/repa-edgedetect"
		input	   = "repa-examples/data/lena.bmp"
		inputgz	   = input ++ ".gz"
		
	  in	benchUp config
			"repa-edgedetect"
			(do	ensureDir "output"
				check $ HasExecutable edgedetect
				whenM (test $ HasFile inputgz)
				 $ qssystem $ "gzip -d " ++ inputgz)
			(edgedetect ++ " " ++ input ++ " output/lena-edgedetect.bmp +RTS -N2 -qg")

	-- fft2d-highpass
	, let	fft2d	= "repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		input	= "repa-examples/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	  in	benchUp config
			"repa-fft2d"
			(do	ensureDir "output"
				check $ HasExecutable fft2d
				whenM (test $ HasFile inputgz)
				 $ qssystem $ "gzip -d " ++ inputgz)
			(fft2d ++ " 1 " ++ input ++ " output/fft2d.bmp +RTS -N4 -qg")

	-- fft3d-highpass
	, benchUp config
		"repa-fft3d"
		(ensureDir "output/fft3d")
		"repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass 128 output/fft3d/slice +RTS -N4 -qg"
	]


-- | Define a plain benchmark with no setup or teardown command
bench :: Config -> String -> String -> Benchmark
bench config name cmd
 = Benchmark
	name
	(return ())
	(systemWithTimings (configVerbose config) cmd)
	(return ())


-- | Define a benchmark with a setup command
benchUp :: Config -> String -> Build () -> String -> Benchmark
benchUp config name cmdUp cmdBench
 = Benchmark
	name
	cmdUp
	(systemWithTimings (configVerbose config) cmdBench)
	(return ())

	
-- | Run a system command, expecing it to print the kernel timings to stdout.
--   We ignore whatever is printed to stderr.
systemWithTimings :: Bool -> String -> Build (Maybe Timing)
systemWithTimings verbose cmd
 = do	when verbose
	 $ outLn $ "\n    " ++ cmd

	(code, logOut, logErr)
		<- systemTeeLog False cmd Log.empty 

	if code == ExitSuccess
	 then	return	$ Just $ parseTimings (Log.toString logOut)
	 else	throw   $ ErrorSystemCmdFailed cmd code logOut logErr


-- | Parse kernel timings from a repa example program.
--   Format is  elapsedTime/systemTime  in milliseconds.
parseTimings :: String -> Timing
parseTimings str
 = let	(lElapsed : _)	= lines str
	elapsedTime	= tail $ dropWhile (/= '=') lElapsed
   in	Timing	(Just $ (read elapsedTime) / 1000)
		Nothing
		Nothing
