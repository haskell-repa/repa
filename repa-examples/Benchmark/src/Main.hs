
import Config
import Run
import System.Cmd
import System.Directory

	
-------------------------------------------------------------------------------
-- The benchmarks to run
--	The arg lists all need to end in  +RTS whatever
--
benchmarks :: [Benchmark]
benchmarks
 = 	[ Benchmark
		"mmult"
		Nothing
		"repa-examples/dist/build/repa-mmult/repa-mmult"
		(words "-random 1024 1024 -random 1024 1024 +RTS -qg")

	, Benchmark
		"laplace"
		(Just "mkdir -p bench/laplace")
		"repa-examples/dist/build/repa-laplace/repa-laplace"
		(words "1000 repa-examples/Laplace/data/pls-400x400.bmp output/laplace/out.bmp +RTS -qg")
		
	, Benchmark
		"fft2d-highpass"
		(Just "mkdir -p bench/fft2d-highpass")
		"repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		(words "1 repa-examples/FFT/data/lena.bmp output/fft2d-highpass/out.bmp +RTS")

	, Benchmark
		"fft3d-highpass"
		(Just "mkdir -p bench/fft3d-highpass")
		"repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass"
		(words "128 bench/fft3d-highpass/out +RTS")
	]


-------------------------------------------------------------------------------
main 
 = do	let config	= configDefault
	runBenchmarks config
	

-- Run the Matrix-Matrix multiply example.	
runBenchmarks :: Config -> IO ()
runBenchmarks config
 = do	verb config 	
 		$ unlines 
		[ "Repa Benchmarking"
		, "    max threads = " ++ (show $ configMaxThreads config) ]
	
	-- Run the benchmarks
	bs	<- mapM (runBench config) benchmarks		
	return ()



-- Get the path to this example
getPathToExample :: FilePath -> IO FilePath
getPathToExample name
 = do	mPath	<- findExecutable name
	case mPath of 
	 Just path	-> return path
	 Nothing	-> error $ "repa-benchmark: can't find executable for " ++ name
