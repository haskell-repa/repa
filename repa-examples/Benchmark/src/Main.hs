
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
		"repa-examples/dist/build/repa-mmult/repa-mmult"
		(words "-random 1024 1024 -random 1024 1024 +RTS -qg")

	, Benchmark
		"laplace"
		"repa-examples/dist/build/repa-laplace/repa-laplace"
		(words "1000 repa-examples/Laplace/data/pls-400x400.bmp bench-laplace-out.bmp +RTS -qg")
		
	, Benchmark
		"fft2d-highpass"
		"repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		(words "1 repa-examples/FFT/data/lena.bmp bench-fft2d-highpass-out.bmp +RTS")
	]


-------------------------------------------------------------------------------
main 
 = do	let config	= configDefault
	runMMult config
	

-- Run the Matrix-Matrix multiply example.	
runMMult :: Config -> IO ()
runMMult config
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
