
import Config
import Run
import System.Cmd
import System.Directory

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
	
	bMMult		<- runBench config 
				"mmult 1024x1024"
				"repa-examples/dist/build/repa-mmult/repa-mmult"
				$ words "-random 1024 1024 -random 1024 1024 +RTS -qg"

	bLaplace	<- runBench config 
				"laplace 1000,400x400"
				"repa-examples/dist/build/repa-laplace/repa-laplace"
				$ words "1000 repa-examples/Laplace/data/pls-400x400.bmp bench-laplace-out.bmp +RTS -qg"

		
	return ()



-- Get the path to this example
getPathToExample :: FilePath -> IO FilePath
getPathToExample name
 = do	mPath	<- findExecutable name
	case mPath of 
	 Just path	-> return path
	 Nothing	-> error $ "repa-benchmark: can't find executable for " ++ name
