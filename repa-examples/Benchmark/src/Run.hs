
module Run
	( Benchmark(..)
	
	, BenchResult(..)
	, makeBenchResult
	, runBench

	, RunResult(..)
	, resultSpeedup
	, runExample
	, runCmd )
where

import Config
import System.Cmd
import System.Directory
import System.Process
import System.IO
import System.Posix.Process
import System.Posix.Types
import System.Posix.IO
import Data.List
import Data.Function

-- Benchmark ------------------------------------------------------------------
data Benchmark
	= Benchmark
	{ benchmarkName		:: String
	, benchmarkCommand	:: FilePath
	, benchmarkArgs		:: [String] }
	


-- Bench Result ---------------------------------------------------------------
data BenchResult
	= BenchResult
	{ benchResultName		:: String
	, benchResultRuns		:: [RunResult]

	  -- The minimum runtime we achieved, using max number of threads
	, benchResultMinElapsedMS	:: Int

	  -- The maximum speedup we achieved, using max number of threads
	, benchResultMaxSpeedup		:: Float }
	
makeBenchResult :: String -> [RunResult] -> BenchResult
makeBenchResult name runs@[r1, r2]
	= BenchResult
	{ benchResultName		= name
	, benchResultRuns		= runs
	, benchResultMinElapsedMS	= minimum $ map runResultElapsedTimeMS runs
	, benchResultMaxSpeedup		= resultSpeedup r1 r2 }


runBench 
	:: Config 
	-> Benchmark
	-> IO BenchResult

runBench config bench
 = do	
	let name	= benchmarkName bench
	let cmd		= benchmarkCommand bench
	let args	= benchmarkArgs bench
	
	verb config $ "** " ++ name
	result1		<- runExample config cmd (args ++ words "-N1")
	resultN		<- runExample config cmd (args ++ words ("-N" ++ (show $ configMaxThreads config)))
	let result	= makeBenchResult name [result1, resultN]

	verb config 
		$ unlines
		[ "name           = " ++ (show $ benchResultName         result)
		, "minElapsedMS   = " ++ (show $ benchResultMinElapsedMS result)
		, "maxSpeedup     = " ++ (show $ benchResultMaxSpeedup   result) ]
		
	return result


-- Single Runs ----------------------------------------------------------------
data RunResult 
	= RunResult
	{ runResultCommand		:: String
	, runResultArgs			:: [String]
	, runResultElapsedTimeMS	:: Int
	, runResultCpuTimeMS		:: Int
	, runResultStdout		:: String }

resultSpeedup :: RunResult -> RunResult -> Float
resultSpeedup result1 result2
	= (fromIntegral $ runResultElapsedTimeMS result1)
	/ (fromIntegral $ runResultElapsedTimeMS result2)


-- | Run an example, collecting runtime details from its stdout.
runExample
	:: Config		
	-> String 		-- ^ command
	-> [String] 		-- ^ command args
	-> IO RunResult

runExample config cmd args
 = do	verb config $ "runExample: " ++ unwords (cmd : args)
	strOut	<- runCmd config cmd args

	verb config 
		$  replicate 80 '-' ++ "\n"
		++ strOut
		++ replicate 80 '-' ++ "\n"
	
	let (lElapsed : _)	= lines strOut
	let elapsedTime		= tail $ dropWhile (/= '=') lElapsed
	
	return 	$ RunResult
		{ runResultCommand		= cmd 
		, runResultArgs			= args
		, runResultElapsedTimeMS	= read elapsedTime
		, runResultCpuTimeMS		= 0
		, runResultStdout		= strOut }
		

-- | Run a system command, collecting its stdout
runCmd
	:: Config
	-> String	-- ^ command
	-> [String]	-- ^ command args
	-> IO String

runCmd config cmd args
 = do
	(fdOut, fdIn)	<- createPipe
	hIn		<- fdToHandle fdIn
	hOut		<- fdToHandle fdOut

	-- run the command
 	ph	<- runProcess cmd args Nothing Nothing Nothing (Just hIn) Nothing 
	code	<- waitForProcess ph

	-- grab its stdout
	strOut	<- hGetContents hOut
	return strOut
