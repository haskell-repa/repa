
-- | Build stages concerning Repa.
module BuildRepa
	( repaUnpack
	, repaBuild
	, repaTest
	, BuildResults(..))
where
import Benchmarks
import Config
import BuildBox
import Control.Monad


-- Unpack -----------------------------------------------------------------------------------------
-- | Download the Repa package from code.haskell.org,
repaUnpack :: Config -> Build ()
repaUnpack config
 = do	outCheckOk "* Checking Google is reachable"
	 $ HostReachable "www.google.com"

	outCheckOk "* Checking code.haskell.org is reachable"
	 $ HostReachable "code.haskell.org"
	
	outCheckOk "* Checking code.haskell.org web server is up"
	 $ UrlGettable "http://code.haskell.org"
	
	out "\n"
	inDir (configScratchDir config)
	 $ do	clobberDir "repa-head"
		
		outLn  "* Getting Darcs Package"
		ssystem "darcs get http://code.haskell.org/repa/repa-head"
	


-- Building ---------------------------------------------------------------------------------------	
-- | Build the packages and register them with the given compiler.
repaBuild :: Config -> Build ()
repaBuild config
 = inDir (configScratchDir config)
 $ inDir "repa-head"
 $ do	outLn "* Building Packages"

	mapM_ (repaBuildPackage True config)
		[ "repa"
		, "repa-bytestring"
		, "repa-io"
		, "repa-algorithms"]

	repaBuildPackage False config "repa-examples"


repaBuildPackage :: Bool -> Config -> FilePath -> Build ()
repaBuildPackage install config dirPackage
 = inDir dirPackage
 $ do	outLine

	ssystem	$ "runghc Setup.hs clean"
	ssystem	$ "runghc Setup.hs configure"
		++ " --user "
		++ " --with-compiler=" ++ configWithGhc config
		++ " --with-hc-pkg="   ++ configWithGhcPkg config
		
	ssystem	"runghc Setup.hs build"

	when install
	 $ ssystem "runghc Setup.hs install"

	outBlank
	
	
-- Testing ----------------------------------------------------------------------------------------
data BuildResults
	= BuildResults
	{ buildResultTime		:: UTCTime
	, buildResultEnvironment	:: Environment
	, buildResultBench		:: [BenchResult] }
	deriving (Show, Read)

instance Pretty BuildResults where
 ppr results
	= hang (ppr "BuildResults") 2 $ vcat
	[ ppr "time: " <> (ppr $ buildResultTime results)
	, ppr $ buildResultEnvironment results
	, ppr ""
	, vcat 	$ punctuate (ppr "\n") 
		$ map ppr 
		$ buildResultBench results ]

-- | Run regression tests.	
repaTest :: Config -> Environment -> Build ()
repaTest config env
 = do	outLn "* Running regression tests"
	
	-- Get the current time.
	utcTime	<- io $ getCurrentTime

	-- Load the baseline file if it was given.
	mBaseline <- case configAgainstResults config of
			Nothing		-> return Nothing
			Just fileName
			 -> do	file	<- io $ readFile fileName
				return	$ Just file
				
	let resultsPrior
		= maybe []
			(\contents -> buildResultBench $ read contents)
			mBaseline

	-- Run the benchmarks in the build directory
	benchResults
	 <- inDir (configScratchDir config ++ "/repa-head")
 	 $ do	mapM 	(outRunBenchmarkWith (configIterations config)  resultsPrior)
			(benchmarks config)

	-- Make the build results.
	let buildResults
		= BuildResults
		{ buildResultTime		= utcTime
		, buildResultEnvironment	= env
		, buildResultBench		= benchResults }

	-- Write results to a file if requested.	
	maybe 	(return ())
		(\(fileName, shouldStamp) -> do
			stamp	<- if shouldStamp
				 	then io $ getStampyTime
					else return ""
					
			
			let fileName'	= fileName ++ stamp
			
			outLn $ "  - Writing results to " ++ fileName'
			io $ writeFile fileName' $ show buildResults)
		(configWriteResults config)
	
	-- Mail results to recipient if requested.
	let spaceHack = text . unlines . map (\l -> " " ++ l) . lines . render
	maybe 	(return ())
		(\(from, to) -> do
			outLn $ "  - Mailing results to " ++ to 
			mail	<- createMailWithCurrentTime from to "[nightly] Repa Performance Test Succeeded"
				$ render $ vcat
				[ text "Repa Performance Test Succeeded"
				, blank
				, ppr env
				, blank
				, spaceHack $ pprComparisons resultsPrior benchResults
				, blank ]
				
			sendMailWithMailer mail defaultMailer				
			return ())
		(configMailFromTo config)
