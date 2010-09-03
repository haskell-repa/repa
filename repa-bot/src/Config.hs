{-# LANGUAGE PatternGuards #-}

module Config 
	( Config(..)
	, defaultMailer
	, basePackages
	, slurpConfig)
where
import BuildBox
import Args
import System.Console.ParseArgs	hiding (args)
import System.Directory
import Control.Monad


-- Config -----------------------------------------------------------------------------------------
-- | Buildbot command line configuration.
data Config
	= Config
	{ configVerbose		:: Bool
	, configScratchDir	:: String

	-- GHC config
	, configWithGhcBuild	:: Maybe FilePath
	, configWithGhc		:: FilePath
	, configWithGhcPkg	:: FilePath

	, configWithGhcSnapshot	:: Maybe FilePath

	-- Build stages
	, configDoGhcUnpack	:: Bool
	, configDoGhcBuild	:: Bool
	, configDoGhcLibs	:: Bool
	, configDoRepaUnpack	:: Bool
	, configDoRepaBuild	:: Bool
	, configDoDPHBuild	:: Bool

	-- Test stages
	, configDoTestRepa	:: Bool 
	, configDoTestDPH	:: Bool

	-- Testing config.
	, configIterations	:: Int
	, configAgainstResults	:: Maybe FilePath 

	-- What do with the results.
	, configWriteResults	:: Maybe (FilePath, Bool)
	, configMailFromTo	:: Maybe (String, String) }
	deriving Show



-- Hard Coded -------------------------------------------------------------------------------------
-- Hard coded config.
--	Can't be bothered turning these into cmd line args.
defaultMailer :: Mailer
defaultMailer
	= MailerMSMTP
	{ mailerPath	= "msmtp"
	, mailerPort	= Just 587 }
	
-- | These are non-repa packages we always want to download and install
--   into a fresh GHC build.
basePackages :: [String]
basePackages
 = 	[ "bmp-1.1.1.1"
	, "QuickCheck-2.1.1.1" ]


-- Slurp ------------------------------------------------------------------------------------------
-- | Slurp configuration information from the command line arguments.
slurpConfig :: Args BuildArg -> FilePath -> IO Config
slurpConfig args scratchDir
 = do 	let Just iterations	= getArg args ArgTestIterations
	
	-- canonicalize all the paths we were given.
	withScratchDir	<- canonicalizePath scratchDir
	
	withGhc		<- maybe 
				(return "ghc")
				(\dir -> canonicalizePath $ dir ++ "/inplace/bin/ghc-stage2")
				(getArg args ArgWithGhcBuild)
				
	withGhcPkg	<- maybe
				(return "ghc-pkg")
				(\dir -> canonicalizePath $ dir ++ "/inplace/bin/ghc-pkg")
				(getArg args ArgWithGhcBuild)

	withGhcSnapshot	<- if gotArg args ArgDoGhcUnpack
			     then maybe (return Nothing)
					(liftM Just . canonicalizePath)
					(getArg args ArgDoGhcUnpack)
					
			     else maybe	(return Nothing)
					(liftM Just . canonicalizePath)
					(getArg args ArgWithGhcSnapshot)
	
    	return $ Config
		{ configVerbose		= gotArg args ArgVerbose
		, configScratchDir	= withScratchDir
		, configWithGhcBuild	= getArg args ArgWithGhcBuild
		, configWithGhc 	= withGhc
		, configWithGhcPkg	= withGhcPkg
		, configWithGhcSnapshot	= withGhcSnapshot

		-- What stages to run.
		-- If --total is set then do them all.
		, configDoGhcUnpack	= gotArg args ArgDoGhcUnpack  || gotArg args ArgDoTotal
		, configDoGhcBuild	= gotArg args ArgDoGhcBuild   || gotArg args ArgDoTotal
		, configDoGhcLibs	= gotArg args ArgDoGhcLibs    || gotArg args ArgDoTotal
		, configDoRepaUnpack	= gotArg args ArgDoRepaUnpack || gotArg args ArgDoTotal
		, configDoRepaBuild	= gotArg args ArgDoRepaBuild  || gotArg args ArgDoTotal
		, configDoDPHBuild	= gotArg args ArgDoDPHBuild   || gotArg args ArgDoTotal

		-- Testing stages
		, configDoTestRepa	= gotArg args ArgDoTestRepa   || gotArg args ArgDoTotal
		, configDoTestDPH	= gotArg args ArgDoTestDPH    || gotArg args ArgDoTotal

		-- Testing config.
		, configIterations	= iterations 
		, configWriteResults	= let result
						| Just name	<- getArg args ArgWriteResultsStamped
						= Just (name, True)

						| Just name	<- getArg args ArgWriteResults
						= Just (name, False)
						
						| otherwise
						= Nothing
					  in  result
						
		, configAgainstResults	= getArg args ArgAgainstResults

		-- TODO: check we have both args
		, configMailFromTo	= let result	
						| Just from	<- getArg args ArgMailFrom
						, Just to	<- getArg args ArgMailTo
						= Just (from, to)
							
						| otherwise
						= Nothing
				  		in	result
		}
	