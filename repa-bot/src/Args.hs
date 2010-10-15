{-# LANGUAGE ScopedTypeVariables #-}

module Args
	( BuildArg(..)
	, buildArgs)
where
import System.Console.ParseArgs

-- Command line args for the buildbot.
data BuildArg
	= ArgHelp
	| ArgVerbose

	-- Working with test files.
	| ArgDoDump
	| ArgDoCompare

	-- Automated builds
	| ArgDaily
	| ArgDailyNow
	| ArgDailyTomorrow

	-- Building GHC and libs.
	| ArgScratchDir
	| ArgGhcUnpack
	| ArgGhcBuild
	| ArgGhcUnpackBuild
	| ArgGhcUse
	| ArgLibs

	-- Testing DPH and Repa
	| ArgDoTestRepa
	| ArgDoTestDPH
	| ArgTestIterations
	| ArgMailFrom 
	| ArgMailTo
	| ArgWriteResults
	| ArgWriteResultsStamped
	| ArgAgainstResults
	deriving (Eq, Ord, Show)


buildArgs :: [Arg BuildArg]
buildArgs
 = 	[ Arg	{ argIndex	= ArgHelp
		, argAbbr	= Just 'h'
		, argName	= Just "help"
		, argData	= Nothing
		, argDesc	= "Print this usage help." }

	, Arg	{ argIndex	= ArgVerbose
		, argAbbr	= Just 'v'
		, argName	= Just "verbose"
		, argData	= Nothing
		, argDesc	= "Verbose logging of build commands." }


	-- Working with test files.
	, Arg	{ argIndex	= ArgDoDump
		, argAbbr	= Nothing
		, argName	= Just "dump"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Dump a test results file in human readable format." }

	, Arg	{ argIndex	= ArgDoCompare 
		, argAbbr	= Nothing
		, argName	= Just "compare"
		, argData	= Nothing
		, argDesc	= "Compare two test results files." }


	-- Automated builds
	, Arg	{ argIndex	= ArgDaily
		, argAbbr	= Nothing
		, argName	= Just "daily"
		, argData	= argDataOptional "time" ArgtypeString
		, argDesc	= "Run the build commands every day at this time. fmt: HH:MM:SS" }

	, Arg	{ argIndex	= ArgDailyNow
		, argAbbr	= Nothing
		, argName	= Just "now"
		, argData	= Nothing
		, argDesc	= "(opt. for --daily) Also run the build right now." }

	, Arg	{ argIndex	= ArgDailyTomorrow
		, argAbbr	= Nothing
		, argName	= Just "tomorrow"
		, argData	= Nothing
		, argDesc	= "(opt. for --daily) Run the first build tomorrow." }


	-- Building GHC and libs.
	, Arg	{ argIndex	= ArgScratchDir
		, argAbbr	= Nothing
		, argName	= Just "scratch"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "For --ghc-unpack and --ghc-unpack-build, where to put the unpacked tree." }

	, Arg	{ argIndex	= ArgGhcUnpack
		, argAbbr	= Nothing
		, argName	= Just "ghc-unpack"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Unpack this GHC snapshot and update it from darcs.haskell.org." }

	, Arg	{ argIndex	= ArgGhcBuild
		, argAbbr	= Nothing
		, argName	= Just "ghc-build"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "Build an already unpacked and updated GHC snapshot." }

	, Arg	{ argIndex	= ArgGhcUnpackBuild
		, argAbbr	= Nothing
		, argName	= Just "ghc-unpack-build"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Unpack this GHC snapshot, update, and build it." }

	, Arg	{ argIndex	= ArgGhcUse
		, argAbbr	= Nothing
		, argName	= Just "ghc-use"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "Use the previously built GHC in this dir" }

	, Arg	{ argIndex	= ArgLibs
		, argAbbr	= Nothing
		, argName	= Just "ghc-libs"
		, argData	= argDataOptional "spec" ArgtypeString
		, argDesc	= "Install some libraries into the GHC build" }


	-- Testing DPH and Repa
	, Arg	{ argIndex	= ArgDoTestDPH
		, argAbbr	= Nothing
		, argName	= Just "test-dph"
		, argData	= Nothing
		, argDesc	= "Run DPH regression tests." }
		
	, Arg	{ argIndex	= ArgDoTestRepa
		, argAbbr	= Nothing
		, argName	= Just "test-repa"
		, argData	= Nothing
		, argDesc	= "Run Repa regression tests." }

	, Arg	{ argIndex	= ArgTestIterations
		, argAbbr	= Just 'i'
		, argName	= Just "iterations"
		, argData	= argDataDefaulted "int" ArgtypeInt 1
		, argDesc	= "(opt. for test modes) Number of times to run each benchmark." }

	, Arg	{ argIndex	= ArgAgainstResults
		, argAbbr	= Just 'a'
		, argName	= Just "against"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes) Print running comparison against results in this file." }

	, Arg	{ argIndex	= ArgWriteResults
		, argAbbr	= Just 'w'
		, argName	= Just "write"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes) Write results to this file." }

	, Arg	{ argIndex	= ArgWriteResultsStamped
		, argAbbr	= Just 's'
		, argName	= Just "write-stamped"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... appending a time stamp to the name." }		

	, Arg	{ argIndex	= ArgMailFrom
		, argAbbr	= Nothing
		, argName	= Just "mailfrom"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for test modes) Use \"msmtp\" to mail results from this address." }

	, Arg	{ argIndex	= ArgMailTo
		, argAbbr	= Nothing
		, argName	= Just "mailto"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... to this address." }			
	]
