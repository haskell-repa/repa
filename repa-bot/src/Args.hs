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
	| ArgScratchDir

	-- Run the process every day
	| ArgDaily
	| ArgDailyNow
	| ArgDailyTomorrow

	-- Recipies
	| ArgDoTotal

	-- GHC building
	| ArgWithGhcSnapshot
	| ArgDoGhcUnpack
	| ArgDoGhcBuild
	| ArgDoGhcLibs

	-- Repa building
	| ArgWithGhcBuild
	| ArgDoRepaUnpack
	| ArgDoRepaBuild

	| ArgDoRepaTest
	| ArgTestIterations

	-- Working with results files
	| ArgDoDump
	| ArgDoCompare
	| ArgWriteResults
	| ArgWriteResultsStamped
	| ArgAgainstResults
	| ArgMailFrom 
	| ArgMailTo
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

	, Arg	{ argIndex	= ArgDoTotal
		, argAbbr	= Nothing
		, argName	= Just "total"
		, argData	= Nothing
		, argDesc	= "Run the total build. All ghc and repa stages." }

	, Arg	{ argIndex	= ArgDoGhcUnpack
		, argAbbr	= Nothing
		, argName	= Just "ghc-unpack"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Unpack this GHC snapshot and update it from darcs.haskell.org." }

	, Arg	{ argIndex	= ArgDoGhcBuild
		, argAbbr	= Nothing
		, argName	= Just "ghc-build"
		, argData	= Nothing
		, argDesc	= "Build an unpacked GHC snapshot." }

	, Arg	{ argIndex	= ArgDoGhcLibs
		, argAbbr	= Nothing
		, argName	= Just "ghc-libs"
		, argData	= Nothing
		, argDesc	= "Download and install base libraries into a GHC build." }

	, Arg	{ argIndex	= ArgDoRepaUnpack
		, argAbbr	= Nothing
		, argName	= Just "repa-unpack"
		, argData	= Nothing
		, argDesc	= "Download the latest version of Repa from code.haskell.org." }

	, Arg	{ argIndex	= ArgDoRepaBuild
		, argAbbr	= Nothing
		, argName	= Just "repa-build"
		, argData	= Nothing
		, argDesc	= "Build and register the Repa packages with a GHC build." }

	, Arg	{ argIndex	= ArgDoRepaTest
		, argAbbr	= Nothing
		, argName	= Just "repa-test"
		, argData	= Nothing
		, argDesc	= "Run Repa regression tests." }

	, Arg	{ argIndex	= ArgDaily
		, argAbbr	= Nothing
		, argName	= Just "daily"
		, argData	= argDataOptional "time" ArgtypeString
		, argDesc	= "Run the given build commands every day at this time. fmt: HH:MM:SS" }

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

	, Arg	{ argIndex	= ArgScratchDir
		, argAbbr	= Nothing
		, argName	= Just "scratch"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "Scratch dir to do the build in." }

	, Arg	{ argIndex	= ArgWithGhcSnapshot
		, argAbbr	= Nothing
		, argName	= Just "with-ghc-snapshot"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Use this GHC snapshot, something like ghc-head-DATE.tgz" }

	, Arg	{ argIndex	= ArgWithGhcBuild
		, argAbbr	= Nothing
		, argName	= Just "with-ghc-build"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "Use this existing GHC build." }

	, Arg	{ argIndex	= ArgMailFrom
		, argAbbr	= Nothing
		, argName	= Just "mailfrom"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for repa-test mode) Use \"msmtp\" to mail results from this address." }

	, Arg	{ argIndex	= ArgMailTo
		, argAbbr	= Nothing
		, argName	= Just "mailto"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for repa-test mode)  ... to this address." }	

	, Arg	{ argIndex	= ArgTestIterations
		, argAbbr	= Just 'i'
		, argName	= Just "iterations"
		, argData	= argDataDefaulted "int" ArgtypeInt 1
		, argDesc	= "(opt. for repa-test mode) Number of times to run each benchmark." }
		
	, Arg	{ argIndex	= ArgWriteResults
		, argAbbr	= Just 'w'
		, argName	= Just "write"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for repa-test mode) Write results to this file." }

	, Arg	{ argIndex	= ArgWriteResultsStamped
		, argAbbr	= Just 's'
		, argName	= Just "write-stamped"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for repa-test mode)  ... appending a time stamp to the name." }
		
	, Arg	{ argIndex	= ArgAgainstResults
		, argAbbr	= Just 'a'
		, argName	= Just "against"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for repa-test mode) Print running comparison against results in this file." }
		
	]
	
	
	