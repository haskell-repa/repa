
module Config 
	( Config(..)
	, defaultMailer
	, basePackages)
where
import BuildBox

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
	, configDoRepaTest	:: Bool 

	-- Testing config.
	, configIterations	:: Int
	, configAgainstResults	:: Maybe FilePath 

	-- What do with the results.
	, configWriteResults	:: Maybe FilePath
	, configMailFromTo	:: Maybe (String, String) }
	deriving Show


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
	