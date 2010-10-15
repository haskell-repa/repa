
-- | Build stages concerning Repa.
module BuildRepa
	( repaUnpack
	, repaBuild)
where
import Config
import BuildBox
import Control.Monad
import Data.Maybe


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
	
	let scratchDir	= fromMaybe ("repaUnpack: must specify --scratch") $ configScratchDir config
	out "\n"
	inDir scratchDir
	 $ do	clobberDir "repa-head"
		
		outLn  "* Getting Darcs Package"
		ssystem "darcs get http://code.haskell.org/repa/repa-head"
	


-- Building ---------------------------------------------------------------------------------------	
-- | Build the packages and register them with the given compiler.
repaBuild :: Config -> Build ()
repaBuild config
 = inDir (fromMaybe ("repaBuild: must specify --scratch") $ configScratchDir config)
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
	
	
