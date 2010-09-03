
-- | Build stages concerning DPH
module BuildDPH
	(dphBuild)
where
import Config
import BuildBox

-- | Build the examples package
dphBuild :: Config -> Build ()
dphBuild config
 = inDir (configScratchDir config)
 $ inDir "ghc-head/libraries/dph/dph-examples"
 $ do	outLn "* Building DPH Examples"

	ssystem	$ "runghc Setup.hs clean"
	ssystem	$ "runghc Setup.hs configure"
		++ " --user "
		++ " --with-compiler=" ++ configWithGhc config
		++ " --with-hc-pkg="   ++ configWithGhcPkg config
		
	ssystem	"runghc Setup.hs build"
