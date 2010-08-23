
-- | Build stages concerning GHC.
module BuildGhc
	( ghcUnpack
	, ghcBuild
	, ghcLibs)
where
import Config
import BuildBox

ghcUnpack :: Config -> Build ()
ghcUnpack config
 = inDir (configScratchDir config)
 $ do	outLn "* Unpacking GHC"
	clobberDir "ghc-head"
	let Just snapshot = configWithGhcSnapshot config

	outLn $ "  - Unpacking snapshot " ++ snapshot
	system $ "tar zxf " ++ snapshot
	
	outLn $ "  - Updating snapshot"
	inDir "ghc-head"
	 $ system "./darcs-all pull -av"
	

ghcBuild :: Config -> Build ()
ghcBuild config
 = inDir (configScratchDir config)
 $ inDir "ghc-head"
 $ do	outLn "* Building GHC"
	
	system $ "perl boot"
	system $ "./configure"
	system $ "make"
	
	inDir "inplace/bin"
	 $ system $ "ln -s ghc-stage2 ghc"
	
	outBlank
	outBlank

ghcLibs :: Config -> Build ()
ghcLibs config
 = do	outLn "* Building base libraries."
	outCheckOk "  - Checking for cabal"
	 $ HasExecutable "cabal"

	let cabal	= "cabal "
			++ " --with-compiler=" ++ configWithGhc config
			++ " --with-hc-pkg="   ++ configWithGhcPkg config

	outLn " - Updating cabal package database"
	systemNull $ "cabal update"
		
	let cabalInstall pkg
		= do	outLn   $ "  - Building " ++ pkg
			system	$ cabal ++ " install " ++ pkg
			outBlank
		
	mapM_ cabalInstall basePackages
