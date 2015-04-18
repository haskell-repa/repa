
module Data.Repa.Query.Build
        ( buildQueryViaRepa )
where
import System.FilePath
import qualified Language.Haskell.TH            as TH
import qualified Data.Repa.Query.Compile.Repa   as CR
import qualified Data.Repa.Query.Graph          as Q
import BuildBox.Build


-- | Produce an executable for the given query.
--
--   We use Template Haskell to convert the query to Haskell using the
--   Repa Flow library, then compile it with GHC.
--
buildQueryViaRepa
        :: FilePath                        -- ^ Working directory.
        -> FilePath                        -- ^ Path to result executable.
        -> Q.Query () String String String -- ^ Query to compile.
        -> IO (Either BuildError ())

buildQueryViaRepa dirScratch _path query
 = runBuild dirScratch
 $ do   
        dec <- io $ TH.runQ $ CR.decOfQuery (TH.mkName "_makeSources") query

        io $ writeFile (dirScratch </> "Main.hs") 
           $ repaHeader ++ "\n" ++ TH.pprint dec ++ "\n\n"

        return ()



repaHeader :: String
repaHeader
 = unlines 
 [ "import qualified Data.Repa.Query.Runtime.Driver"
 , "import qualified Data.Repa.Flow.Auto"
 , "import qualified Data.Repa.Flow.Auto.IO"
 , "import qualified Data.Repa.Flow.Auto.Format"
 , "import qualified Data.Repa.Flow.IO.Bucket"
 , "import qualified Data.Repa.Convert.Format.Fields"
 , "import qualified Data.Repa.Convert.Format.Numeric"
 , "import qualified Data.Repa.Product"
 , "import qualified GHC.Classes"
 , "import qualified GHC.Num"
 , "import qualified GHC.Base"
 , "import qualified GHC.Err" 
 , ""
 , "main "
 , " = do { sources <- _makeSources"
 , "      ; True    <- Data.Repa.Query.Runtime.Driver.streamSourcesToStdout sources"
 , "      ; return () }" ] 



