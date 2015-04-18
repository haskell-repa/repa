
module Data.Repa.Query.Build
        ( buildQueryViaRepa 
        , buildJsonViaRepa
        , buildDslViaRepa)
where
import System.FilePath
import System.Directory
import Control.Monad
import Data.Repa.Query.Convert.JSON             ()
import qualified BuildBox.Build                 as BB
import qualified BuildBox.Command.System        as BB
import qualified BuildBox.Command.File          as BB
import qualified Language.Haskell.TH            as TH
import qualified Data.Repa.Query.Compile.Repa   as CR
import qualified Data.Repa.Query.Graph          as Q
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Lazy.Char8     as BS


---------------------------------------------------------------------------------------------------
-- | Produce an executable for the given query.
--
--   We use Template Haskell to convert the query to Haskell using the
--   Repa Flow library, then compile it with GHC.
--
--   The working directory is used to store the intermediate Haskell
--   source file as well as the built executable. Written files are not
--   removed after the build, so the whole directory and its contents
--   should be removed by the caller when they're done.
--
buildQueryViaRepa
        :: FilePath                        -- ^ Working directory.
        -> Bool                            -- ^ Cleanup intermediate files.
        -> Q.Query () String String String -- ^ Query to compile.
        -> FilePath                        -- ^ Path of output executable.
        -> BB.Build ()

buildQueryViaRepa dirScratch cleanup query fileExe
 = do   
        -- Ensure the working directory already exists.
        BB.ensureDir dirScratch

        -- Use Template Haskell to convert the query into Haskell code.
        dec <- BB.io $ TH.runQ $ CR.decOfQuery (TH.mkName "_makeSources") query

        let fileHS      = dirScratch </> "Main.dump-repa.hs" -- written by us.
        let fileHI      = dirScratch </> "Main.dump-repa.hi" -- dropped by GHC
        let fileHO      = dirScratch </> "Main.dump-repa.o"  -- dropped by GHC

        -- Write out Haskell into a temp file.
        BB.io $ writeFile fileHS
              $ repaHeader ++ "\n" ++ TH.pprint dec ++ "\n\n"

        -- Call GHC to build the query.
        _  <- BB.sesystemq $ "ghc -fforce-recomp -O2 --make " ++ fileHS ++ " -o " ++ fileExe

        -- Remove dropped files.
        when cleanup
         $ BB.io $ mapM_ removeFile [fileHS, fileHI, fileHO]

        return ()


---------------------------------------------------------------------------------------------------
-- | Like `buildQueryViaRepa`, but accept a query encoded as JSON.
buildJsonViaRepa
        :: FilePath             -- ^ Working directory.
        -> Bool                 -- ^ Cleanup intermediate files.
        -> String               -- ^ Query encoded as JSON.
        -> FilePath             -- ^ Path to output executable.
        -> BB.Build (Q.Query () String String String)
                                -- ^ Operator graph of result query.
buildJsonViaRepa dirScratch cleanup json fileExe
 = do   
        let Just query :: Maybe (Q.Query () String String String)
                = Aeson.decode $ BS.pack json

        buildQueryViaRepa dirScratch cleanup query fileExe
        return  query


---------------------------------------------------------------------------------------------------
-- | Like `buildQueryViaRepa`, but accept a query encoded in the DSL.
buildDslViaRepa
        :: FilePath             -- ^ Working directory.
        -> Bool                 -- ^ Cleanup intermediate files.
        -> String               -- ^ Query encoded in the DSL.
        -> FilePath             -- ^ Path to output executable.
        -> BB.Build (Q.Query () String String String)
                                -- ^ Operator graph of compiled query.

buildDslViaRepa dirScratch cleanup dslQuery fileExe
 = do   
        -- Attach header that defines the prims, and write out the code.
        --
        -- TODO: sanitize query before pasting on header to make sure it
        --       doesn't try to import other modules.
        --
        let fileHS      = dirScratch </> "Main.dump-dsl.hs"

        BB.io $ writeFile fileHS (edslHeader ++ dslQuery)

        jsonQuery       <- BB.sesystemq 
                        $ "ghc " ++ fileHS
                                 ++ " -e "
                                 ++ "\"B.putStrLn (A.encode (A.toJSON result))\""

        -- Remove dropped files.
        when cleanup
         $ BB.io $ removeFile fileHS

        buildJsonViaRepa dirScratch cleanup jsonQuery fileExe


---------------------------------------------------------------------------------------------------
-- | Junk pasted to the front of a query written in the EDSL 
--   to make it a well formed Haskell program.
edslHeader :: String
edslHeader
 = unlines
 [ "{-# LANGUAGE NoImplicitPrelude   #-}"
 , "{-# LANGUAGE ScopedTypeVariables #-}"
 , "{-# LANGUAGE GADTs               #-}"
 , "import Data.Repa.Query.Source.Primitive"
 , "import qualified Data.Repa.Query.Convert.JSON"
 , "import qualified Data.ByteString.Lazy.Char8    as B (putStrLn)"
 , "import qualified Data.Aeson                    as A (encode, toJSON)"
 , ""]


-- | Junk pasted to the front of generated Repa code 
--   to make it a well formed Haskell program.
repaHeader :: String
repaHeader
 = unlines 
 [ "import qualified Data.Repa.Query.Runtime.Driver"
 , "import qualified Data.Repa.Query.Runtime.Primitive"
 , "import qualified Data.Repa.Product"
 , ""
 , "main "
 , " = do { sources <- _makeSources"
 , "      ; True    <- Data.Repa.Query.Runtime.Driver.streamSourcesToStdout sources"
 , "      ; return () }" ] 

