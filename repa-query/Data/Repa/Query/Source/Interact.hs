
-- | Driver commands for running queries interactively in GHCi.
module Data.Repa.Query.Source.Interact
        ( module Data.Repa.Query.Source
        , module Data.Repa.Query.Job)
--        , graphWith
--        , jsonWith
--        , repaWith)
where
import Data.Repa.Query.Source
import Data.Repa.Query.Job
-- import qualified Data.Repa.Query.Source.Builder         as Q

-- import qualified Data.Repa.Query.Source                 as Q
-- import qualified Data.Repa.Query.Build                  as Q
-- import qualified Data.Repa.Query.Build.Repa             as Q

-- import qualified Data.Aeson                             as Aeson
-- import qualified Data.Aeson.Encode.Pretty               as Aeson

-- import qualified Data.ByteString.Lazy.Char8             as BS

-- import qualified System.Process                         as S

-- import qualified Language.Haskell.TH                    as TH

-- import qualified BuildBox                               as BB
-- import qualified BuildBox.Command.File                  as BB
-- import Prelude                                          as P


{-
---------------------------------------------------------------------------------------------------
-- | Print the query AST to stdout.
graphWith 
        :: FilePath     -- ^ Root path to data.
        -> Q Query      -- ^ Query.
        -> IO ()

graphWith pathRootData qquery
 = do   let config  = Q.Config pathRootData
        equery <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> do putStrLn $ show q
                return ()


---------------------------------------------------------------------------------------------------
-- | Print the JSON version of a query to stdout.
jsonWith :: FilePath     -- ^ Root path to data.
         -> Q Query      -- ^ Query.
         -> IO ()

jsonWith pathRootData qquery
 = do   let config  = Q.Config pathRootData
        equery <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> BS.putStrLn
                $ Aeson.encodePretty' aesonConfig
                $ Aeson.toJSON q

aesonConfig
 = Aeson.defConfig
        { Aeson.confIndent      = 4
        , Aeson.confCompare     = compare }


---------------------------------------------------------------------------------------------------
-- | Print the Repa version of a query to stdout.
repaWith :: FilePath    -- ^ Root path to data.
         -> Q Query     -- ^ Query.
         -> IO ()

repaWith pathRootData qquery
 = do   let config = Q.Config pathRootData
        equery <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> do dec <- TH.runQ 
                    $ Q.decOfQuery (TH.mkName "_makeSources") q
                putStrLn $ TH.pprint dec

-}

