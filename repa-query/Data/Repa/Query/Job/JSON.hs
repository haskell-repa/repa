{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Query.Job.JSON
        (encode, toJSON, fromJSON, Result(..))
where
import Control.Monad
import Data.Repa.Query.Graph.JSON
import Data.Repa.Query.Job.Spec
import Data.Aeson                               as Aeson
import Data.Text                                (Text)
import qualified Data.HashMap.Strict            as H
import qualified Data.Text                      as Text

----------------------------------------------------------------------------------------------- Job
instance ToJSON Job where
 toJSON xx 
  = case xx of
        JobQuery   query format
         -> object [ "_type"    .= text "job"
                   , "job"      .= text "query"
                   , "query"    .= toJSON query
                   , "format"   .= toJSON format ]

        JobExtract query format target
         -> object [ "_type"    .= text "job"
                   , "job"      .= text "extract"
                   , "query"    .= toJSON query
                   , "format"   .= toJSON format
                   , "target"   .= toJSON target ]

        JobSieve   query format target
         -> object [ "_type"    .= text "job"
                   , "job"      .= text "sieve"
                   , "query"    .= toJSON query
                   , "format"   .= toJSON format
                   , "target"   .= toJSON target ]


instance FromJSON Job where
 parseJSON (Object hh)

        | Just (String "job")     <- H.lookup "_type"  hh
        , Just (String "query")   <- H.lookup "job"    hh
        , Just jQuery             <- H.lookup "query"  hh
        , Just jFormat            <- H.lookup "format" hh
        = do    query   <- parseJSON jQuery
                format  <- parseJSON jFormat
                return  $ JobQuery query format

        | Just (String "job")     <- H.lookup "_type"  hh
        , Just (String "extract") <- H.lookup "job"    hh
        , Just jQuery             <- H.lookup "query"  hh
        , Just jFormat            <- H.lookup "format" hh
        , Just jTarget            <- H.lookup "target" hh
        = do    query   <- parseJSON jQuery
                format  <- parseJSON jFormat
                target  <- parseJSON jTarget
                return  $ JobExtract query format target

        | Just (String "job")     <- H.lookup "_type"  hh
        , Just (String "sieve")   <- H.lookup "job"    hh
        , Just jQuery             <- H.lookup "query"  hh
        , Just jFormat            <- H.lookup "format" hh
        , Just jTarget            <- H.lookup "target" hh
        = do    query   <- parseJSON jQuery
                format  <- parseJSON jFormat
                target  <- parseJSON jTarget
                return  $ JobExtract query format target

 parseJSON _
        = mzero


--------------------------------------------------------------------------------------------- Query
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (Query a nF bV nV)) where
 toJSON xx
  = case xx of
        Query flow graph
         -> object [ "_type"    .= text "query"
                   , "out"      .= toJSON flow
                   , "graph"    .= toJSON graph ]


instance (FromJSON nF, FromJSON bV, FromJSON nV)
      => (FromJSON (Query () nF bV nV)) where
 parseJSON (Object hh)

        | Just (String "query") <- H.lookup "_type"  hh
        , Just jOut             <- H.lookup "out"    hh
        , Just jGraph           <- H.lookup "graph"  hh
        = do    fOut    <- parseJSON jOut
                graph   <- parseJSON jGraph
                return  $ Query fOut graph

 parseJSON _ = mzero


-------------------------------------------------------------------------------------- OutputFormat
instance ToJSON OutputFormat where
 toJSON xx
  = case xx of
        OutputFormatFixed delim fields
         -> object [ "_type"    .= text "output_format"
                   , "format"   .= text "fixed"
                   , "delim"    .= toJSON delim
                   , "fields"   .= toJSON fields ]

        OutputFormatAsciiBuildTime 
         -> object [ "_type"    .= text "output_format"
                   , "format"   .= text "ascii_build_time" ]


instance FromJSON OutputFormat where
 parseJSON (Object hh)
        | Just (String "output_format") <- H.lookup "_type" hh
        , Just (String "fixed") <- H.lookup "format" hh
        , Just jDelim           <- H.lookup "delim"  hh
        , Just jFields          <- H.lookup "fields" hh
        = do    delim   <- parseJSON jDelim
                fields  <- parseJSON jFields
                return  $  OutputFormatFixed delim fields

        | Just (String "output_format")    <- H.lookup "_type" hh
        , Just (String "ascii_build_time") <- H.lookup "format" hh
        = do    return  $  OutputFormatAsciiBuildTime

 parseJSON _ = mzero


------------------------------------------------------------------------------------- ExtractTarget
instance ToJSON ExtractTarget where
 toJSON xx 
  = case xx of
        ExtractTargetFile path
         -> object [ "_type"    .= text "extract_target"
                   , "target"   .= text "file"
                   , "file"     .= Text.pack path ]


instance FromJSON ExtractTarget where
 parseJSON (Object hh)
        | Just (String "extract_target") <- H.lookup "_type"  hh
        , Just (String "file")           <- H.lookup "target" hh
        , Just (String path)             <- H.lookup "file"   hh
        = do    return  $ ExtractTargetFile (Text.unpack path)

 parseJSON _ = mzero


------------------------------------------------------------------------------------- ExtractTarget
instance ToJSON SieveTarget where
 toJSON xx 
  = case xx of
        SieveTargetDir path
         -> object [ "_type"    .= text "sieve_target"
                   , "target"   .= text "dir"
                   , "dir"      .= Text.pack path ]


instance FromJSON SieveTarget where
 parseJSON (Object hh)
        | Just (String "sieve_target")   <- H.lookup "_type"  hh
        , Just (String "dir")            <- H.lookup "target" hh
        , Just (String path)             <- H.lookup "dir "   hh
        = do    return  $ SieveTargetDir (Text.unpack path)

 parseJSON _ = mzero


---------------------------------------------------------------------------------------------------
text :: Text -> Text
text x = x 



