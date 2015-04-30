{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Query.Job.JSON
        (encode, toJSON, fromJSON, Result(..))
where
import Control.Monad
import Data.Repa.Query.Graph.JSON
import Data.Repa.Query.Job
import Data.Aeson                               as Aeson
import Data.Text                                (Text)
import qualified Data.HashMap.Strict            as H

--------------------------------------------------------------------------------------------- Query
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (Query a nF bV nV)) where
 toJSON xx
  = case xx of
        Query format flow graph
         -> object [ "_type"    .= text "query"
                   , "out"      .= toJSON flow
                   , "format"   .= toJSON format
                   , "graph"    .= toJSON graph ]


instance (FromJSON nF, FromJSON bV, FromJSON nV)
      => (FromJSON (Query () nF bV nV)) where
 parseJSON (Object hh)

        | Just (String "query") <- H.lookup "_type"  hh
        , Just jOut             <- H.lookup "out"    hh
        , Just jFormat          <- H.lookup "format" hh
        , Just jGraph           <- H.lookup "graph"  hh
        = do    fOut    <- parseJSON jOut
                format  <- parseJSON jFormat
                graph   <- parseJSON jGraph
                return  $ Query format fOut graph

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

---------------------------------------------------------------------------------------------------
text :: Text -> Text
text x = x 
