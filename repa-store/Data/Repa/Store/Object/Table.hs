
-- | A Table is a sequence of rows of fields, where each row contains the same
--   number of fields. Conversely, all the columns are the same length.
--
--   Table data is stored in a directory with the ".t" extension. 
--   The directory contains a number of buckets holding the data, and some
--   meta-data  encoded as JSON.
-- 
-- @
--   sometable.t/
--   sometable.t/000000.tsv
--   sometable.t/000001.tsv
--   sometable.t/000001.tsv
--   ...
--   sometable.t/_table.json
-- @
--
module Data.Repa.Store.Object.Table
        ( -- * Table
          Table  (..)
        , lookupColumn)
where
import Data.Repa.Store.Object.Column
import Control.Monad
import Data.Text                                (Text)
import Data.Aeson                               as A
import qualified Data.List                      as L
import qualified Data.Repa.Store.Format         as F
import qualified Data.HashMap.Strict            as H


-- | Describes a single table.
data Table
        = Table
        { -- | Table name
          tableName      :: Text

          -- | How rows and fields are deliminted in the data.
        , tableDelim     :: F.Delim

          -- | Table columns.
        , tableColumns   :: [Column] 

          -- | Local directory that holds the column family, if known.
          --   (not serialised)
        , tableDirectory :: Maybe FilePath }
        deriving Show


instance ToJSON Table where
 toJSON (Table name delim columns _mDirectory)
        = object [ "_type"      .= text "table"
                 , "name"       .= toJSON name
                 , "delim"      .= toJSON delim
                 , "columns"    .= toJSON columns]


instance FromJSON Table where
 parseJSON (Object hh)
        | Just (String "table") <- H.lookup "_type"   hh
        , Just jName            <- H.lookup "name"    hh
        , Just jDelim           <- H.lookup "delim"   hh
        , Just jColumns         <- H.lookup "columns" hh
        = do    name    <- parseJSON jName
                delim   <- parseJSON jDelim
                columns <- parseJSON jColumns
                return  $ Table name delim columns Nothing

 parseJSON _ = mzero


-- | Lookup a single column in the table,
--   returning `Nothing` if it's not there.
lookupColumn :: Text -> Table -> Maybe Column
lookupColumn name table
 = L.find (\c -> name == columnName c) (tableColumns table)


text :: Text -> Text
text x = x

