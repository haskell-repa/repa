
-- | A column is a single sequence of elements.
module Data.Repa.Store.Object.Column
        (Column (..))
where
import Control.Monad
import Data.Text                                (Text)
import Data.Aeson                               as A
import qualified Data.Repa.Store.Format         as F
import qualified Data.HashMap.Strict            as H


-- | Describes a single column in a table.
data Column 
        = Column
        { -- | Name of column.
          columnName            :: Text

          -- | On-disk data format of column.
        , columnFormat          :: F.FieldBox

          -- | Human readable description of column.
        , columnDescription     :: Text 

          -- | Local directory that holds the column, if known.
          --   (not serialized)
        , columnDirectory       :: Maybe FilePath }
        deriving Show


instance ToJSON Column where
 toJSON (Column name field desc _mDirectory)
        = object [ "_type"  .= text "column"
                 , "name"   .= toJSON name
                 , "format" .= toJSON field
                 , "desc"   .= toJSON desc ]


instance FromJSON Column where
 parseJSON (Object hh)
        | Just (String "column") <- H.lookup "_type"  hh
        , Just (String name)     <- H.lookup "name"   hh
        , Just jField            <- H.lookup "format" hh
        , Just (String desc)     <- H.lookup "desc"   hh
        = do
                field   <- parseJSON jField
                return $ Column name field desc Nothing

 parseJSON _ = mzero


text :: Text -> Text
text x = x
