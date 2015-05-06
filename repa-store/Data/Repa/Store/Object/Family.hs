
module Data.Repa.Store.Object.Family
        (Family (..))
where
import Data.Repa.Store.Object.Column
import Control.Monad
import Data.Text
import Data.Aeson                               as A
import qualified Data.Repa.Store.Format         as F
import qualified Data.HashMap.Strict            as H


-- | Describes a column family in the store.
data Family
        = Family
        { -- | Name of family.
          familyName            :: Text

          -- | Format of parent dimension.
        , familyFormat          :: F.FieldBox

          -- | Human readable description of family.
        , familyDescription     :: Text 

          -- | Meta-data for associated columns,
          --   or `Nothing` if it hasn't been loaded.
        , familyColumns         :: Maybe [Column] 

          -- | Local directory that holds the column family, if known.
          --   (not serialised)
        , familyDirectory       :: Maybe FilePath }
        deriving Show


instance ToJSON Family where
 toJSON (Family name format desc mColumns _mDirectory)
  =  object $    [ "_type"      .= text "family"
                 , "name"       .= toJSON name
                 , "format"     .= toJSON format
                 , "desc"       .= toJSON desc ]
  ++ (maybe [] (\columns ->
                 [ "columns"    .= toJSON columns]) mColumns)


instance FromJSON Family where
 parseJSON (Object hh)
        | Just (String "family") <- H.lookup "_type"  hh
        , Just (String name)     <- H.lookup "name"   hh
        , Just jField            <- H.lookup "format" hh
        , Just (String desc)     <- H.lookup "desc"   hh
        = do
                field    <- parseJSON jField

                mColumns <- case H.lookup "columns" hh of
                                Nothing  -> return Nothing
                                Just txt -> liftM Just $ parseJSON txt

                return $ Family name field desc
                                mColumns Nothing

 parseJSON _ = mzero


text :: Text -> Text
text x = x
