
module Data.Repa.Store.Object.Dimension
        (Dimension (..))
where
import Control.Monad
import Data.Text
import Data.Aeson                               as A
import qualified Data.Repa.Store.Format         as F
import qualified Data.HashMap.Strict            as H


-- | Describes a dimension of the store.
data Dimension
        = Dimension
        { -- | Name of dimension.
          dimensionName         :: Text

          -- | On-disk data format of key column.
        , dimensionKeyFormat    :: F.FieldBox

          -- | Human readable description of column.
        , dimensionDescription  :: Text }
        deriving Show


instance ToJSON Dimension where
 toJSON (Dimension name format desc)
        = object [ "_type"      .= text "dimension"
                 , "name"       .= toJSON name
                 , "format"     .= toJSON format
                 , "desc"       .= toJSON desc ]


instance FromJSON Dimension where
 parseJSON (Object hh)
        | Just (String "dimension") <- H.lookup "_type" hh
        , Just (String name)     <- H.lookup "name"   hh
        , Just jField            <- H.lookup "format" hh
        , Just (String desc)     <- H.lookup "desc"   hh
        = do
                field   <- parseJSON jField
                return $ Dimension name field desc

 parseJSON _ = mzero


text :: Text -> Text
text x = x
