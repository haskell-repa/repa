
module Data.Repa.Store.Object.Dimension
        (Dimension (..))
where
import Data.Repa.Store.Object.Family
import Control.Monad
import Data.Text
import Data.Aeson                               as A
import qualified Data.Repa.Store.Format         as F
import qualified Data.HashMap.Strict            as H


-- | Describes a dimension of the store.
data Dimension
        = Dimension
        { -- | Name of dimension.
          dimensionName          :: Text

          -- | On-disk data format of key column.
        , dimensionKeyFormat     :: F.FieldBox

          -- | Human readable description of column.
        , dimensionDescription   :: Text 

          -- | Meta-data for subdimensions,
          --   or `Nothing` if it hasn't been loaded.
        , dimensionSubDimensions :: Maybe [Dimension]

          -- | Meta-data for associated column families,
          --   or `Nothing` if it hasn't been loaded.
        , dimensionFamilies      :: Maybe [Family] 

          -- | Local directory that holds the column, if known.
          --   (not serialized)
        , dimensionDirectory     :: Maybe FilePath }
        deriving Show


instance ToJSON Dimension where
 toJSON (Dimension name format desc mSubDims mFamilies _mDirectory)
  =  object $    [ "_type"    .= text "dimension"
                 , "name"     .= toJSON name
                 , "format"   .= toJSON format
                 , "desc"     .= toJSON desc ]
  ++ (maybe [] (\subdims -> 
                 [ "subdims"  .= toJSON subdims])  mSubDims)
  ++ (maybe [] (\families ->
                 [ "families" .= toJSON families]) mFamilies)


instance FromJSON Dimension where
 parseJSON (Object hh)
        | Just (String "dimension") <- H.lookup "_type" hh
        , Just (String name)        <- H.lookup "name"   hh
        , Just jField               <- H.lookup "format" hh
        , Just (String desc)        <- H.lookup "desc"   hh
        = do
                field     <- parseJSON jField

                mSubDims  <- case H.lookup "subdims" hh of
                                Nothing  -> return Nothing
                                Just txt -> liftM Just $ parseJSON txt

                mFamilies <- case H.lookup "families" hh of
                                Nothing  -> return Nothing
                                Just txt -> liftM Just $ parseJSON txt

                return $ Dimension 
                                name field desc 
                                mSubDims mFamilies Nothing

 parseJSON _ = mzero


text :: Text -> Text
text x = x
