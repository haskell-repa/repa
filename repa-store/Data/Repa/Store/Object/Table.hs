
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
        ( Table  (..)
        , Column (..)
        , loadMeta, ErrorLoadMeta
        , listPartitions)
where
import System.FilePath
import Control.Monad
import Data.Text                                (Text)
import Data.List
import Data.Maybe
import Data.Aeson                               as A
import qualified Data.HashMap.Strict            as H
import qualified Data.ByteString.Lazy.Char8     as BS
import qualified System.Directory               as S      


---------------------------------------------------------------------------------------------------
-- | Describes a single table.
data Table
        = Table
        { -- | Table name
          tableName      :: Text

          -- | Table columns.
        , tableColumns   :: [Column] }
        deriving Show


instance ToJSON Table where
 toJSON (Table name columns)
        = object [ "type"    .= text "table"
                 , "name"    .= toJSON name
                 , "columns" .= toJSON columns]


instance FromJSON Table where
 parseJSON (Object hh)
        | Just (String "table")         <- H.lookup "type"    hh
        , Just jName                    <- H.lookup "name"    hh
        , Just jColumns                 <- H.lookup "columns" hh
        = do    name    <- parseJSON jName
                columns <- parseJSON jColumns
                return  $ Table name columns

 parseJSON _ = mzero


---------------------------------------------------------------------------------------------------
-- | Describes a single column in a table.
data Column 
        = Column
        { -- | Name of column.
          columnName    :: Text

          -- | On-disk data format of column.
        , columnFormat  :: Text

          -- | English description of column.
        , columnDesc    :: Text }
        deriving (Eq, Show)


instance ToJSON Column where
 toJSON (Column name format desc)
        = object [ "type"   .= text "column"
                 , "name"   .= toJSON name
                 , "format" .= toJSON format
                 , "desc"   .= toJSON desc ]


instance FromJSON Column where
 parseJSON (Object hh)
        | Just (String "column")        <- H.lookup "type"   hh
        , Just (String name)            <- H.lookup "name"   hh
        , Just (String format)          <- H.lookup "format" hh
        , Just (String desc)            <- H.lookup "desc"   hh
        = return $ Column name format desc

 parseJSON _ = mzero


text :: Text -> Text
text x = x


---------------------------------------------------------------------------------------------------
-- | Load table meta-data from the given table directory.
--
--   The path should be a directory holding the table data, 
--   like "sometable.t"
--
loadMeta :: FilePath -> IO (Either ErrorLoadMeta Table)     
loadMeta path
 = check
 where  pathMeta        = path </> "_table.json"

        check
         = do   hasTableDir     <- S.doesDirectoryExist path
                if not hasTableDir 
                 then return $ Left $ ErrorLoadMetaNoDir path
                 else load

        load
         = do   hasMetaFile     <- S.doesFileExist pathMeta
                if not hasMetaFile 
                 then return $ Left $ ErrorLoadMetaNoMeta pathMeta
                 else parse

        parse
         = do   strMeta <- BS.readFile pathMeta
                case A.decode strMeta of
                 Nothing   -> return $ Left $ ErrorLoadMetaMalformed pathMeta
                 Just meta -> return $ Right meta


-- | Errors that can happen when loading table meta data.
data ErrorLoadMeta
          -- | Table directory does not exist.
        = ErrorLoadMetaNoDir     FilePath

          -- | Table directory does not include meta-data.
        | ErrorLoadMetaNoMeta    FilePath

          -- | Table meta-data is malformed.
        | ErrorLoadMetaMalformed FilePath
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Given a table directory and its meta-data, 
--   get a list of all table partitions.
--
listPartitions :: FilePath -> Table -> IO (Maybe [FilePath])
listPartitions path _table
 = check
 where  
        check 
         = do   hasTableDir     <- S.doesDirectoryExist path
                if not hasTableDir
                 then return $ Nothing
                 else list

        list 
         = do   files   <- liftM (filter (\x -> not $ elem x [".", ".."]))
                        $  S.getDirectoryContents path

                mfs     <- liftM catMaybes  
                        $  mapM  slurp 
                        $  map  (path </>) files

                return  $ Just $ sort (liftM concat mfs)


        slurp :: FilePath -> IO (Maybe [FilePath])
        slurp file
         = case takeExtension file of
                ".tsv"  -> return $ Just [file]
                _       -> return Nothing

