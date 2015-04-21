
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
        , lookupColumn

          -- * Column
        , Column (..)

          -- * Metadata
        , loadMeta, ErrorLoadMeta

          -- * Partitions
        , listPartitions)
where
import System.FilePath
import Control.Monad
import Data.Text                                (Text)
import Data.List
import Data.Maybe
import Data.Aeson                               as A
import qualified Data.Repa.Store.Format         as F
import qualified Data.HashMap.Strict            as H
import qualified Data.ByteString.Lazy.Char8     as BS
import qualified System.Directory               as System      


---------------------------------------------------------------------------------------------------
-- | Describes a single table.
data Table
        = Table
        { -- | Table name
          tableName      :: Text

          -- | How rows and fields are deliminted in the data.
        , tableDelim    :: F.Delim

          -- | Table columns.
        , tableColumns   :: [Column] }
        deriving Show


instance ToJSON Table where
 toJSON (Table name delim columns)
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
                return  $ Table name delim columns

 parseJSON _ = mzero


-- | Lookup a single column in the table,
--   returning `Nothing` if it's not there.
lookupColumn :: Text -> Table -> Maybe Column
lookupColumn name table
 = find (\c -> name == columnName c) (tableColumns table)


---------------------------------------------------------------------------------------------------
-- | Describes a single column in a table.
data Column 
        = Column
        { -- | Name of column.
          columnName    :: Text

          -- | On-disk data format of column.
        , columnFormat  :: F.FieldBox

          -- | English description of column.
        , columnDesc    :: Text }
        deriving Show


instance ToJSON Column where
 toJSON (Column name field desc)
        = object [ "_type"  .= text "column"
                 , "name"   .= toJSON name
                 , "format" .= toJSON field
                 , "desc"   .= toJSON desc ]


instance FromJSON Column where
 parseJSON (Object hh)
        | Just (String "column")        <- H.lookup "_type"  hh
        , Just (String name)            <- H.lookup "name"   hh
        , Just jField                   <- H.lookup "format" hh
        , Just (String desc)            <- H.lookup "desc"   hh
        = do
                field   <- parseJSON jField
                return $ Column name field desc

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
         = do   hasTableDir     <- System.doesDirectoryExist path
                if not hasTableDir 
                 then return $ Left $ ErrorLoadMetaNoDir path
                 else load

        load
         = do   hasMetaFile     <- System.doesFileExist pathMeta
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
         = do   hasTableDir     <- System.doesDirectoryExist path
                if not hasTableDir
                 then return $ Nothing
                 else list

        list 
         = do   files   <- liftM (filter (\x -> not $ elem x [".", ".."]))
                        $  System.getDirectoryContents path

                mfs     <- liftM catMaybes  
                        $  mapM  slurp 
                        $  map  (path </>) files

                return  $ Just $ sort (liftM concat mfs)


        slurp :: FilePath -> IO (Maybe [FilePath])
        slurp file
         = case takeExtension file of
                ".tsv"  -> return $ Just [file]
                _       -> return Nothing

