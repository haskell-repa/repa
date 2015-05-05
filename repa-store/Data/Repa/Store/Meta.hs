
module Data.Repa.Store.Meta
        ( loadMeta
        , ErrorLoadMeta)
where
import Data.Repa.Store.Object.Table
import System.FilePath
import qualified Data.Aeson                     as A
import qualified System.Directory               as System      
import qualified Data.ByteString.Lazy.Char8     as BS


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
