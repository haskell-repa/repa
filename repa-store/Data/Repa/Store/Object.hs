
-- | Metadata about an object in the store.
module Data.Repa.Store.Object
        ( Object           (..)
        , objectName
        , listObjects
        , ErrorListObjects (..))
where
import Data.Repa.Store.Object.Table
import Data.Repa.Store.Meta
import System.FilePath
import Data.Maybe
import Control.Monad
import Data.Text                        (Text)
import Data.Aeson                       ((.=))
import qualified Data.Aeson             as A
import qualified Data.HashMap.Strict    as H
import qualified System.Directory       as S


-- | Meta-data about an object in the store.
data Object 
        = ObjectTable   Table
        deriving Show


instance A.ToJSON Object where
 toJSON (ObjectTable table)
        = A.object [ "type"        .= text "object"
                   , "object"      .= text "table"
                   , "table"       .= A.toJSON table ]

instance A.FromJSON Object where
 parseJSON (A.Object hh)
        | Just (A.String "object") <- H.lookup "type"   hh
        , Just (A.String "table")  <- H.lookup "object" hh
        , Just jTable              <- H.lookup "table"  hh
        = do    table   <- A.parseJSON jTable
                return  $ ObjectTable table

 parseJSON _ = mzero

text :: Text -> Text
text x = x


---------------------------------------------------------------------------------------------------
-- | Get the base name of some object.
objectName :: Object -> Text
objectName oo
 = case oo of
        ObjectTable t   -> tableName t


---------------------------------------------------------------------------------------------------
-- | Get a list of objects available in a directory.
listObjects :: FilePath -> IO (Either ErrorListObjects [Object])
listObjects path
 = check
 where  
        check
         = do   hasDir  <- S.doesDirectoryExist path
                if not hasDir
                 then return $ Left $ ErrorListObjectsNoDir path
                 else list

        list
         = do   files   <- liftM (filter (\x -> not $ elem x [".", ".."]))
                        $  S.getDirectoryContents path

                liftM (sequence . catMaybes) 
                        $ mapM diag $ map (path </>) files

        diag file
         = case takeExtension file of
             ".t" -> loadMeta file >>= \r
                  -> case r of
                        Left  err   -> return $ Just $ Left  $ ErrorListObjectsLoad err
                        Right table -> return $ Just $ Right $ ObjectTable table

             _    -> return Nothing


-- | Errors that can happen when listing objects in a directory.
data ErrorListObjects
        = ErrorListObjectsNoDir FilePath
        | ErrorListObjectsLoad  ErrorLoadMeta
        deriving Show

