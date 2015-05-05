
-- | Metadata about an object in the store.
module Data.Repa.Store.Object
        ( Object        (..)
        , ObjectType    (..)
        , objectExtensions
        , nameOfObject
        , typeOfObject
        , childrenOfObject

          -- * Listing Objects
        , listObjectsInDir
        , ErrorListObjects (..)

          -- * Loading Objects
        , loadObjectFromDir
        , ErrorLoadObject  (..)

          -- * Resolving object paths.
        , resolveObject
        , ResolvePart  (..)
        , ErrorResolveObject (..))
where
import Data.Repa.Store.Object.Dimension
import Data.Repa.Store.Object.Family
import Data.Repa.Store.Object.Column
import Data.Repa.Store.Object.Table

import System.FilePath                          as FilePath
import Control.Monad
import Data.Maybe
import Data.Text                                (Text)
import Data.Aeson                               ((.=))

import qualified Data.List                      as L
import qualified Data.Aeson                     as A
import qualified Data.HashMap.Strict            as H
import qualified Data.ByteString.Lazy.Char8     as BS
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as Text
import qualified System.Directory               as System


---------------------------------------------------------------------------------------------------
-- | Meta-data about an object in the store.
data Object 
        -- | A single dimension, which can have attached sub-dimensions
        --   and column families.
        = ObjectDimension Dimension

        -- | A family of columns that all have the same length.
        | ObjectFamily    Family

        -- | A single flat column.
        | ObjectColumn    Column

        -- | A flat table where all the columns are in the 
        --   same data set and have the same length.
        | ObjectTable     Table
        deriving Show


-- | Enumeration of known object types.
data ObjectType
        = ObjectTypeDimension
        | ObjectTypeFamily
        | ObjectTypeColumn
        | ObjectTypeTable
        deriving Show


-- | File extensions for the above objects.
objectExtensions :: [(ObjectType, String)]
objectExtensions 
 =      [ (ObjectTypeDimension, ".dimension")
        , (ObjectTypeFamily,    ".family")
        , (ObjectTypeColumn,    ".column") 
        , (ObjectTypeTable,     ".table")    ]


instance A.ToJSON Object where
 toJSON (ObjectDimension dimension)
        = A.object [ "_type"       .= text "object"
                   , "object"      .= text "dimension"
                   , "dimension"   .= A.toJSON dimension ]

 toJSON (ObjectFamily family)
        = A.object [ "_type"       .= text "object"
                   , "object"      .= text "family"
                   , "family"      .= A.toJSON family ]

 toJSON (ObjectColumn column)
        = A.object [ "_type"       .= text "object"
                   , "object"      .= text "column"
                   , "column"      .= A.toJSON column ]

 toJSON (ObjectTable table)
        = A.object [ "_type"       .= text "object"
                   , "object"      .= text "table"
                   , "table"       .= A.toJSON table ]


instance A.FromJSON Object where

 parseJSON (A.Object hh)
        | Just (A.String "object")    <- H.lookup "_type"     hh
        , Just (A.String "dimension") <- H.lookup "object"    hh
        , Just jDimension             <- H.lookup "dimension" hh
        = do    dimension <- A.parseJSON jDimension
                return    $  ObjectDimension dimension

 parseJSON (A.Object hh)
        | Just (A.String "object")    <- H.lookup "_type"     hh
        , Just (A.String "family")    <- H.lookup "object"    hh
        , Just jFamily                <- H.lookup "family" hh
        = do    family    <- A.parseJSON jFamily
                return    $  ObjectFamily family

 parseJSON (A.Object hh)
        | Just (A.String "object")    <- H.lookup "_type"     hh
        , Just (A.String "column")    <- H.lookup "object"    hh
        , Just jColumn                <- H.lookup "column"    hh
        = do    column    <- A.parseJSON jColumn
                return    $  ObjectColumn column

 parseJSON (A.Object hh)
        | Just (A.String "object") <- H.lookup "_type"  hh
        , Just (A.String "table")  <- H.lookup "object" hh
        , Just jTable              <- H.lookup "table"  hh
        = do    table     <- A.parseJSON jTable
                return    $ ObjectTable table

 parseJSON _ = mzero


text :: Text -> Text
text x = x


---------------------------------------------------------------------------------------------------
-- | Get the base name of an object.
nameOfObject :: Object -> Text
nameOfObject oo
 = case oo of
        ObjectDimension d       -> dimensionName d
        ObjectFamily f          -> familyName f
        ObjectColumn c          -> columnName c
        ObjectTable t           -> tableName t


-- | Get the type of an object.
typeOfObject :: Object -> ObjectType
typeOfObject oo
 = case oo of
        ObjectDimension{}       -> ObjectTypeDimension
        ObjectFamily{}          -> ObjectTypeFamily
        ObjectColumn{}          -> ObjectTypeColumn
        ObjectTable{}           -> ObjectTypeTable


-- | Get the child objects of the given one.
childrenOfObject :: Object -> [Object]
childrenOfObject oo
 = case oo of
        ObjectDimension dim     
         ->  map ObjectDimension (join $ maybeToList $ dimensionSubDimensions dim)
         ++  map ObjectFamily    (join $ maybeToList $ dimensionFamilies dim)

        ObjectFamily fam
         ->  map ObjectColumn    (join $ maybeToList $ familyColumns fam)

        ObjectColumn{} -> []

        ObjectTable tab
         ->  map ObjectColumn    (tableColumns tab)


---------------------------------------------------------------------------------------------------
-- | Get a list of objects available in a directory.
listObjectsInDir 
        :: FilePath 
        -> IO (Either ErrorListObjects [(FilePath, Object)])

listObjectsInDir path
 = check
 where  
        check
         = do   hasDir  <- System.doesDirectoryExist path
                if not hasDir
                 then return $ Left $ ErrorListObjectsNoDir path
                 else list

        list
         = do   files   <- liftM (filter (\x -> not $ elem x [".", ".."]))
                        $  System.getDirectoryContents path

                liftM (sequence . catMaybes) 
                        $ mapM diag $ map (path </>) files

        diag dir
         = do   let ext = takeExtension dir
                if elem ext [".dimension", ".family", ".column", ".table"]
                 then do r <- loadObjectFromDir dir
                         case r of
                          Left  err    
                           -> return $ Just $ Left $ ErrorListObjectsLoad err

                          Right object 
                           -> return $ Just $ Right (dir, object)

                 else return Nothing


-- | Errors that can happen when listing objects in a directory.
data ErrorListObjects
        = ErrorListObjectsNoDir FilePath
        | ErrorListObjectsLoad  ErrorLoadObject
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Given the name of the directory that holds some object,
--   load its associated meta-data.
--
loadObjectFromDir :: FilePath -> IO (Either ErrorLoadObject Object)
loadObjectFromDir path
 = check
 where  check
         = do   hasTableDir     <- System.doesDirectoryExist path
                if not hasTableDir 
                 then return $ Left $ ErrorLoadObjectNoDir path
                 else recognize

        recognize
         = case takeExtension path of
                ".dimension"    -> loadDimension
                ".family"       -> loadFamily
                ".column"       -> loadColumn
                ".table"        -> loadTable
                _               -> return $ Left $ ErrorLoadObjectUnrecognised path

        -- Dimension --------------------------------------
        loadDimension
         = do   let pathMeta    =  path </> "_dimension.json"
                hasMetaFile     <- System.doesFileExist pathMeta
                if not hasMetaFile
                 then return $ Left $ ErrorLoadObjectNoMeta pathMeta
                 else do strMeta <- BS.readFile pathMeta
                         case A.decode strMeta of
                          Nothing    -> return $ Left  $ ErrorLoadObjectMalformed pathMeta
                          Just meta  -> loadDimension_sub meta

        loadDimension_sub meta
         = do   eObjs    <- listObjectsInDir path
                case eObjs of
                 Left  err -> return $ Left $ ErrorLoadObjectList err
                 Right objs
                  -> do let objsSub    = [d | (_path, ObjectDimension d) <- objs]
                        let objsFamily = [f | (_path, ObjectFamily    f) <- objs]
                        return $ Right $ ObjectDimension 
                               $ meta   { dimensionSubDimensions = Just objsSub
                                        , dimensionFamilies      = Just objsFamily }


        -- Family -----------------------------------------
        loadFamily
         = do   let pathMeta    =  path </> "_family.json"
                hasMetaFile     <- System.doesFileExist pathMeta
                if not hasMetaFile
                 then return $ Left $ ErrorLoadObjectNoMeta pathMeta
                 else do strMeta <- BS.readFile pathMeta
                         case A.decode strMeta of
                          Nothing    -> return $ Left  $ ErrorLoadObjectMalformed pathMeta
                          Just meta  -> loadFamily_sub meta

        loadFamily_sub meta
         = do   eObjs   <- listObjectsInDir path
                case eObjs of
                 Left  err -> return $ Left $ ErrorLoadObjectList err
                 Right objs
                  -> do let objsCol    = [d | (_path, ObjectColumn d) <- objs]
                        return $ Right $ ObjectFamily
                               $ meta  { familyColumns = Just objsCol }


        -- Column ------------------------------------------
        loadColumn
         = do   let pathMeta    =  path </> "_column.json"
                hasMetaFile     <- System.doesFileExist pathMeta
                if not hasMetaFile
                 then return $ Left $ ErrorLoadObjectNoMeta pathMeta
                 else do strMeta <- BS.readFile pathMeta
                         case A.decode strMeta of
                          Nothing    -> return $ Left  $ ErrorLoadObjectMalformed pathMeta
                          Just meta  -> return $ Right $ ObjectColumn meta


        -- Table -------------------------------------------
        loadTable
         = do   let pathMeta    =  path </> "_table.json"
                hasMetaFile     <- System.doesFileExist pathMeta
                if not hasMetaFile 
                 then return $ Left $ ErrorLoadObjectNoDir pathMeta
                 else do strMeta <- BS.readFile pathMeta
                         case A.decode strMeta of
                           Nothing   -> return $ Left  $ ErrorLoadObjectMalformed pathMeta
                           Just meta -> return $ Right $ ObjectTable meta


-- | Errors that can happen when loading object meta data.
data ErrorLoadObject
        -- | Object directory does not exist.
        = ErrorLoadObjectNoDir     FilePath

        -- | Object directory extension is unrecognised.
        | ErrorLoadObjectUnrecognised FilePath

        -- | Object directory does not include meta-data.
        | ErrorLoadObjectNoMeta    FilePath

        -- | Object meta-data is malformed.
        | ErrorLoadObjectMalformed FilePath

        -- | Error encountered when listing sub-objects.
        | ErrorLoadObjectList      ErrorListObjects
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Given the logical path of an object, build a list of meta data
--   for the physical object that represents it, along with any
--   container objects.
--
--   For example, suppose we have the following directory structure:
--
-- @
-- home/
-- + data/
--   + historical/
--     + symbol.dimension/
--       + date.family/
--         + volume.column
--         + open.column
--         + close.column
-- @
-- 
-- Given the path "home/data/historical/symbol/date/volume",
-- we build a list of `ResolvePart`s, one for each part of the file path,
-- ending in a part that holds the meta-data for the target column.
--
resolveObject :: FilePath -> IO (Either ErrorResolveObject [ResolvePart])
resolveObject path
 = go_dir [] [] (FilePath.splitPath path)
 where
        -- Recursing into plain directories ---------------
        -- The path doesn't contain any components, like '.'
        go_dir _acc [] []  
         = return $ Left $ ErrorResolveObjectEmptyPath path

        -- We've entered into some directories, but there are no more parts to
        -- the path. Return the current directory as the result that was selected.
        go_dir acc _prev []
         = return $ Right acc

        -- See if there is anything in the current directory
        -- named like the next part in the path.
        go_dir acc prev (p : ps)
         = do   let path' = FilePath.joinPath $ prev ++ [p]
                isDir     <- System.doesDirectoryExist path'
                if isDir 
                 then do
                        -- The next part names a directory,
                        -- so enter into it. 
                        let r   = ResolveDir $ FilePath.joinPath (prev ++ [p])
                        go_dir (acc ++ [r]) (prev ++ [p]) ps
                 else do
                        -- The next part didn't name a directory,
                        -- so see if there is a store object with that name.
                        ePathObjs    <- listObjectsInDir (FilePath.joinPath prev)
                        case ePathObjs of
                         Left  err      -> return $ Left $ ErrorResolveObjectList err
                         Right pathObjs -> go_objs acc pathObjs (p : ps)


        -- Recursing into objects -------------------------
        -- We ran out of components to the path.
        go_objs _acc _pathObjs []
         = return $ Left $ ErrorResolveObjectEmptyPath path

        -- We can't find any object that matches the next component in the path.
        go_objs _acc []   (_p : _ps)
         = return $ Left $ ErrorResolveObjectNotFound  path

        -- We're at the final component in the path,
        -- so we're expecting it to name one of the objects here.
        go_objs acc pathObjs (p : [])
         = let  (p', _)         = L.span (/= '/') p
                (_pObjs, objs)  = unzip pathObjs
                objMap          = Map.fromList $ zip (map nameOfObject objs) pathObjs

           in   case Map.lookup (Text.pack p') objMap of
                 Nothing             -> return $ Left  $ ErrorResolveObjectNotFound path
                 Just (pathObj, obj) -> return $ Right $ acc ++ [ResolveObject pathObj obj]

        -- Enter into a container object named after the next
        -- component of the path.
        go_objs acc pathObjs  (p : ps)
         = let  (p', _)         = L.span (/= '/') p
                (_pObjs, objs)  = unzip pathObjs
                objMap          = Map.fromList $ zip (map nameOfObject objs) pathObjs

           in   case Map.lookup (Text.pack p') objMap of
                 Nothing
                  -> return $ Left $ ErrorResolveObjectNotFound path

                 Just (pathObj, obj) 
                  -> let objsChildren  = childrenOfObject obj
                         psObjChildren = zip (repeat "TODO") objsChildren       
                     in  go_objs (acc ++ [ResolveObject pathObj obj]) psObjChildren ps

                                -- TODO: we don't have the path for the child objects, 
                                --       and there won't be one for columns in tables.
                                --       Add a Maybe field to the object meta data types
                                --       to list the path that they are at.

-- | Part of the resolved collection of objects.
data ResolvePart
        = ResolveDir    FilePath 
        | ResolveObject FilePath Object
        deriving Show


-- | Errors that can happen when resolving an object name.
data ErrorResolveObject
        = ErrorResolveObjectEmptyPath FilePath
        | ErrorResolveObjectNotFound  FilePath
        | ErrorResolveObjectList      ErrorListObjects
        deriving Show





