
module Data.Repa.Store.Resolve
        ( resolveObject
        , ResolvePart           (..)
        , ErrorResolveObject    (..))
where
import Data.Repa.Store.Object
import System.FilePath                          as FilePath
import qualified Data.List                      as L
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as Text
import qualified System.Directory               as System


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
        go_objs acc objs (p_ : [])
         | (p, _) <- L.span (/= '/') p_
         = let  objMap  = Map.fromList $ zip (map nameOfObject objs) objs

           in   case Map.lookup (Text.pack p) objMap of
                 Nothing  -> return $ Left  $ ErrorResolveObjectNotFound path
                 Just obj -> return $ Right $ acc ++ [ResolveObject obj]

        -- Enter into a container object named after the next
        -- component of the path.
        go_objs acc objs (p : ps)
         = let  (p', _) = L.span (/= '/') p
                objMap  = Map.fromList $ zip (map nameOfObject objs) objs

           in   case Map.lookup (Text.pack p') objMap of
                 Nothing
                  -> return $ Left $ ErrorResolveObjectNotFound path

                 Just obj
                  -> let objsChildren  = childrenOfObject obj
                     in  go_objs (acc ++ [ResolveObject obj]) objsChildren ps


-- | Part of the resolved collection of objects.
data ResolvePart
        = ResolveDir    FilePath 
        | ResolveObject Object
        deriving Show


-- | Errors that can happen when resolving an object name.
data ErrorResolveObject
        = ErrorResolveObjectEmptyPath FilePath
        | ErrorResolveObjectNotFound  FilePath
        | ErrorResolveObjectList      ErrorListObjects
        deriving Show
