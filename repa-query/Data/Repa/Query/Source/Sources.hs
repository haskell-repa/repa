
module Data.Repa.Query.Source.Sources
        ( fromFile
        , fromTable
        , fromColumn
        , fromColumns)
where
import System.FilePath
import Control.Monad.State.Strict
import Data.Repa.Query.Source.Builder           as B
import qualified Data.Repa.Store.Object.Table   as Table
import qualified Data.Repa.Query.Graph          as G
import qualified Data.Repa.Store.Format         as F
import qualified Data.Text                      as T
import qualified Prelude                        as P
import Prelude   


-- | Read complete rows from a flat file.
fromFile  :: FilePath
          -> F.Delim 
          -> F.Field a 
          -> Q (Flow a)

fromFile path delim field
 = do   fOut    <- newFlow
        addNode $ G.NodeSource 
                $ G.SourceFile () path delim (F.flattens field) 
                $ takeFlow fOut
        return  fOut


-- | Read complete rows from a table.
fromTable  :: FilePath -> Q (Flow a)
fromTable path 
 = do   
        -- Load meta-data for the table.
        pathRoot <- B.getRootDataQ
        emeta    <- B.liftIO $ Table.loadMeta (pathRoot </> path)

        case emeta of
         Left errLoadMeta
          -> failQ $ show errLoadMeta

         Right table
          -> do fOut    <- newFlow
                addNode $ G.NodeSource
                        $ G.SourceTable () path 
                                (Table.tableDelim table)
                                (P.map Table.columnFormat $ Table.tableColumns table)
                        $ takeFlow fOut
                return fOut


-- | Read a named column from a table.
fromColumn :: FilePath -> String -> Q (Flow a)
fromColumn path name
 = do
        -- Load meta-data for the table.
        pathRoot <- B.getRootDataQ
        emeta    <- B.liftIO $ Table.loadMeta (pathRoot </> path)

        -- Function to lookup the index of a named column.
        let lookupColIx table nameCol
                | Just ix
                <- P.lookup nameCol 
                         $  P.zip  (P.map (T.unpack . Table.columnName) 
                                          (Table.tableColumns table))
                                   [0..]
                = Right (nameCol, ix)

                | otherwise
                = Left nameCol

        case emeta of
         Left errLoadMeta
          -> failQ $ show errLoadMeta

         Right table
          -- Work out what index the requested column is in the table.
          -- We store both the name and index to help detect if the
          -- table has changed since we built the query.
          -> case lookupColIx table name of
              Right colIx
               -> do    fOut    <- newFlow
                        addNode $ G.NodeSource
                                $ G.SourceTableColumn () path 
                                        (Table.tableDelim table)
                                        (P.map Table.columnFormat $ Table.tableColumns table)
                                        colIx
                                $ takeFlow fOut
                        return fOut

              Left nameUnknown 
               -> failQ $ "unknown column " ++ show nameUnknown


-- | Read a named column from a table.
fromColumns :: FilePath -> [String] -> Q (Flow a)
fromColumns path names
 = do
        -- Load meta-data for the table.
        pathRoot <- B.getRootDataQ
        emeta    <- B.liftIO $ Table.loadMeta (pathRoot </> path)

        -- Function to lookup the index of a named column.
        let lookupColIx table name
                | Just ix
                <- P.lookup name 
                         $  P.zip  (P.map (T.unpack . Table.columnName) 
                                          (Table.tableColumns table))
                                   [0..]
                = Right (name, ix)

                | otherwise
                = Left name

        case emeta of
         Left errLoadMeta
          -> failQ $ show errLoadMeta

         Right table
          -- Work out what index the requested column is in the table.
          -- We store both the name and index to help detect if the
          -- table has changed since we built the query.
          -> case P.sequence $ P.map (lookupColIx table) names of
              Right colIxs
               -> do    fOut    <- newFlow
                        addNode $ G.NodeSource
                                $ G.SourceTableColumns () path 
                                        (Table.tableDelim table)
                                        (P.map Table.columnFormat $ Table.tableColumns table)
                                        colIxs
                                $ takeFlow fOut
                        return fOut

              Left nameUnknown 
               -> failQ $ "unknown column " ++ show nameUnknown


