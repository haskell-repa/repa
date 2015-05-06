
module Data.Repa.Query.Source.Primitive.Sources
        ( fromFile
        , fromStore)
where
import System.FilePath
import Control.Monad.State.Strict
import Data.Repa.Query.Source.Builder           as B
import qualified Data.Repa.Store.Object         as O
import qualified Data.Repa.Store.Object.Table   as O
-- import qualified Data.Repa.Store.Object.Family  as O
import qualified Data.Repa.Store.Object.Column  as O
import qualified Data.Repa.Query.Graph          as G
import qualified Data.Repa.Store.Format         as F
import qualified Data.Text                      as T
import qualified Prelude                        as P
import Prelude   


-- | Read complete rows from a local file.
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


-- | Read the object at the given path in the store.
fromStore :: FilePath -> Q (Flow a)
fromStore path
 = do   pathRoot  <- B.getRootDataQ
        let path' =  pathRoot </> path
        eObject   <- B.liftIO $ O.resolveObject path'

        case eObject of
         Left err
          -> failQ $ show err

         Right parts
          -- Source entire rows from a table.
          |  O.ResolveObject (O.ObjectTable table) : _ 
                        <- reverse parts
          ,  Just dir   <- O.tableDirectory table
          -> do fOut    <- newFlow
                addNode $ G.NodeSource
                        $ G.SourceTable () dir
                                (O.tableDelim table)
                                (P.map O.columnFormat $ O.tableColumns table)
                        $ takeFlow fOut
                return fOut

          -- Source a single column from a table.
          |  O.ResolveObject (O.ObjectColumn column) 
           : O.ResolveObject (O.ObjectTable table)  : _
                        <- reverse parts
          ,  Just dir   <- O.tableDirectory table
          -> do 
                let colNames    = map O.columnName $ O.tableColumns table
                let Just colIx  = lookup (O.columnName column) 
                                $ zip colNames [0 :: Int ..]

                fOut    <- newFlow
                addNode $ G.NodeSource
                        $ G.SourceTableColumn () dir
                                (O.tableDelim table)
                                (P.map O.columnFormat $ O.tableColumns table)
                                (T.unpack $ O.columnName column, colIx)
                        $ takeFlow fOut
                return fOut


          -- Source a single column from a column family
          |  O.ResolveObject (O.ObjectColumn column) 
           : O.ResolveObject (O.ObjectFamily _family)  : _
                        <- reverse parts
          ,  Just dir   <- O.columnDirectory column
          -> do 
                fOut    <- newFlow
                addNode $ G.NodeSource
                        $ G.SourceFamilyColumn () dir
                                (O.columnFormat column)
                        $ takeFlow fOut
                return fOut

          | otherwise
          -> failQ "repa-query.fromStore: not found"


---------------------------------------------------------------------------------------------------
--  Read some named column from a table.
--
--   TODO: The columns come out in the order they were in the table,
--   rather than the order specified in the list. This is because
--   we're just reading all the fields in a row and masking the ones
--   that we don't want.
--
{-
fromColumns :: FilePath -> [String] -> Q (Flow a)
fromColumns path names
 = do
        -- Load meta-data for the table.
        pathRoot <- B.getRootDataQ
        emeta    <- B.liftIO $ O.loadObjectFromDir (pathRoot </> path)

        -- Function to lookup the index of a named column.
        let lookupColIx table name
                | Just ix
                <- P.lookup name 
                         $  P.zip  (P.map (T.unpack . O.columnName) 
                                          (O.tableColumns table))
                                   [0..]
                = Right (name, ix)

                | otherwise
                = Left name

        case emeta of
         Left errLoadMeta
          -> failQ $ show errLoadMeta

         Right (O.ObjectTable table)
          -- Work out what index the requested column is in the table.
          -- We store both the name and index to help detect if the
          -- table has changed since we built the query.
          -> case P.sequence $ P.map (lookupColIx table) names of
              Right colIxs
               -> do    fOut    <- newFlow
                        addNode $ G.NodeSource
                                $ G.SourceTableColumns () path 
                                        (O.tableDelim table)
                                        (P.map O.columnFormat $ O.tableColumns table)
                                        colIxs
                                $ takeFlow fOut
                        return fOut

              Left nameUnknown 
               -> failQ $ "unknown column " ++ show nameUnknown

         Right _ -> failQ $ "not a table"
-}

