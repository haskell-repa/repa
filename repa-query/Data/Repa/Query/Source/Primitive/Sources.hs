
module Data.Repa.Query.Source.Primitive.Sources
        ( fromFile
        , fromStore
        , fromStoreColumns)
where
import System.FilePath
import Control.Monad.State.Strict
import Data.Repa.Query.Source.Builder           as B
import qualified Data.Repa.Store.Resolve        as O
import qualified Data.Repa.Store.Object         as O
import qualified Data.Repa.Store.Object.Table   as O
import qualified Data.Repa.Store.Object.Family  as O
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


---------------------------------------------------------------------------------------------------
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
           : O.ResolveObject (O.ObjectFamily family)  : _
                        <- reverse parts
          ,  Just dirFamily <- O.familyDirectory family
          ,  Just dirColumn <- O.columnDirectory column
          -> do 
                fOut    <- newFlow
                addNode $ G.NodeSource
                        $ G.SourceFamilyColumns () 
                                dirFamily 
                                [dirColumn]
                                (O.familyFormat family)
                                [O.columnFormat column]
                        $ takeFlow fOut
                return fOut

          | otherwise
          -> failQ "repa-query.fromStore: not found"


---------------------------------------------------------------------------------------------------
-- | Source multiple columns from a compound object,
--   like a table or column family.
fromStoreColumns :: FilePath -> [String] -> Q (Flow a)
fromStoreColumns path nameColsWanted
 = do
        pathRoot  <- B.getRootDataQ
        let path' =  pathRoot </> path
        eObject   <- B.liftIO $ O.resolveObject path'

        case eObject of
         Left err
          -> failQ $ show err

         Right parts
          -- Source multiple columns from a column family.
          | O.ResolveObject (O.ObjectFamily family) : _ <- reverse parts
          , Just dirFamily  <- O.familyDirectory family
          , Just colsAll    <- O.familyColumns family
          , nameColsAll     <- zip (map O.columnName colsAll) colsAll
          , Just colsWanted <- sequence [ lookup (T.pack name) nameColsAll 
                                        | name <- nameColsWanted ]
          -> do 
                fOut    <- newFlow
                addNode $  G.NodeSource
                        $  G.SourceFamilyColumns ()
                                dirFamily
                                [ dirFamily </> name ++ ".column" | name <- nameColsWanted]
                                (O.familyFormat family)
                                (map O.columnFormat colsWanted)
                        $ takeFlow fOut
                return fOut

          | otherwise
          -> failQ "repa-query.fromStoreColumns: not found"

