
module Data.Repa.Query
        ( -- * Types
          Q.Q, Q.Query, Q.Flow, Q.Value

          -- * Query execution
        , runWith

          -- * Query construction
        , Q.query

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.
        , Q.Delim  (..)
        , Q.Field  (..)

          -- ** Sourcing
        , Q.fromFile
        , Q.fromTable
        , Q.fromColumn
        , Q.fromColumns

          -- ** Mapping
        , Q.map, Q.map2, Q.map3, Q.map4, Q.map5

          -- ** Folding
        , Q.fold
        , Q.folds

          -- ** Filtering
        , Q.filter

          -- ** Grouping
        , Q.groups
        , Q.groupsBy

          -- ** Dates
        , Q.yearOfDate
        , Q.monthOfDate
        , Q.dayOfDate

          -- ** Constructors
        , Q.row0, Q.row1, Q.row2, Q.row3, Q.row4
        , Q.row5, Q.row6, Q.row7, Q.row8, Q.row9

          -- ** Projections
        , Q.get2_1, Q.get2_2
        , Q.get3_1, Q.get3_2, Q.get3_3
        , Q.get4_1, Q.get4_2, Q.get4_3, Q.get4_4
        , Q.get5_1, Q.get5_2, Q.get5_3, Q.get5_4, Q.get5_5)
where
import qualified Data.Repa.Query.Source                 as Q
import qualified Data.Repa.Query.Source.Builder         as Q
import qualified Data.Repa.Query.Build                  as Q
import qualified System.Process                         as S
import qualified BuildBox                               as BB
import qualified BuildBox.Command.File                  as BB
import Prelude                                          as P


-- | Run a query locally.
runWith :: FilePath     -- ^ Scratch path for temp files.
        -> FilePath     -- ^ Root path to data.
        -> Q.Q Q.Query  -- ^ Query to run.
        -> IO ()

runWith pathScratch pathRootData qquery
 = do   let config  = Q.Config pathRootData
        equery  <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> do r <- BB.runBuild pathScratch (run_query q)
                case r of
                 Left berr  -> error (show berr)
                 Right _    -> P.return ()

 where
        run_query q
         = BB.withTempFile P.$ \fout
         -> do  Q.buildQueryViaRepa 
                        pathScratch True
                        q fout

                BB.io P.$ S.system P.$ fout ++ " -root-data " ++ pathRootData
