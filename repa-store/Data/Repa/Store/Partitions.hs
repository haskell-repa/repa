
module Data.Repa.Store.Partitions
        (listPartitions)
where
import System.FilePath
import Control.Monad
import Data.Maybe
import qualified Data.List              as L
import qualified System.Directory       as System


-- | Given a table directory and its meta-data, 
--   get a list of all table partitions.
--
listPartitions :: FilePath ->  IO (Maybe [FilePath])
listPartitions path 
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

                return  $ Just $ L.sort (liftM concat mfs)


        slurp :: FilePath -> IO (Maybe [FilePath])
        slurp file
         = case takeExtension file of
                ".tsv"  -> return $ Just [file]
                ".txt"  -> return $ Just [file]
                _       -> return Nothing
