
-- Split the rows in a CSV or TSV file into separate files based on 
-- the first field. See `Config.hs` for an example.
import Config
import Data.Repa.Flow                                   as F
import Data.Repa.Flow.Auto.IO                           as F
import Data.Repa.Array                                  as A
import qualified Data.Repa.Flow.Generic                 as G
import qualified Data.Repa.Flow.Generic.IO              as G
import qualified Data.Repa.Array.Generic                as G
import qualified Data.Repa.Array.Material.Foreign       as G
import System.Environment
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.Maybe
import Data.Word
import Data.Char
import Prelude                                  as P


main :: IO ()
main 
 = do   args    <- getArgs
        config  <- parseArgs args configZero

        mapM_ (pSieve config) 
         $ configInFiles config


pSieve :: Config -> FilePath -> IO ()
pSieve config fileIn
 = do   
        -- Stream the input file.
        let ext = takeExtension fileIn

        sIn     <-  G.project_i 0
                =<< (fromFiles [fileIn]
                      $ (if | ext == ".tsv" -> F.sourceTSV
                            | ext == ".csv" -> F.sourceCSV
                            | otherwise     -> error $ "unknown format " ++ show ext))

        -- Flatten the stream of chunks into a stream of rows.
        sRows   <- G.unchunk_i sIn

        -- Sieve out rows into separate files based on the 
        -- first field in each row.
        let !dirOut     = fromMaybe "." $ configOutDir config
        createDirectoryIfMissing True dirOut
        oSieve  <- G.sieve_o (sieveRow config dirOut)
        G.drainS sRows oSieve


-- | Produde the filename and output for a single row.
sieveRow 
        :: Config                               -- ^ Sieve Configuration.
        -> FilePath                             -- ^ Output directory.
        -> Array (Array Char)                   -- ^ Row of fields of data.
        -> Maybe (FilePath, G.Array G.F Word8)

sieveRow config dirOut arrFields
 = let  
        -- The file to write this row to.
        file    = dirOut </> A.toList (arrFields `index` 0)

        -- Insert any extra fields into the row.
        fInsert ix'
         = case configInsertColumn config of
                []                   -> Nothing
                (ix, _name, val) : []
                  | ix == ix'        -> Just $ A.fromList val
                  | otherwise        -> Nothing
                _ -> error "TODO: multi inserts not handled"
        {-# INLINE fInsert #-}

        !arrIns  = A.insert fInsert arrFields

        -- Concatenate fields back into a flat array.
        !arrC    = A.fromList ['\t']
        !arrNL   = A.fromList ['\n']

        !arrFlat = A.concat $ A.fromList [ A.intercalate arrC arrIns, arrNL ]

   in   Just ( file
             , G.mapS G.F (fromIntegral . ord) arrFlat)
{-# INLINE sieveRow #-}


