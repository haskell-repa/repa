
-- | Drivers used by queries compiled via Repa.
--
--   This code is imported by the generated query code and used by the 
--   running queries, rather than being used at query compile time.
--
module Data.Repa.Query.Runtime.Driver
        ( execQuery
        , streamSourcesToStdout)
where
import Data.Repa.Flow                                   as F
import Data.Word
import Data.Maybe
import System.IO
import qualified Data.Repa.Flow.Generic                 as FG
import qualified Data.Repa.Array.Generic                as AG
import qualified Data.Repa.Array.Material.Foreign       as AF
import qualified Data.Repa.Array.Material.Auto          as AA
import qualified Foreign.Ptr                            as Foreign
import qualified Foreign.ForeignPtr                     as Foreign
import qualified System.Environment                     as System
import Prelude                                          as P
#include "repa-query.h"


---------------------------------------------------------------------------------------------------
-- | Top level driver for a query.
--
--   The query takes the path to the root data directory as its first argument.
--
execQuery :: (FilePath -> IO (Sources Word8)) -> IO ()
execQuery makeSources
 = do
        args    <- System.getArgs

        config  <- parseArgs args configZero
        let Just pathRootData = configRootData config

        ss      <- makeSources pathRootData
        True    <- streamSourcesToStdout ss
        return  ()
{-# INLINE execQuery #-}


-- | Query command-line config.
data Config
        = Config
        { configRootData        :: Maybe FilePath }

configZero 
        = Config Nothing


-- | Parse command line arguments given to query.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
 | isJust $ configRootData config
 = return config
 | otherwise = dieUsage

parseArgs args config
 | "-root-data" : path : rest   <- args
 = parseArgs rest $ config { configRootData = Just path }

 | otherwise
 = dieUsage

dieUsage 
 = error $ P.unlines
 [ "Usage: query -root-data <PATH>"
 , "Execute a Repa query."
 , ""
 , "OPTIONS:"
 , " -root-data PATH    (required) Root path containing table data." ]


---------------------------------------------------------------------------------------------------
-- | Read data from a bundle of sources and write it to stdout.
--  
--   This function only works for sources bundles containing a single stream.
--   If this is not true then the function returns False, and no data is
--   written to stdout.
--
streamSourcesToStdout :: Sources Word8 -> IO Bool
streamSourcesToStdout ss
 =   FG.funnel_i ss 
 >>= streamSourceToStdout
{-# INLINE_FLOW streamSourcesToStdout #-}


-- | Read data from a unitary stream and write it to stdout.
streamSourceToStdout   :: FG.Sources () IO (AA.Array AA.A Word8) -> IO Bool
streamSourceToStdout (FG.Sources () pullX)
 = do   go
        return True

 where  go 
         = pullX () eat_streamSource eject_streamSource
        {-# INLINE go #-}

        eat_streamSource (chunk :: AG.Array AA.A Word8)
         = do   let (start, len, fptr :: Foreign.ForeignPtr Word8) 
                        = AF.toForeignPtr $ AG.convert AF.F chunk

                Foreign.withForeignPtr fptr $ \ptr 
                 -> hPutBuf stdout (ptr `Foreign.plusPtr` start) len

                hFlush stdout
                go
        {-# INLINE eat_streamSource #-}

        eject_streamSource
         = do   hClose stdout
                return ()
        {-# INLINE eject_streamSource #-}
{-# INLINE_FLOW streamSourceToStdout #-}

