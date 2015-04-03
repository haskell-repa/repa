{-# LANGUAGE PatternGuards, PackageImports  #-}

import Solver
import Data.Array.Repa                  as A
import Data.Array.Repa.IO.Matrix
import Data.Array.Repa.IO.Timing
import Data.Array.Repa.Algorithms.Randomish
import Data.Maybe
import System.Environment
import Control.Monad
import System.Random
import Prelude                          as P

-- Arg Parsing ----------------------------------------------------------------
data Arg
        = ArgSolver       String
        | ArgMatrixRandom Int Int
        | ArgMatrixFile   FilePath
        | ArgOutFile      FilePath
        deriving Show

isArgMatrix arg
 = case arg of
        ArgMatrixRandom{}       -> True
        ArgMatrixFile{}         -> True
        _                       -> False

parseArgs []            = []
parseArgs (flag:xx)
        | "-file"       <- flag
        , f:rest        <- xx
        = ArgMatrixFile f : parseArgs rest

        | "-out"        <- flag
        , f:rest        <- xx
        = ArgOutFile f  : parseArgs rest
        
        | "-random"     <- flag
        , x:y:rest      <- xx
        = ArgMatrixRandom (read x) (read y) : parseArgs rest
        
        | otherwise     
        = error $ "bad arg " P.++ flag P.++ "\n"

printHelp
        = putStr        
        $ unlines
        [ "Usage: mmult [args..]"
        , ""
        , "  -random <height> <width>   Use a random matrix of this size."
        , "  -file   <filename>         Read a matrix from this file."
        , "  -out    <filename>         Write resulting matrix to this file."
        , ""
        , "  Format of matrix file:"
        , "    MATRIX"
        , "    <width> <height>"
        , "    <whitespace separated values..>"
        , "" ]


-- | Get a matrix from a file, or generate a random one.
getMatrix :: Arg -> IO (Array U DIM2 Double)
getMatrix arg
 = case arg of
        ArgMatrixFile   fileName        
         -> readMatrixFromTextFile fileName

        ArgMatrixRandom height width    
         -> return $ randomishDoubleArray (Z :. height :. width) (-100) 100 12345


-- Main -----------------------------------------------------------------------
main :: IO ()
main 
 = do   args    <- liftM parseArgs $ getArgs
        main' args

main' args
        | [argMat1, argMat2]    <- filter isArgMatrix args
        , mArgOut               <- listToMaybe [s | ArgOutFile s <- args]
        = do    
                -- Get matrices from files, 
                -- or generate random ones we were asked to.
                mat1            <- getMatrix argMat1
                mat2            <- getMatrix argMat2

                mat1 `deepSeqArray` mat2 `deepSeqArray` return ()
                
                -- Run the solver.
                (matResult, t)  <- time $ mmultP mat1 mat2

                -- Print how long it took.
                putStr (prettyTime t)

                -- Print a checksum of all the elements
                checkSum        <- A.sumAllP matResult
                putStrLn $ "checkSum        = " P.++ show checkSum

                -- Write the output to file if requested.
                case mArgOut of 
                 Nothing        -> return ()
                 Just fileOut   -> writeMatrixToTextFile fileOut matResult
                                        
        | otherwise
        = printHelp

