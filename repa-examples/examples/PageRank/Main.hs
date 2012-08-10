{-# LANGUAGE BangPatterns #-}

import Data.Conduit.Binary      as B
import Data.Conduit.List        as C
import Data.Conduit.Text        as T
import Data.Conduit             as C
import System.Environment
import Data.IntMap              as M
import Control.Monad

main
 = do   [filePath]      <- getArgs

        C.runResourceT 
         $  B.sourceFile filePath
         $= T.decode T.utf8
         $= T.lines
         $$ C.foldM
            (\(n, mm) line
                -> do   let !mm' = M.insert n line mm
                        let !n'  = n + 1

                        when (n `mod` 100000 == 0)
                         $ unsafeLiftIO
                         $ do   putStrLn $ "loaded " ++ show n

                        return (n', mm'))
            (0, M.empty)


accumulate :: U.Vector Rank -> 
