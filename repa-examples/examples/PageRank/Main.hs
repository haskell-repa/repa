{-# LANGUAGE BangPatterns #-}

import Data.Conduit.Binary      as B
import Data.Conduit.List        as C
import Data.Conduit.Text        as T
import Data.Conduit             as C
import System.Environment
import Data.IntMap              as M
import Control.Monad
import qualified GrowBuffer     as G

main
 = do   [filePath]      <- getArgs
        buffer          <- G.new 1 1.5

        C.runResourceT 
         $  B.sourceFile filePath
--         $= T.decode T.utf8
         $= B.lines
         $$ C.foldM
            (\(n, mm) line
                -> do   unsafeLiftIO
                         $ G.extend mm line

                        let !n'  = n + 1

                        when (n `mod` 100000 == 0)
                         $ unsafeLiftIO
                         $ do   putStrLn $ "loaded " ++ show n

                        return (n', mm))
            (0, buffer)


