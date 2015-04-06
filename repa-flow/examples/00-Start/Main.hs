
module Main where
import Data.Repa.Flow           as F
import Data.Repa.Array          as A
import Data.Char

main 
 = do   ws  <- fromFiles'
                [ "/usr/share/dict/words"
                , "/usr/share/dict/cracklib-small"] 
                sourceLines

        up  <- map_i (A.map toUpper) ws
        out <- toFiles' ["out1.txt", "out2.txt"] $ sinkLines
        drainS up out
