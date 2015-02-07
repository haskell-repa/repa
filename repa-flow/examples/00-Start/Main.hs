
module Main where
import Data.Repa.Flow               as R
import Data.Repa.Flow.Default.Debug as R
import Data.Char


main 
 = do   
        ws  <- fromFiles 
                [ "/usr/share/dict/words"
                , "/usr/share/dict/cracklib-small"] 
                sourceLines

        up  <- map_i B (mapS U toUpper) ws
        out <- toFiles ["out1.txt", "out2.txt"] $ sinkLines B U
        drainS up out
