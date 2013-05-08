
module Solver where

import qualified Stream                 as S
import qualified Data.Vector.Unboxed    as U
import Control.Concurrent.MVar


thing :: U.Vector Int -> U.Vector Int -> IO (U.Vector Int)
thing vecLens vecElems
 = do   vLeft   <- newEmptyMVar
        vRight  <- newEmptyMVar
        
        S.unstreamUnboxed 
                $ S.map (+ 12345)
                $ S.foldSegsBubble
                        (+)
                        0 
                        (S.streamUnboxed vecLens)
                        (S.streamUnboxed vecElems)
                        (Just vLeft)
                        (Just vRight)

{-                $ S.foldSegsTrade 
                        (+) 
                        0
                        (S.streamUnboxed vecLens)
                        (S.streamUnboxed vecElems)
                        (Just vLeft)
                        (Just vRight)
-}
