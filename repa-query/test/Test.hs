{-# LANGUAGE ScopedTypeVariables #-}

import Data.Repa.Query.EDSL                     as Q
import Data.Repa.Query.JSON                     as Q
import qualified Data.Repa.Query.Graph          as G

import Data.Aeson                               (encode, toJSON, fromJSON, Result)
import qualified Data.ByteString.Lazy.Char8     as BS
import qualified Prelude
import Prelude (($), Int, return)


q1 
 = do   (f :: Flow Int) <- source "foo"
        f2      <- map (+ 1) f
        f3      <- groups f2
        return f3

