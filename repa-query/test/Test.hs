{-# LANGUAGE ScopedTypeVariables #-}

import Data.Repa.Query.Compile.Repa             as Q
import Data.Repa.Query.Convert.JSON             as Q
import Data.Repa.Query.Source.EDSL              as Q
import Data.Repa.Query.Source.Builder           as Q
import Data.Repa.Query.Format
import qualified Data.Repa.Query.Graph          as G

import Data.Aeson                               (encode, toJSON, fromJSON, Result)
import qualified Data.ByteString.Lazy.Char8     as BS
import qualified Prelude
import Prelude (($), Int, return, String)
import Data.Repa.Query.Build


result  
 = query (LinesSep '\t') (IntAsc :*: IntAsc)
 $ do   f       <- source "foo.txt" Lines IntAsc
        f2      <- map (+ 1) f
        f3      <- groups f2
        return f3

