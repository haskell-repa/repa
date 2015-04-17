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


-----------------------------------------------------------
-- User query
qb1 :: Q (Flow (Int, Int))
qb1
 = do   (f :: Flow Int) <- source "foo" (Lines '\t' [DoubleAsc])
        f2      <- map (+ 1) f
        f3      <- groups f2
        return f3


-----------------------------------------------------------
q1  :: G.Query () String String String
q1 = buildQ (Lines '\t' [DoubleAsc]) qb1

