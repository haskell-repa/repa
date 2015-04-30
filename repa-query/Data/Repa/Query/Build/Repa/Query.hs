
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- | Compilation of Repa queries to native code by 
--   emitting a Haskell program using repa-flow and compiling it with GHC.
module Data.Repa.Query.Build.Repa.Query
        ( decOfQuery
        , expOfQuery)
where
import Data.Repa.Query.Build.Repa.Exp                   as R
import Data.Repa.Query.Build.Repa.Graph                 as R
import Data.Repa.Query.Graph                            as G
import Language.Haskell.TH                              as H
import qualified Data.Repa.Store.Format                 as Q
import qualified Data.Repa.Query.Runtime.Primitive      as P


---------------------------------------------------------------------------------------------------
-- | Yield a top-level Haskell declararation for a query.
--
--   The query expression is bound to the given name.
decOfQuery   
        :: Name
        -> G.Query () String String String 
        -> Q H.Dec

decOfQuery nResult query
 = do   let hRootData = H.varP (mkName ("_rootData"))

        hExp    <- [| \ $hRootData -> $(expOfQuery query) |]

        return  $  H.ValD (H.VarP nResult) (H.NormalB hExp) [] 


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression for a query
expOfQuery   :: G.Query  () String String String -> Q H.Exp
expOfQuery (G.Query (G.OutputFormatFixed delim fields) 
                    sResult (G.Graph nodes))
 = case delim of
        Q.Fixed{}
         -> [| $sources P.>>= P.concatPackFormat_i  $format' |]

        Q.Lines{}
         -> [| $sources P.>>= P.unlinesPackFormat_i $format' |]

        Q.LinesSep{}
         -> [| $sources P.>>= P.unlinesPackFormat_i $format' |]

 where  sources 
         = go nodes

        Just format'
         = expOfDelimFields delim fields

        go []   
         = do   let hResult     = H.varE (H.mkName sResult)
                [| P.return $hResult |]

        go (n : ns)
         = do   (hPat, hRhs)    <- bindOfNode n
                [| $(return hRhs) P.>>= \ $(return hPat) -> $(go ns) |]


