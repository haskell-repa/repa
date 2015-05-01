
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- | Compilation of Repa queries to native code by 
--   emitting a Haskell program using repa-flow and compiling it with GHC.
module Data.Repa.Query.Build.Repa.Job
        ( decOfJob
        , expOfJob)
where
import Data.Repa.Query.Build.Repa.Exp                   as R
import Data.Repa.Query.Build.Repa.Graph                 as R
import Data.Repa.Query.Graph                            as G
import Data.Repa.Query.Job.Spec                         as G
import Language.Haskell.TH                              as H
import qualified Data.Repa.Store.Format                 as Q
import qualified Data.Repa.Query.Runtime.Primitive      as P


---------------------------------------------------------------------------------------------------
-- | Yield a top-level Haskell declararation for a job
--
--   The result value is bound to the given name.
decOfJob
        :: Name
        -> Job
        -> Q H.Dec

decOfJob nResult job
 = do   let hRootData = H.varP (mkName ("_rootData"))
        hExp    <- [| \ $hRootData -> $(expOfJob job) |]

        return  $  H.ValD (H.VarP nResult) (H.NormalB hExp) [] 


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression for a job
expOfJob   :: Job -> Q H.Exp

expOfJob (JobQuery (G.Query sResult (G.Graph nodes)) outputFormat)
 = case outputFormat of
    G.OutputFormatFixed delim fields
     -> let Just format'
             = expOfDelimFields delim fields

        in case delim of
            Q.Fixed{}
             -> [| $sources P.>>= P.concatPackFormat_i  $format' |]

            Q.Lines{}
             -> [| $sources P.>>= P.unlinesPackFormat_i $format' |]

            Q.LinesSep{}
             -> [| $sources P.>>= P.unlinesPackFormat_i $format' |]

    G.OutputFormatAsciiBuildTime 
     ->         [| $sources P.>>= P.unlinesPackAscii_i |]

 where  sources 
         = go nodes
        
        go []   
         = do   let hResult     = H.varE (H.mkName sResult)
                [| P.return $hResult |]

        go (n : ns)
         = do   (hPat, hRhs)    <- bindOfNode n
                [| $(return hRhs) P.>>= \ $(return hPat) -> $(go ns) |]

expOfJob _
 = error "expOfJob: finish me"
