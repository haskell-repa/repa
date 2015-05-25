
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
import qualified Data.Repa.Query.Runtime.Driver         as P
import Data.Repa.Scalar.Product


---------------------------------------------------------------------------------------------------
-- | Yield a top-level Haskell declararation for a job
--
--   The result value is bound to the given name.
decOfJob
        :: Name
        -> Job
        -> Q H.Dec

decOfJob nResult job
 = do   hExp    <- [| $(expOfJob job) |]
        return  $  H.ValD (H.VarP nResult) (H.NormalB hExp) [] 


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression for a job.
--
--   These all produce a value of type IO ().
--   Run the action to invoke the job.
--
expOfJob   :: Job -> Q H.Exp

expOfJob (JobQuery   query format)
 =      [| P.execQuery   $(expOfBulkQueryFormat query format) |]

expOfJob (JobExtract query format target)
 =      [| P.execExtract $(expOfBulkQueryFormat query format) 
                         $(expOfExtractTarget target) |]

expOfJob (JobSieve   query format target)
 =      [| P.execSieve   $(expOfKeyQueryFormat  query format)
                         $(expOfSieveTarget   target) |]


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression that produces a flow for the 
--   given query and output format.
--
expOfBulkQueryFormat 
        :: G.QueryS -> G.OutputFormat
        -> Q H.Exp 

expOfBulkQueryFormat (G.Query sResult (G.Graph nodes)) format 
 = case format of
    G.OutputFormatFixed delim fields
     -> let Just format' = expOfDelimFields delim fields
        in case delim of
            Q.Fixed{}    -> [| \ $hRootData -> $sources P.>>= P.packFormat_i   $format' |]
            Q.Lines{}    -> [| \ $hRootData -> $sources P.>>= P.packFormatLn_i $format' |]
            Q.LinesSep{} -> [| \ $hRootData -> $sources P.>>= P.packFormatLn_i $format' |]

    G.OutputFormatAsciiBuildTime 
     -> [| \ $hRootData -> $sources P.>>= P.packAsciiLn_i |]

 where 
        hRootData = H.varP (mkName "_rootData")
        sources   = go nodes
        
        go []   
         = do   let hResult     = H.varE (H.mkName sResult)
                [| P.return $hResult |]

        go (n : ns)
         = do   (hPat, hRhs)    <- bindOfNode n
                [| $(return hRhs) P.>>= \ $(return hPat) -> $(go ns) |]


---------------------------------------------------------------------------------------------------
expOfKeyQueryFormat
        :: G.QueryS -> G.OutputFormat
        -> Q H.Exp

expOfKeyQueryFormat (G.Query sResult (G.Graph nodes)) format
 = case format of
    G.OutputFormatAsciiBuildTime
     -> [| \ $hRootData -> $sources 
                P.>>= P.map_i (\(k :*: x) -> (k, x))
                P.>>= P.keyPackAsciiLn_i |]
  
    _ -> error "repa-query.expOfKeyQueryFormat: not finished"

 where
        hRootData = H.varP (mkName "_rootData")
        sources   = go nodes
        
        go []   
         = do   let hResult     = H.varE (H.mkName sResult)
                [| P.return $hResult |]

        go (n : ns)
         = do   (hPat, hRhs)    <- bindOfNode n
                [| $(return hRhs) P.>>= \ $(return hPat) -> $(go ns) |]


---------------------------------------------------------------------------------------------------
expOfExtractTarget :: G.ExtractTarget -> Q H.Exp
expOfExtractTarget target
 = case target of
        G.ExtractTargetFile file 
         -> let hFile   = H.LitE (H.StringL file)
            in [| P.ExtractTargetFile $(return hFile) |]


expOfSieveTarget :: G.SieveTarget -> Q H.Exp
expOfSieveTarget target
 = case target of
        G.SieveTargetDir file 
         -> let hFile   = H.LitE (H.StringL file)
            in [| P.SieveTargetDir $(return hFile) |]

