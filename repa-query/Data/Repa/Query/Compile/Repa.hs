{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- | Convert a query graph to Haskell code using Repa-flow as the back end.
--   We can then compile the result with GHC to get an executable.
module Data.Repa.Query.Compile.Repa
        (bindOfNode)
where
import Language.Haskell.TH              as H
import Language.Haskell.TH.Quote        as H
import Data.Repa.Flow                   as F
import Data.Repa.Query.Graph            as G


-- | Yield a Haskell expression for a query
expOfQuery   :: G.Query  () String String String -> Q H.Exp
expOfQuery (G.Query sResult (G.Graph nodes))
 = go nodes
 where  go []   
         =      H.varE (H.mkName sResult)

        go (n : ns)
         = do   (hPat, hRhs)    <- bindOfNode n
                [| $(return hRhs) >>= \ $(return hPat) -> $(go ns) |]


-- | Yield a Haskell binding for a flow node.
bindOfNode   :: G.Node   () String String String -> Q (H.Pat, H.Exp)
bindOfNode nn
 = case nn of
        G.NodeSource source     -> bindOfSource source
        G.NodeOp     op         -> bindOfFlowOp op


-- | Yield a Haskell binding for a flow op.
bindOfSource :: G.Source () String -> Q (H.Pat, H.Exp)
bindOfSource ss
 = case ss of
        G.SourceTable _ tableName sOut
         -> do  let hTable      =  return (LitE (StringL tableName))
                xRhs            <- [| fromFiles [ $hTable ] sourceLines |]
                pOut            <- H.varP (H.mkName sOut)
                return (pOut, xRhs)


-- | Yield a Haskell binding for a flow op.
bindOfFlowOp :: G.FlowOp () String String String -> Q (H.Pat, H.Exp)

bindOfFlowOp op
 = case op of
        G.FopMapI sIn sOut xFun
         -> do  let hIn         =  H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| F.map_i $(expOfExp xFun) $hIn |]
                return  (pOut, hRhs)

{-      TODO: add filter to repa-flow API
        G.FopFilterI sIn sOut xFun
         -> do  let hIn         = H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                let hFun        = expOfExp xFun
                hRhs            <- [| F.filter_i $hFun $hIn |]
                return  (pOut, hRhs)


        TODO: add fold to single elem to repa-flow API
        G.FopFoldI  sIn sOut xFun xNeu
         -> do  let hInElems    =  H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                let hFun        = expOfExp xFun
                let hNeu        = expOfExp xNeu
                hRhs            <- [| F.folds_i $hFun $hNeu $hInLens $hInElems |]
                return  (pOut, hRhs)
-}

        G.FopFoldsI sInLens sInElems sOut xFun xNeu
         -> do  let hInLens     =  H.varE (H.mkName sInLens)
                let hInElems    =  H.varE (H.mkName sInElems)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| F.folds_i $(expOfExp xFun) $(expOfExp xNeu) 
                                                $hInLens         $hInElems |]
                return  (pOut, hRhs)


        G.FopGroupsI sIn sOut xFun
         -> do  let hIn         =  H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| F.groupsBy_i $(expOfExp xFun) $hIn |]
                return  (pOut, hRhs)


-- | Yield a Haskell expression from a query scalar expression.
expOfExp   :: G.Exp () String String -> H.ExpQ 
expOfExp xx
 = case xx of
        G.XLit _ lit    
         -> do  hl      <- litOfLit lit
                H.litE hl

        G.XVar _ str
         -> H.varE (H.mkName str)

        G.XLam _ sBind xBody
         -> H.lamE [H.varP (H.mkName sBind)] (expOfExp xBody)

        G.XOp  _ sop xsArgs
         -> H.appsE (expOfScalarOp sop : map expOfExp xsArgs)


-- | Yield a Haskell expression from a query scalar op.
expOfScalarOp :: ScalarOp -> H.ExpQ
expOfScalarOp sop
 = case sop of
        SopNeg  -> [| negate |]
        SopAdd  -> [| (+)  |]
        SopSub  -> [| (-)  |]
        SopMul  -> [| (*)  |]
        SopDiv  -> [| (/)  |]
        SopEq   -> [| (==) |]
        SopNeq  -> [| (<=) |]
        SopGt   -> [| (>)  |]
        SopGe   -> [| (>=) |]
        SopLt   -> [| (<)  |]
        SopLe   -> [| (<=) |]


-- | Yield a Haskell literal from a query literal.
litOfLit :: G.Lit -> Q H.Lit
litOfLit lit 
 = case lit of
        G.LitInt i      -> return $ H.IntegerL i
        G.LitString s   -> return $ H.StringL  s
        _               -> error "fix float conversion"


