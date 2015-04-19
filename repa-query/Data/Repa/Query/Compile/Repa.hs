{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- | Compilation of Repa queries to native code by 
--   emitting a Haskell program using repa-flow and compiling it with GHC.
module Data.Repa.Query.Compile.Repa
        ( decOfQuery
        , expOfQuery)
where
import Language.Haskell.TH                              as H
import Data.Repa.Flow.Auto.Format                       as F
import Data.Repa.Query.Graph                            as G
import qualified Data.Repa.Query.Format                 as Q
import qualified Data.Repa.Convert.Format               as C

import qualified Data.Repa.Query.Runtime.Primitive      as P
import qualified Data.Repa.Product                      as P


---------------------------------------------------------------------------------------------------
-- | Yield a top-level Haskell declararation for a query.
--
--   The query expression is bound to the given name.
decOfQuery   
        :: Name
        -> G.Query () String String String 
        -> Q H.Dec

decOfQuery nResult query
 = do   hexp    <- expOfQuery query
        return  $ H.ValD (H.VarP nResult) (H.NormalB hexp) []


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression for a query
expOfQuery   :: G.Query  () String String String -> Q H.Exp
expOfQuery (G.Query sResult delim fields (G.Graph nodes))
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
         = expOfRowFormat delim fields

        go []   
         = do   let hResult     = H.varE (H.mkName sResult)
                [| P.return $hResult |]

        go (n : ns)
         = do   (hPat, hRhs)    <- bindOfNode n
                [| $(return hRhs) P.>>= \ $(return hPat) -> $(go ns) |]


---------------------------------------------------------------------------------------------------
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
        G.SourceTable _ tableName delim@Q.Lines{} fields sOut
         -> do  let hTable       = return (LitE (StringL tableName))
                let Just format' = expOfRowFormat delim fields

                xRhs    <- [| P.fromFiles [ $hTable ] 
                                (P.sourceLinesFormat 
                                        (P.mul 64 1024)
                                        (P.error "query: line to long.")
                                        (P.error "query: cannot convert field.")
                                        $format') |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)


        G.SourceTable _ tableName delim@Q.LinesSep{} fields sOut
         -> do  let hTable       = return (LitE (StringL tableName))
                let Just format' = expOfRowFormat delim fields

                xRhs    <- [| P.fromFiles [ $hTable ] 
                                (P.sourceLinesFormat 
                                        (P.mul 64 1024)
                                        (P.error "query: line to long.")
                                        (P.error "query: cannot convert field.")
                                        $format') |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)


        G.SourceTable _ tableName delim@Q.Fixed{} fields sOut
         -> do  let hTable       = return (LitE (StringL tableName))
                let Just format' = expOfRowFormat delim fields

                xRhs    <- [| P.fromFiles [ $hTable ]
                                (P.sourceFixedFormat
                                        $format'
                                        (P.error "query: cannot convert field.")) |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)


-- | Yield a Haskell binding for a flow op.
bindOfFlowOp :: G.FlowOp () String String String -> Q (H.Pat, H.Exp)
bindOfFlowOp op
 = case op of
        G.FopMapI sIn sOut xFun
         -> do  let hIn         =  H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.map_i $(expOfExp xFun) $hIn |]
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
                hRhs            <- [| P.folds_i $(expOfExp xFun) $(expOfExp xNeu) 
                                                $hInLens         $hInElems |]
                return  (pOut, hRhs)


        G.FopGroupsI sIn sOut xFun
         -> do  let hIn         =  H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [|       P.map_i (\(g, n) -> g :*: n)
                                      P.=<< P.groupsBy_i $(expOfExp xFun) $hIn |]
                                      
                return  (pOut, hRhs)

        _ -> error $ "finish bindOfFlowOp: " ++ show op


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression from a query scalar expression.
expOfExp   :: G.Exp () String String -> H.ExpQ 
expOfExp xx
 = case xx of
        G.XVal _ (G.VLit _ lit)
         -> do  hl      <- litOfLit lit
                H.litE hl

        G.XVal _ (G.VLam _ sBind xBody)
         -> H.lamE [H.varP (H.mkName sBind)] (expOfExp xBody)

        G.XVar _ str
         -> H.varE (H.mkName str)

        G.XApp _ x1 x2
         -> H.appsE [expOfExp x1, expOfExp x2]

        G.XOp  _ sop xsArgs
         -> let Just hsop       = expOfScalarOp sop
            in  H.appsE (hsop : map expOfExp xsArgs)


-- | Yield a Haskell expression from a query scalar op.
expOfScalarOp :: ScalarOp -> Maybe H.ExpQ
expOfScalarOp sop
 = case sop of
        SopNeg          -> Just [| P.negate |]
        SopAdd          -> Just [| P.add    |]
        SopSub          -> Just [| P.sub    |]
        SopMul          -> Just [| P.mul    |]
        SopDiv          -> Just [| P.div    |]
        SopEq           -> Just [| P.eq     |]
        SopNeq          -> Just [| P.neq    |]
        SopLe           -> Just [| P.le     |]
        SopGt           -> Just [| P.gt     |]
        SopGe           -> Just [| P.ge     |]
        SopLt           -> Just [| P.lt     |]

        SopProj 2 1     -> Just [| (\(x :*: _)                   -> x) |]
        SopProj 2 2     -> Just [| (\(_ :*: x)                   -> x) |]

        SopProj 3 1     -> Just [| (\(x :*: _ :*: _)             -> x) |]
        SopProj 3 2     -> Just [| (\(_ :*: x :*: _)             -> x) |]
        SopProj 3 3     -> Just [| (\(_ :*: _ :*: x)             -> x) |]

        SopProj 4 1     -> Just [| (\(x :*: _ :*: _ :*: _)       -> x) |]
        SopProj 4 2     -> Just [| (\(_ :*: x :*: _ :*: _)       -> x) |]
        SopProj 4 3     -> Just [| (\(_ :*: _ :*: x :*: _)       -> x) |]
        SopProj 4 4     -> Just [| (\(_ :*: _ :*: _ :*: x)       -> x) |]

        SopProj 5 1     -> Just [| (\(x :*: _ :*: _ :*: _ :*: _) -> x) |]
        SopProj 5 2     -> Just [| (\(_ :*: x :*: _ :*: _ :*: _) -> x) |]
        SopProj 5 3     -> Just [| (\(_ :*: _ :*: x :*: _ :*: _) -> x) |]
        SopProj 5 4     -> Just [| (\(_ :*: _ :*: _ :*: x :*: _) -> x) |]
        SopProj 5 5     -> Just [| (\(_ :*: _ :*: _ :*: _ :*: x) -> x) |]

        _               -> Nothing


-- | Yield a Haskell literal from a query literal.
litOfLit :: G.Lit -> Q H.Lit
litOfLit lit 
 = case lit of
        G.LInt i        -> return $ H.IntegerL i
        G.LString s     -> return $ H.StringL  s
        _               -> error "fix float conversion"


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression for a row format.
expOfRowFormat :: Q.Delim -> [Q.FieldBox] -> Maybe H.ExpQ
expOfRowFormat delim fields
 = case (delim, fields) of
        (Q.Fixed, [f])
         |  Just f'     <- expOfFieldFormat f
         -> Just f'

        (Q.Fixed, (f:fs))
         |  Just ff'    <- expOfFieldFormats f fs
         -> Just [| C.App $ff' |]

        (Q.Lines, [f])
         |  Just f'     <- expOfFieldFormat f
         -> Just f'

        (Q.LinesSep _c, [f])
         |  Just f'     <- expOfFieldFormat f
         -> Just f'

        (Q.LinesSep c,  (f:fs))
         |  Just ff'    <- expOfFieldFormats f fs
         -> Just [| P.Sep $(H.litE (H.charL c)) $ff' |]

        _ -> Nothing


-- | Yield a Haskell expression for some fields.
expOfFieldFormats :: Q.FieldBox -> [Q.FieldBox] -> Maybe H.ExpQ 
expOfFieldFormats f1 []        
        = expOfFieldFormat f1

expOfFieldFormats f1 (f2 : fs) 
        | Just f1'   <- expOfFieldFormat  f1
        , Just ff'   <- expOfFieldFormats f2 fs
        = Just [| $f1' P.:*: $ff' |]

        | otherwise
        = Nothing


-- | Yield a Haskell expression for a field format.
expOfFieldFormat   :: Q.FieldBox -> Maybe H.ExpQ
expOfFieldFormat (Q.FieldBox field)
 = case field of
        Q.Word8be       -> Just [| P.Word8be   |]
        Q.Int8be        -> Just [| P.Int8be    |]

        Q.Word16be      -> Just [| P.Word16be  |]
        Q.Int16be       -> Just [| P.Int16be   |]

        Q.Word32be      -> Just [| P.Word32be  |]
        Q.Int32be       -> Just [| P.Int32be   |] 

        Q.Word64be      -> Just [| P.Word64be  |]
        Q.Int64be       -> Just [| P.Int64be   |]

        Q.Float32be     -> Just [| P.Float32be |]
        Q.Float64be     -> Just [| P.Float64be |]

        Q.YYYYsMMsDD c  -> Just [| P.YYYYsMMsDD $(H.litE (H.charL c)) |]
        Q.DDsMMsYYYY c  -> Just [| P.DDsMMsYYYY $(H.litE (H.charL c)) |]

        Q.IntAsc        -> Just [| P.IntAsc    |]
        Q.DoubleAsc     -> Just [| P.DoubleAsc |]

        Q.FixAsc len    -> Just [| P.FixAsc $(H.litE (H.integerL (fromIntegral len))) |]
        Q.VarAsc        -> Just [| P.VarAsc    |]

        _               -> Nothing

