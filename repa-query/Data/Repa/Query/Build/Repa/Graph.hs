{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- | Compilation of Repa queries to native code by 
--   emitting a Haskell program using repa-flow and compiling it with GHC.
module Data.Repa.Query.Build.Repa.Graph
        ( bindOfNode
        , bindOfSource
        , bindOfFlowOp)
where
import Data.Repa.Query.Build.Repa.Exp                   as R
import Data.Repa.Query.Graph                            as G
import Language.Haskell.TH                              as H
import qualified Data.Repa.Store.Format                 as Q
import qualified Data.Repa.Query.Runtime.Primitive      as P


-------------------------------------------------------------------
-- | Yield a Haskell binding for a flow node.
bindOfNode   :: G.Node   () String String String -> Q (H.Pat, H.Exp)
bindOfNode nn
 = case nn of
        G.NodeSource source     -> bindOfSource source
        G.NodeOp     op         -> bindOfFlowOp op


-- | Yield a Haskell binding for a flow op.
bindOfSource :: G.Source () String -> Q (H.Pat, H.Exp)
bindOfSource ss
 = let hRootData    = H.varE (mkName "_rootData")
   in case ss of

        ---------------------------------------------------
        G.SourceFile _ path delim fields sOut

         -- Variable length rows.
         | case delim of
                Q.Lines{}       -> True
                Q.LinesSep{}    -> True
                _               -> False
                
         -> do  let hTable       = return (LitE (StringL path))
                let Just format' = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])

                xRhs    
                 <- [| P.fromFiles [ $hRootData P.</> $hTable ] 
                        (P.sourceLinesFormat 
                                (P.mul 64 1024)
                                (P.error "query: line to long.")
                                (P.error "query: cannot convert field.")
                                $format') |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)

          -- Fixed length rows.
          | otherwise
          -> do let hTable       = return (LitE (StringL path))
                let Just format' = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])

                xRhs    
                 <- [|  P.fromFiles [ $hRootData P.</> $hTable ]
                          (P.sourceFixedFormat
                                $format'
                                (P.error "query: cannot convert field.")) |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)


        ---------------------------------------------------
        G.SourceTable _ path delim fields sOut
         -> do  let hPath        = return (LitE (StringL path))
                let Just hFormat = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])
                let hDelim       = expOfDelim delim

                xRhs    
                 <- [|  P.sourceTableFormat 
                                (P.mul 64 1024)
                                (P.error "query: line too long.")
                                (P.error "query: cannot convert field.")
                                ($hRootData P.</> $hPath)
                                $hDelim $hFormat |]

                pOut    <- H.varP (H.mkName sOut)
                return  (pOut, xRhs)


        ---------------------------------------------------
        G.SourceTableColumn  _ path delim fields col  sOut
         -> do  let hPath        = return (LitE (StringL path))
                let Just hFormat = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])
                let hDelim       = expOfDelim delim
                let Just hNat    = expOfNat (snd col)

                xRhs    
                 <- [|  P.select_i $hNat
                         P.=<< P.sourceTableFormat
                                (P.mul 64 1024)
                                (P.error "query: line too long.")
                                (P.error "query: cannot convert field.")
                                ($hRootData P.</> $hPath)
                                $hDelim $hFormat |]

                pOut    <- H.varP (H.mkName sOut)
                return  (pOut, xRhs)


        ---------------------------------------------------
        G.SourceTableColumns _ path delim fields cols sOut
         -> do  let hPath        = return (LitE (StringL path))
                let Just hFormat = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])
                let hDelim       = expOfDelim delim
                let hMask        = expOfMask (length fields) (map snd cols)

                xRhs    
                 <- [|  P.mask_i $hMask
                         P.=<< P.sourceTableFormat
                                (P.mul 64 1024)
                                (P.error "query: line too long.")
                                (P.error "query: cannot convert field.")
                                ($hRootData P.</> $hPath)
                                $hDelim $hFormat |]

                pOut    <- H.varP (H.mkName sOut)
                return  (pOut, xRhs)


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
                hRhs            <- [| P.groupsBy_i $(expOfExp xFun) $hIn |]
                                      
                return  (pOut, hRhs)

        _ -> error $ "repa-query: TODO bindOfFlowOp code gen for " ++ show op


