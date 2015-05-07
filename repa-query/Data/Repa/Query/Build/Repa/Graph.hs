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
import qualified Data.Repa.Product                      as P

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
         | Q.LinesSep c <- delim                
         -> do  let hTable       = return (LitE (StringL path))
                let Just format' = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])
                let c'           = return (LitE (CharL c))

                xRhs    
                 <- [| P.fromFiles [ $hTable ] 
                        (P.sourceLinesFormat 
                                (P.mul 64 1024)
                                (P.error "query: line to long.")
                                (P.error "query: cannot convert field.")
                                (P.Sep $c' $format')) |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)

         -- Variable length rows.
         | Q.Lines      <- delim
         -> do  let hTable       = return (LitE (StringL path))
                let Just format' = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])

                xRhs    
                 <- [| P.fromFiles [ $hTable ] 
                        (P.sourceLinesFormat 
                                (P.mul 64 1024)
                                (P.error "query: line to long.")
                                (P.error "query: cannot convert field.")
                                (P.Sep '\t' $format')) |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)

          -- Fixed length rows.
          | otherwise
          -> do let hTable       = return (LitE (StringL path))
                let Just format' = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])

                xRhs    
                 <- [|  P.fromFiles [ $hTable ]
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


        ---------------------------------------------------
        G.SourceFamilyColumn  _ pathFamily pathColumn formatKey formatColumn sOut
         -> do  let hPathFamily  = return (LitE (StringL pathFamily))
                let hPathColumn  = return (LitE (StringL pathColumn))
                let Just hFormatKey     = expOfFieldFormat formatKey 
                let Just hFormatColumn  = expOfFieldsFormat ([formatColumn] ++ [Q.FieldBox Q.Nil])

                xRhs    
                 <- [|  do sk <- P.sourceFamilyKey
                                        (P.mul 64 1024)
                                        (P.error "query: line too long.")
                                        (P.error "query: cannot convert field.")
                                        ($hRootData P.</> $hPathFamily)
                                        $hFormatKey

                           sc <- P.sourceFamilyColumn
                                        (P.mul 64 1024)
                                        (P.error "query: line too long.")
                                        (P.error "query: cannot convert field.")
                                        ($hRootData P.</> $hPathColumn)
                                        (P.Sep '\t' $hFormatColumn) 

                           P.zipWith_i (\k c -> k P.:*: c P.:*: P.Unit) sk sc

                     |]

                pOut    <- H.varP (H.mkName sOut)
                return  (pOut, xRhs)


-- | Yield a Haskell binding for a flow op.
bindOfFlowOp :: G.FlowOp () String String String -> Q (H.Pat, H.Exp)
bindOfFlowOp op
 = case op of
        G.FopMapI [sIn] sOut xFun
         -> do  let hIn         =  H.varE (H.mkName sIn)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.map_i $(expOfExp xFun) $hIn |]
                return  (pOut, hRhs)

        -- TODO: this shold 
        G.FopMapI [sIn1, sIn2] sOut xFun
         -> do  let hIn1        =  H.varE (H.mkName sIn1)
                let hIn2        =  H.varE (H.mkName sIn2)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.zipWith_i $(expOfExp xFun) 
                                        $hIn1 $hIn2 |]
                return  (pOut, hRhs)

        G.FopMapI [sIn1, sIn2, sIn3] sOut xFun
         -> do  let hIn1        =  H.varE (H.mkName sIn1)
                let hIn2        =  H.varE (H.mkName sIn2)
                let hIn3        =  H.varE (H.mkName sIn3)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.zipWith3_i $(expOfExp xFun) 
                                        $hIn1 $hIn2 $hIn3 |]
                return  (pOut, hRhs)

        G.FopMapI [sIn1, sIn2, sIn3, sIn4] sOut xFun
         -> do  let hIn1        =  H.varE (H.mkName sIn1)
                let hIn2        =  H.varE (H.mkName sIn2)
                let hIn3        =  H.varE (H.mkName sIn3)
                let hIn4        =  H.varE (H.mkName sIn4)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.zipWith4_i $(expOfExp xFun) 
                                        $hIn1 $hIn2 $hIn3 $hIn4 |]
                return  (pOut, hRhs)

        G.FopMapI [sIn1, sIn2, sIn3, sIn4, sIn5] sOut xFun
         -> do  let hIn1        =  H.varE (H.mkName sIn1)
                let hIn2        =  H.varE (H.mkName sIn2)
                let hIn3        =  H.varE (H.mkName sIn3)
                let hIn4        =  H.varE (H.mkName sIn4)
                let hIn5        =  H.varE (H.mkName sIn5)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.zipWith5_i $(expOfExp xFun) 
                                        $hIn1 $hIn2 $hIn3 $hIn4 $hIn5 |]
                return  (pOut, hRhs)

        G.FopMapI [sIn1, sIn2, sIn3, sIn4, sIn5, sIn6] sOut xFun
         -> do  let hIn1        =  H.varE (H.mkName sIn1)
                let hIn2        =  H.varE (H.mkName sIn2)
                let hIn3        =  H.varE (H.mkName sIn3)
                let hIn4        =  H.varE (H.mkName sIn4)
                let hIn5        =  H.varE (H.mkName sIn5)
                let hIn6        =  H.varE (H.mkName sIn6)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.zipWith6_i $(expOfExp xFun) 
                                        $hIn1 $hIn2 $hIn3 $hIn4 $hIn5 $hIn6 |]
                return  (pOut, hRhs)

        G.FopMapI [sIn1, sIn2, sIn3, sIn4, sIn5, sIn6, sIn7] sOut xFun
         -> do  let hIn1        =  H.varE (H.mkName sIn1)
                let hIn2        =  H.varE (H.mkName sIn2)
                let hIn3        =  H.varE (H.mkName sIn3)
                let hIn4        =  H.varE (H.mkName sIn4)
                let hIn5        =  H.varE (H.mkName sIn5)
                let hIn6        =  H.varE (H.mkName sIn6)
                let hIn7        =  H.varE (H.mkName sIn7)
                pOut            <- H.varP (H.mkName sOut)
                hRhs            <- [| P.zipWith7_i $(expOfExp xFun) 
                                        $hIn1 $hIn2 $hIn3 $hIn4 $hIn5 $hIn6 $hIn7 |]
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


