{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
-- | Compilation of Repa queries to native code by 
--   emitting a Haskell program using repa-flow and compiling it with GHC.
module Data.Repa.Query.Build.Repa.Graph
        ( bindOfNode
        , bindOfSource
        , bindOfFlowOp)
where
import Control.Monad
import Data.Repa.Query.Build.Repa.Exp                   as R
import Data.Repa.Query.Graph                            as G
import Language.Haskell.TH                              as H
import qualified Data.Repa.Store.Format                 as Q
import qualified Data.Repa.Query.Runtime.Primitive      as P
import qualified Data.Repa.Scalar.Product               as P


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

{- TODO: Sep has renamed to mkSep


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
-}
          -- Fixed length rows.
          | otherwise
          -> error "repa-query: bindOfSource finish me"
{-
                do let hTable       = return (LitE (StringL path))
                let Just format' = expOfFieldsFormat (fields ++ [Q.FieldBox Q.Nil])

                xRhs    
                 <- [|  P.fromFiles [ $hTable ]
                          (P.sourceFixedFormat
                                $format'
                                (P.error "query: cannot convert field.")) |]

                pOut    <- H.varP (H.mkName sOut)
                return (pOut, xRhs)
-}

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
        G.SourceFamilyColumns _ 
                pathFamily pathColumns
                formatKey  formatColumns
                sOut
         -> do
                -- Directory holding column family.
                let hPathFamily     
                        = return (LitE (StringL pathFamily))

                -- Directories holding wanted columns.
                let hPathColumns    
                        = map (return . LitE . StringL) pathColumns

                -- Format of column family key.
                let Just hFormatKey 
                        = expOfFieldFormat formatKey 

                -- Formats of wanted columns.
                let Just hFormatColumns
                        = sequence $ map expOfFieldFormat $ formatColumns

                -- Source the key.
                xSourceFamilyKey
                        <- [| P.sourceFamilyKey
                                (P.mul 64 1024)
                                (P.error "query: line too long.")
                                (P.error "query: cannot convert field.")
                                ($hRootData P.</> $hPathFamily)
                                $hFormatKey |]

                -- Source the wanted columns.
                xSourceFamilyCols
                        <- mapM (\  (path, format)
                                 -> expOfSourceFamilyColumn hRootData path format)
                        $  zip hPathColumns hFormatColumns

                -- Zip together the family key and wanted columns.
                nKey      <- newName "k"
                let arity = length pathColumns
                nsCols    <- replicateM arity (newName "c")

                let Just xZipWith
                        = expOfZipWith
                                (expOfRowN (arity + 1))
                                (H.varE nKey : [H.varE nCol | nCol <- nsCols])

                xZipped <- expOfBindsExp 
                                (  (H.varP nKey, return xSourceFamilyKey)
                                : [(H.varP nCol, return xSourceFamilyCol)
                                        | nCol             <- nsCols
                                        | xSourceFamilyCol <- xSourceFamilyCols])
                        $  xZipWith
                          
                -- Final result.
                pOut    <- H.varP (H.mkName sOut)
                return  (pOut, xZipped)


expOfBindsExp 
        :: [(Q H.Pat, Q H.Exp)]
        -> Q H.Exp
        -> Q H.Exp

expOfBindsExp [] qBody
        = qBody

expOfBindsExp ((p, x) : more) qBody
 = do   hRest   <- expOfBindsExp more qBody
        [| $x P.>>= \ $p -> $(return hRest) |]


expOfSourceFamilyColumn 
        :: Q H.Exp      -- ^ Data root path.
        -> Q H.Exp      -- ^ Path to column.
        -> Q H.Exp      -- ^ Format of column
        -> Q H.Exp

expOfSourceFamilyColumn hPathRootData hPathColumn hFormatColumn
 =      [| P.sourceFamilyColumn
                (P.mul 64 1024)
                (P.error "query: line too long.")
                (P.error "query: cannot convert field.")
                ($hPathRootData P.</> $hPathColumn)
                $hFormatColumn |]


-- | Combine corresponding values in some flows with a function.
expOfZipWith :: Q H.Exp -> [Q H.Exp] -> Maybe (Q H.Exp)
expOfZipWith xF xArgs
 = case xArgs of
        [x1]
          -> Just [| P.map_i       $xF $x1 |]

        [x1, x2]
          -> Just [| P.zipWith_i   $xF $x1 $x2 |]

        [x1, x2, x3]
          -> Just [| P.zipWith3_i  $xF $x1 $x2 $x3 |]

        [x1, x2, x3, x4]        
          -> Just [| P.zipWith4_i  $xF $x1 $x2 $x3 $x4 |]

        [x1, x2, x3, x4, x5]
          -> Just [| P.zipWith5_i  $xF $x1 $x2 $x3 $x4 $x5 |]

        [x1, x2, x3, x4, x5, x6]
          -> Just [| P.zipWith6_i  $xF $x1 $x2 $x3 $x4 $x5 $x6 |]

        [x1, x2, x3, x4, x5, x6, x7]
          -> Just [| P.zipWith7_i  $xF $x1 $x2 $x3 $x4 $x5 $x6 $x7 |]

        _ -> Nothing


-- | Produce a functional expression that combines its arguments
--   into a row.
expOfRowN   :: Int -> Q H.Exp
expOfRowN n
 = do   vars      <- replicateM n (newName "x")

        let xBody = foldr (\hv hb -> [| $hv P.:*: $hb |]) [| P.Unit |] 
                  $ map H.varE vars

        let xExp  = foldr (\hv hb -> [| \ $hv -> $hb |])  xBody
                  $ map H.varP vars

        xExp


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell binding for a flow op.
-- 
--   TODO: generalise this to avoid cut-and-paste.
--   We've got the zipWith function builder now.
--
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


