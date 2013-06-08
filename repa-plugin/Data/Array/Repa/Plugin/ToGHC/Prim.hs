
module Data.Array.Repa.Plugin.ToGHC.Prim
        ( convertPrim
        , convertPolytypicPrim
        , isPolytypicPrimName)
where
import Data.Array.Repa.Plugin.Primitives
import Data.Array.Repa.Plugin.ToGHC.Type

import qualified HscTypes                as G
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified UniqSupply              as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Prim      as D
import qualified DDC.Core.Flow.Compounds as D


-- | Convert a primop that has the same definition independent 
--   of its type arguments.
convertPrim 
        :: Env -> Env
        -> D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPrim _kenv tenv n 
 = let prims    = envPrimitives tenv
   in case n of
        D.NameOpFlow D.OpFlowRateOfSeries
         -> return $ prim_rateOfSeries prims

        D.NameOpLoop D.OpLoopGuard
         -> return $ prim_guard prims

        -- ERROR: This isn't a primtive name,
        --        or we don't have an implementation for it.
        _ -> errorMissingPrim (envGuts tenv) n Nothing


-------------------------------------------------------------------------------
-- | Convert a primop that has a different definition depending on the type
--   argument. If primops handled by this function must be detected by
--   `isPolyTypicPrimName` below.
convertPolytypicPrim 
        :: Env -> Env
        -> D.Name -> [D.Type D.Name]
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPolytypicPrim kenv _tenv n tsArg
 = let prims    = envPrimitives kenv
   in case n of

        -- Arith
        D.NamePrimArith D.PrimArithAdd
         |  tsArg == [D.tNat]                   -> return $ prim_addInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_addInt prims

        D.NamePrimArith D.PrimArithSub
         |  tsArg == [D.tNat]                   -> return $ prim_subInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_subInt prims

        D.NamePrimArith D.PrimArithMul
         |  tsArg == [D.tNat]                   -> return $ prim_mulInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_mulInt prims

        D.NamePrimArith D.PrimArithDiv
         |  tsArg == [D.tNat]                   -> return $ prim_divInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_divInt prims

        D.NamePrimArith D.PrimArithMod
         |  tsArg == [D.tNat]                   -> return $ prim_modInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_modInt prims

        D.NamePrimArith D.PrimArithRem
         |  tsArg == [D.tNat]                   -> return $ prim_remInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_remInt prims

        -- Eq
        D.NamePrimArith D.PrimArithEq
         | tsArg == [D.tNat]                    -> return $ prim_eqInt prims
         | tsArg == [D.tInt]                    -> return $ prim_eqInt prims

        D.NamePrimArith D.PrimArithNeq
         | tsArg == [D.tNat]                    -> return $ prim_neqInt prims
         | tsArg == [D.tInt]                    -> return $ prim_neqInt prims

        D.NamePrimArith D.PrimArithGt
         | tsArg == [D.tNat]                    -> return $ prim_gtInt prims
         | tsArg == [D.tInt]                    -> return $ prim_gtInt prims

        D.NamePrimArith D.PrimArithGe
         | tsArg == [D.tNat]                    -> return $ prim_geInt prims
         | tsArg == [D.tInt]                    -> return $ prim_geInt prims

        D.NamePrimArith D.PrimArithLt
         | tsArg == [D.tNat]                    -> return $ prim_ltInt prims
         | tsArg == [D.tInt]                    -> return $ prim_ltInt prims

        D.NamePrimArith D.PrimArithLe
         | tsArg == [D.tNat]                    -> return $ prim_leInt prims
         | tsArg == [D.tInt]                    -> return $ prim_leInt prims


        -- Ref
        D.NameOpStore D.OpStoreNew
         |  tsArg == [D.tNat]                   -> return $ prim_newRefInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_newRefInt prims
         |  tsArg == [D.tTuple2 D.tInt D.tInt]  -> return $ prim_newRefInt_T2 prims

        D.NameOpStore D.OpStoreRead
         |  tsArg == [D.tNat]                   -> return $ prim_readRefInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_readRefInt prims
         |  tsArg == [D.tTuple2 D.tInt D.tInt]  -> return $ prim_readRefInt_T2 prims

        D.NameOpStore D.OpStoreWrite
         |  tsArg == [D.tNat]                   -> return $ prim_writeRefInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_writeRefInt prims
         |  tsArg == [D.tTuple2 D.tInt D.tInt]  -> return $ prim_writeRefInt_T2 prims

        -- Vector
        D.NameOpStore D.OpStoreNewVector
         |  tsArg == [D.tNat]                   -> return $ prim_newVectorInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_newVectorInt prims

        D.NameOpStore D.OpStoreNewVectorN
         |  [tA, _tK] <- tsArg, tA == D.tNat    -> return $ prim_newVectorInt   prims
         |  [tA, _tK] <- tsArg, tA == D.tInt    -> return $ prim_newVectorInt   prims

        D.NameOpStore D.OpStoreReadVector
         |  tsArg == [D.tNat]                   -> return $ prim_readVectorInt  prims
         |  tsArg == [D.tInt]                   -> return $ prim_readVectorInt  prims

        D.NameOpStore D.OpStoreWriteVector
         |  tsArg == [D.tNat]                   -> return $ prim_writeVectorInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_writeVectorInt prims

        D.NameOpStore D.OpStoreSliceVector
         |  tsArg == [D.tNat]                   -> return $ prim_sliceVectorInt prims
         |  tsArg == [D.tInt]                   -> return $ prim_sliceVectorInt prims

        -- Next
        D.NameOpStore D.OpStoreNext
         |  [tA, tK] <- tsArg, tA == D.tInt || tA == D.tNat
         -> do  tK'             <- convertType kenv tK
                let (x, t)      = prim_nextInt prims
                return  ( G.App x (G.Type tK')
                        , G.applyTy t tK' )

        D.NameOpStore D.OpStoreNext
         |  [tA, tK] <- tsArg, tA == D.tTuple2 D.tInt D.tInt
         -> do  tK'             <- convertType kenv tK
                let (x, t)      = prim_nextInt_T2 prims
                return  ( G.App x (G.Type tK')
                        , G.applyTy t tK' )



        -- Loop
        D.NameOpLoop D.OpLoopLoopN
         -> return $ prim_loop prims


        -- ERROR: This isn't a primitive name,
        --        or we don't have an implementation for it,
        --        or the function `isPolytypicPrimName` tells lies.
        _  -> errorMissingPrim (envGuts kenv) n Nothing


-- | Check whether the function with this name must be handled polytypically. 
--   This needs to match all the names handled by `convertPolytypicPrim` above.
isPolytypicPrimName :: D.Name -> Bool
isPolytypicPrimName n
 = elem n       
        [ D.NamePrimArith       D.PrimArithAdd
        , D.NamePrimArith       D.PrimArithSub
        , D.NamePrimArith       D.PrimArithMul
        , D.NamePrimArith       D.PrimArithDiv
        , D.NamePrimArith       D.PrimArithMod
        , D.NamePrimArith       D.PrimArithRem

        , D.NamePrimArith       D.PrimArithEq
        , D.NamePrimArith       D.PrimArithNeq
        , D.NamePrimArith       D.PrimArithGt
        , D.NamePrimArith       D.PrimArithGe
        , D.NamePrimArith       D.PrimArithLt
        , D.NamePrimArith       D.PrimArithLe

        , D.NameOpStore         D.OpStoreNew
        , D.NameOpStore         D.OpStoreRead
        , D.NameOpStore         D.OpStoreWrite
        , D.NameOpStore         D.OpStoreNewVector
        , D.NameOpStore         D.OpStoreNewVectorN
        , D.NameOpStore         D.OpStoreReadVector
        , D.NameOpStore         D.OpStoreWriteVector 
        , D.NameOpStore         D.OpStoreSliceVector 
        , D.NameOpStore         D.OpStoreNext

        , D.NameOpLoop          D.OpLoopLoopN ]
        

-- | Complain that we couldn't find a primitive that we needed.
errorMissingPrim :: G.ModGuts -> D.Name -> Maybe String -> a
errorMissingPrim _guts _n (Just str)
 = error $ unlines
 $ map ("        " ++)
        [ ""
        , "repa-plugin:"
        , " Cannot find definition for primitive '" ++ str ++ "'"
        , ""
        , " When using the repa-plugin you must import a module that provides"
        , " implementations for the primitives used by the lowering transform."
        , ""
        , " This problem is likely caused by importing just the repa-series"
        , " module that contains the stream operators, but not the module that"
        , " contains the target primitives as well."
        , ""
        , " If you don't want to define your own primitives then try adding"
        , "  'import Data.Array.Repa.Series' to your client module."
        , ""
        , " This is a problem with the Repa plugin, and not GHC proper."
        , " You can ignore the following request to report this as a GHC bug." 
        , "" ]


errorMissingPrim _guts n Nothing
 = error $ unlines
 $ map ("        " ++)
        [ ""
        , "repa-plugin:"
        , " No Haskell symbol name for Disciple Core Flow primitive:"
        , "  '" ++ show n ++ "'"
        , ""
        , " Please report this problem on the Repa bug tracker,"
        , "   or complain about it on the Repa mailing list."
        , ""
        , " This is a problem with the Repa plugin, and not GHC proper."
        , " You can ignore the following request to report this as a GHC bug." ]

