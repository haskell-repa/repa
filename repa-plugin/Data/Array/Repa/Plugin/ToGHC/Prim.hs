
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

import qualified Data.Map                as Map
import Data.Maybe

-- | Convert a primop that has the same definition independent 
--   of its type arguments.
convertPrim 
        :: Env -> Env
        -> D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPrim _kenv tenv n 
 = let prims    = envPrimitives tenv
   in case n of
        D.NameOpSeries D.OpSeriesRateOfSeries
         -> return $ prim_rateOfSeries prims

        D.NameOpControl D.OpControlGuard
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
 = let  prims    = envPrimitives kenv

        getPrim nn t 
         | t == D.tInt      = let Just r = Map.lookup nn (prim_baseInt    prims) in r
         | t == D.tNat      = let Just r = Map.lookup nn (prim_baseWord   prims) in r
         | t == D.tFloat 32 = let Just r = Map.lookup nn (prim_baseFloat  prims) in r
         | t == D.tFloat 64 = let Just r = Map.lookup nn (prim_baseDouble prims) in r
         | otherwise        = error "repa-plugin.convertPolytypicPrim failed"

   in case n of
        -- Loop Combinators
        D.NameOpControl D.OpControlLoopN
         -> return $ prim_loop prims

        -- Arithmetic Primops
        D.NamePrimArith _
         |  [t]   <- tsArg       
         -> return $ getPrim n t


        -- Store Primops
        D.NameOpSeries (D.OpSeriesNext 1)
         |  [tA, tK] <- tsArg, tA == D.tTuple2 D.tInt D.tInt
         -> do  tK'     <- convertType kenv tK
                let (x, t)      = prim_nextInt_T2 prims
                return  ( G.App x (G.Type tK')
                        , G.applyTy t tK' )

        D.NameOpSeries (D.OpSeriesNext 1)
         |  [tA, tK] <- tsArg
         -> do  let (x, t) = getPrim n tA
                tK'        <- convertType kenv tK
                return  ( G.App x (G.Type tK')
                        , G.applyTy t tK' )

        D.NameOpStore _
         | t : _ <- tsArg
         -> return $ getPrim n t

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

        , D.NameOpSeries        (D.OpSeriesNext 1)

        , D.NameOpControl       D.OpControlLoopN

        , D.NameOpStore         D.OpStoreNew
        , D.NameOpStore         D.OpStoreRead
        , D.NameOpStore         D.OpStoreWrite
        , D.NameOpStore         D.OpStoreNewVector
        , D.NameOpStore         D.OpStoreNewVectorN
        , D.NameOpStore         (D.OpStoreReadVector  1)
        , D.NameOpStore         (D.OpStoreWriteVector 1)
        , D.NameOpStore         D.OpStoreSliceVector ]

        

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

