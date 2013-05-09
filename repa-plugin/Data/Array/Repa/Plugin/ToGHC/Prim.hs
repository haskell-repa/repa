
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
        D.NamePrimArith D.PrimArithAdd
         |  tsArg == [D.tInt]
         -> return $ prim_addInt         prims

        D.NamePrimArith D.PrimArithMul
         |  tsArg == [D.tInt]
         -> return $ prim_mulInt         prims

        D.NameOpStore D.OpStoreNewVector
         |  tsArg == [D.tInt]       
         -> return $ prim_newIntVector   prims

        D.NameOpStore D.OpStoreNewVectorN
         |  [tA, _tK] <- tsArg, tA == D.tInt
         -> return $ prim_newIntVector   prims

        D.NameOpStore D.OpStoreReadVector
         |  tsArg == [D.tInt]       
         -> return $ prim_readIntVector  prims

        D.NameOpStore D.OpStoreWriteVector
         |  tsArg == [D.tInt]       
         -> return $ prim_writeIntVector prims

        D.NameOpStore D.OpStoreNext
         |  [tA, tK] <- tsArg, tA == D.tInt
         -> do  tK'             <- convertType kenv tK
                let (x, t)      = prim_nextInt prims
                return  ( G.App x (G.Type tK')
                        , G.applyTy t tK' )


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
        , D.NamePrimArith       D.PrimArithMul
        , D.NameOpStore         D.OpStoreNewVector
        , D.NameOpStore         D.OpStoreNewVectorN
        , D.NameOpStore         D.OpStoreReadVector
        , D.NameOpStore         D.OpStoreWriteVector 
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

