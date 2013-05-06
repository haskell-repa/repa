
module Data.Array.Repa.Plugin.ToGHC.Prim
        ( convertPrim
        , convertPolytypicPrim
        , isPolytypicPrimName)
where
import Data.Array.Repa.Plugin.ToGHC.Prim.Table
import Data.Array.Repa.Plugin.ToGHC.Type
import Data.List

import qualified HscTypes                as G
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TysPrim                 as G
import qualified Var                     as G
import qualified UniqSupply              as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Prim      as D


-- | Convert a primop that has the same definition independent 
--   of its type arguments.
convertPrim 
        :: Env -> Env
        -> D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPrim _kenv tenv n 
        | Just gv       <- findPrimitive (envImported tenv) n []
        = return (G.Var gv, G.varType gv)

        -- ERROR: Primitive is in our prim table, but the Haskell client
        --        module hasn't imported an implementation of it.
        |  Just prim   <- find (\p -> primName p == n) primitives
        = errorMissingPrim (envGuts tenv) n (Just $ primSymbol prim)

        -- ERROR: Primitive is not even in the prim table,
        --        this is definately a bug in the Repa plugin.
        | otherwise
        = errorMissingPrim (envGuts tenv) n Nothing


-------------------------------------------------------------------------------
-- | Convert a primop that has a different definition depending on the type
--   argument. If primops handled by this function must be detected by
--   `isPolyTypicPrimName` below.
convertPolytypicPrim 
        :: Env -> Env
        -> D.Name -> D.Type D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPolytypicPrim kenv _tenv n tArg
 = case n of
        -- IO primitives that need to be instantiated with RealWorld#.
        D.NameOpStore D.OpStoreNewArray
         | Just gv      <- findPrimitive (envImported kenv) n [tArg]
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpStore D.OpStoreReadArray
         | Just gv      <- findPrimitive (envImported kenv) n [tArg]
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpStore D.OpStoreWriteArray
         | Just gv      <- findPrimitive (envImported kenv) n [tArg]
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        -- Pure primitives.
        _
           | Just gv      <- findPrimitive (envImported kenv) n [tArg]
           ->   return  ( G.Var gv
                        , G.varType gv)


        -- ERROR: Primitive is in our prim table, but the Haskell client
        --        module hasn't imported an implementation of it.
        _  |  Just prim   <- find (\p -> primName p == n) primitives
           -> errorMissingPrim (envGuts kenv) n (Just $ primSymbol prim)

        -- ERROR: Primitive is not even in the prim table,
        --        this is definately a bug in the Repa plugin.
        _  -> errorMissingPrim (envGuts kenv) n Nothing



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

