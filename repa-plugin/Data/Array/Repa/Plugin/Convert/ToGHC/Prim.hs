
module Data.Array.Repa.Plugin.Convert.ToGHC.Prim
        ( convertPrim
        , convertPolytypicPrim
        , isPolytypicPrimName)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
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
import qualified DDC.Core.Flow.Compounds as D


-- | Convert a primop that has the same definition independent 
--   of its type arguments.
convertPrim 
        :: Env -> Env
        -> D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPrim _kenv tenv n 
        | Just gv       <- findPrimitive (envGuts tenv) n []
        = return (G.Var gv, G.varType gv)

        -- ERROR: Primitive is in our prim table, but the Haskell client
        --        module hasn't imported an implementation of it.
        |  Just prim   <- find (\p -> primName p == n) primitives
        = errorMissingPrim n (Just $ primSymbol prim)

        -- ERROR: Primitive is not even in the prim table,
        --        this is definately a bug in the Repa plugin.
        | otherwise
        = errorMissingPrim n Nothing


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
         | Just gv      <- findPrimitive (envGuts kenv) n [tArg]
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpStore D.OpStoreReadArray
         | Just gv      <- findPrimitive (envGuts kenv) n [tArg]
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpStore D.OpStoreWriteArray
         | Just gv      <- findPrimitive (envGuts kenv) n [tArg]
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        -- Pure primitives.
        _
           | Just gv      <- findPrimitive (envGuts kenv) n [tArg]
           ->   return  ( G.Var gv
                        , G.varType gv)


        -- ERROR: Primitive is in our prim table, but the Haskell client
        --        module hasn't imported an implementation of it.
        _  |  Just prim   <- find (\p -> primName p == n) primitives
           -> errorMissingPrim n (Just $ primSymbol prim)

        -- ERROR: Primitive is not even in the prim table,
        --        this is definately a bug in the Repa plugin.
        _  -> errorMissingPrim n Nothing


-------------------------------------------------------------------------------
data Prim
        = Prim
        { -- | Name of the primitive function in the Disciple Core code.
          primName      :: D.Name

          -- | The type arguments that a function must be applied to to
          --   match this primitive, or `Nothing` to ignore that argument.
        , primTypeArgs  :: [Maybe (D.Type D.Name)]

          -- | Name of the symbol in the GHC Core code.
        , primSymbol    :: String }


-- | Find the primitive function for the given name and type arguments.
findPrimitive 
        :: G.ModGuts 
        -> D.Name 
        -> [D.Type D.Name] 
        -> Maybe G.Var

findPrimitive guts name ts
        | Just prim'    <- find (\p -> primName p == name) primitives
        , length ts == length (primTypeArgs prim')
        , all id $ zipWith matchesTypeParam ts (primTypeArgs prim')
        = findImportedPrimVar guts (primSymbol prim')

        | otherwise
        = Nothing


-- | Check if this type argument matches the required parameter.
matchesTypeParam 
        :: D.Type D.Name 
        -> Maybe (D.Type D.Name) 
        -> Bool

matchesTypeParam t mt
 = case mt of
        Nothing -> True
        Just t' -> t == t'


-- | Check whether the function with this name must be handled polytypically. 
--   This is try when it has an entry in the primitive table that 
--   requires type arguments.
isPolytypicPrimName :: D.Name -> Bool
isPolytypicPrimName n
        | Just prim'    <- find (\p -> primName p == n) primitives
        = not $ null (primTypeArgs prim')

        | otherwise
        = False


-- | Complain that we couldn't find a primitive that we needed.
errorMissingPrim :: D.Name -> Maybe String -> a

errorMissingPrim _n (Just str)
 = error $ unlines
 $ map ("        " ++)
        [ ""
        , "Cannot find definition for primitive '" ++ str ++ "'"
        , ""
        , "When using the repa-plugin you need to import a module that"
        , "provides implementations for the primitives used by the lowering"
        , "transform."
        , ""
        , "This problem is likely caused by importing just the repa-series"
        , "module that contains the stream operators, but not the module that"
        , "contains the target primitives."
        , ""
        , "If you don't want to define your own primitives then try adding"
        , "  'import Data.Array.Repa.Series'  to your client module."
        , ""
        , "This is a problem with the Repa plugin, and not GHC proper."
        , "You can ignore the following request to report this as a GHC bug." ]


errorMissingPrim n Nothing
 = error $ unlines
 $ map ("        " ++)
        [ ""
        , "No Haskell symbol name for Disciple Core Flow primitive:"
        , "  '" ++ show n ++ "'"
        , ""
        , "Please report this problem on the Repa bug tracker,"
        , "  or complain about it on the Repa mailing list."
        , ""
        , "This is a problem with the Repa plugin, and not GHC proper."
        , "You can ignore the following request to report this as a GHC bug." ]


-------------------------------------------------------------------------------
primitives :: [Prim]
primitives
 = [    -- Arithmetic Operators
        Prim    (D.NamePrimArith D.PrimArithAdd)
                [Just D.tInt]
                "primAddInt"

   ,    Prim    (D.NamePrimArith D.PrimArithMul)
                [Just D.tInt]
                "primMulInt"

        -- Flow Operators
   ,    Prim    (D.NameOpFlow D.OpFlowRateOfStream)
                []
                "primRateOfStream"

        -- Store Operators
   ,    Prim    (D.NameOpStore D.OpStoreNext)
                [Just D.tInt]
                "primNextInt"

   ,    Prim    (D.NameOpStore D.OpStoreReadArray)
                [Just D.tInt]
                "primReadIntArray"

   ,    Prim    (D.NameOpStore D.OpStoreWriteArray)
                [Just D.tInt]
                "primWriteIntArray"

        -- Loop Combinators
   ,    Prim    (D.NameOpLoop D.OpLoopLoopN)
                [Nothing]
                "primLoop"
   ]

