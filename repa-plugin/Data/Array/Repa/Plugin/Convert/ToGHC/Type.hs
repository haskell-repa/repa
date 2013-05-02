
module Data.Array.Repa.Plugin.Convert.ToGHC.Type
        ( convertType

        , Env(..)
        , bindVarT
        , bindVarX)
where
import Data.Array.Repa.Plugin.Convert.FatName
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import Data.Map                         (Map)

import qualified HscTypes                as G
import qualified Type                    as G
import qualified TypeRep                 as G
import qualified TysPrim                 as G
import qualified TysWiredIn              as G
import qualified TyCon                   as G
import qualified UniqSupply              as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Compounds as D
import qualified DDC.Core.Flow.Prim      as D

import qualified Data.Map                as Map


-- Type -----------------------------------------------------------------------
convertType 
        :: Env
        -> D.Type D.Name 
        -> G.UniqSM G.Type

convertType kenv tt
 = case tt of

        -- DDC[World#]   => GHC[State# RealWorld#]
        --   The GHC state token takes a phantom type to indicate
        --   what state thread it corresponds to.
        D.TCon (D.TyConBound (D.UPrim (D.NameTyConFlow D.TyConFlowWorld) _) _)
         -> return $ G.TyConApp G.statePrimTyCon [G.realWorldTy]

        -- DDC[Array# _] => GHC[MutableByteArray#]
        --   GHC uses the same monomorphic array type to store all types
        --   of unboxed elements.
        D.TApp{}
         | Just (D.NameTyConFlow D.TyConFlowArray, [_tA])
                <- D.takePrimTyConApps tt
         -> return $ G.mkMutableByteArrayPrimTy G.realWorldTy

        -- DDC[Stream# a] => GHC[Stream {Lifted a}]
        --   In the code we get from the lowering transform, for element
        --   types like Int# the "hash" refers to the fact that it is
        --   primitive, and not nessesarally unboxed. The type arguments 
        --   to 'Stream' in GHC land need to be the boxed versions.
        D.TApp{}
         | Just (nStream@(D.NameTyConFlow D.TyConFlowStream), [tK, tElem])
                <- D.takePrimTyConApps tt
         , Just (GhcNameTyCon tc) <- Map.lookup nStream (envNames kenv)
         , Just tElem'            <- boxedGhcTypeOfElemType tElem
         -> do  tK'     <- convertType kenv tK
                return  $ G.TyConApp tc [tK', tElem']


        -- Generic Conversion -------------------
        D.TVar (D.UName n)
         -> case lookup n (envVars kenv) of
                Nothing
                 -> error $ unlines 
                          [ "repa-plugin.ToGHC.convertType: variable " 
                                     ++ show n ++ " not in scope"
                          , "env = " ++ show (map fst $ envVars kenv) ]

                Just gv  
                 -> return $ G.TyVarTy gv

        D.TCon tc
         -> return $ convertTyConApp (envNames kenv) tc []

        D.TForall b t
         -> do  (kenv', gv)     <- bindVarT kenv b
                t'              <- convertType kenv' t
                return  $ G.ForAllTy gv t'

        -- Function types.
        D.TApp{}
         | Just (t1, _, _, t2)    <- D.takeTFun tt
         -> do  t1'     <- convertType kenv t1
                t2'     <- convertType kenv t2
                return  $  G.FunTy t1' t2'

        -- Applied type constructors.
        D.TApp{}
         | Just (tc, tsArgs)      <- D.takeTyConApps tt
         -> do  tsArgs' <- mapM (convertType kenv) tsArgs
                return  $ convertTyConApp (envNames kenv) tc tsArgs'

        _ -> error $ "repa-plugin.convertType: no match for " ++ show tt


-- TyConApp -------------------------------------------------------------------
-- Type constructor applications.
convertTyConApp 
        :: Map D.Name GhcName
        -> D.TyCon D.Name -> [G.Type] -> G.Type

convertTyConApp names tc tsArgs'
 = case tc of
        D.TyConBound (D.UPrim n _) _
         | Just tc'                <- convertTyConPrimName n
         -> G.TyConApp tc' tsArgs'

        D.TyConBound (D.UName n) _
         | Just (GhcNameTyCon tc') <- Map.lookup n names
         -> G.TyConApp tc' tsArgs'

--        D.TyConBound (D.UName (D.NameCon str)) _
--         -> G.LitTy (G.StrTyLit $ G.fsLit str)

        D.TyConSpec D.TcConFun
         | [t1, t2] <- tsArgs'
         -> G.FunTy t1 t2

        _ -> error $ "repa-plugin.convertTyConApp: no match for " 
                   ++ show tc -- ++ " " ++ show (Map.keys names)


-- TyCon ----------------------------------------------------------------------
-- | Convert a Flow type constructor name to a GHC type constructor.
convertTyConPrimName :: D.Name -> Maybe G.TyCon
convertTyConPrimName n
 = case n of
        D.NameTyConFlow (D.TyConFlowTuple 2)
         -> Just G.unboxedPairTyCon

        D.NameKiConFlow D.KiConFlowRate
         -> Just G.liftedTypeKindTyCon

        D.NamePrimTyCon D.PrimTyConNat  
         -> Just G.intPrimTyCon

        D.NamePrimTyCon D.PrimTyConInt  
         -> Just G.intPrimTyCon

        _ -> Nothing


-------------------------------------------------------------------------------
-- | Get the GHC boxed type corresponding to this Flow element type.
boxedGhcTypeOfElemType :: D.Type D.Name -> Maybe G.Type
boxedGhcTypeOfElemType t
 | t == D.tInt          = Just G.intTy
 | otherwise            = Nothing



-- Env ------------------------------------------------------------------------
-- | Environment used to map DDC names to GHC names.
--   Used when converting DDC Core to GHC core.
data Env
        = Env 
        { -- | Guts of the original GHC module.
          envGuts       :: G.ModGuts

          -- | Name map we got during the original GHC -> DDC conversion.
        , envNames      :: Map D.Name GhcName

          -- | Locally scoped variables.
        , envVars       :: [(D.Name, G.Var)]
        }


-- | Bind a fresh GHC variable for a DDC expression variable.
bindVar :: Env -> Env -> D.Bind D.Name -> G.UniqSM (Env, G.Var)
bindVar kenv env (D.BName n@(D.NameVar str) t)
 = do   gt      <-  convertType kenv t
        gv      <- newDummyVar str gt
        let env' = env { envVars       = (n, gv) : envVars env }
        return   (env', gv)

bindVar kenv env (D.BNone t)
 = do   gt      <- convertType kenv t
        gv      <- newDummyVar "v" gt
        return  (env, gv)

bindVar _ _ b
        = error $ "repa-plugin.ToGHC.bindVar: can't bind " ++ show b


bindVarT kenv           = bindVar kenv kenv
bindVarX kenv tenv      = bindVar kenv tenv

