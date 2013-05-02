
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
import Debug.Trace
import DDC.Base.Pretty

-- Exp ------------------------------------------------------------------------
-- | Get the GHC expression var corresponding to some DDC name.
{-
getExpBind 
        :: Env 
        -> D.Bind D.Name
        -> G.UniqSM (Env, G.Var)

getExpBind kenv b
 -- DDC name was created from a GHC name originally, or is the name of some
 -- primitive thing, so we can use the known GHC name for it.
 | D.BName dn _         <- b
 , Just (GhcNameVar gv) <- Map.lookup dn (envNames kenv)
 = return (names, gv)

 -- DDC name was created during program transformation, so we need to create
 -- a new GHC name for it.
 | otherwise
 = do   let tName   = D.typeOfBind b

        let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName "x"
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let ty      = convertType kenv tName
        let info    = G.vanillaIdInfo
        let gv      = G.mkLocalVar details name ty info

        let names'  = case b of
                        D.BName dn _ -> Map.insert dn (GhcNameVar gv) names
                        _            -> names

        return  ( names', gv)
-}

-- Type -----------------------------------------------------------------------
convertType 
        :: Env
        -> D.Type D.Name -> G.Type

convertType kenv tt
 = case tt of

        -- DDC[World#]   => GHC[State# RealWorld#]
        --   The GHC state token takes a phantom type to indicate
        --   what state thread it corresponds to.
        D.TCon (D.TyConBound (D.UPrim (D.NameTyConFlow D.TyConFlowWorld) _) _)
         -> G.TyConApp G.statePrimTyCon [G.realWorldTy]

        -- DDC[Array# _] => GHC[MutableByteArray#]
        --   GHC uses the same monomorphic array type to store all types
        --   of unboxed elements.
        D.TApp{}
         | Just (D.NameTyConFlow D.TyConFlowArray, [_tA])
                <- D.takePrimTyConApps tt
         -> G.mkMutableByteArrayPrimTy G.realWorldTy

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
         -> let tK'     = convertType kenv tK
            in  G.TyConApp tc [tK', tElem']


        -- Generic Conversion -------------------
        D.TVar (D.UName n)
         | Just gv              <- lookup n (envVars kenv)
         -> G.TyVarTy gv

        D.TCon tc
         -> convertTyConApp (envNames kenv) tc []

        D.TForall (D.BName n _) t
         | Just (GhcNameVar gv)   <- Map.lookup n (envNames kenv)       -- TODO: wrong
         -> G.ForAllTy gv (convertType kenv t)

        -- Function types.
        D.TApp{}
         | Just (t1, _, _, t2)    <- D.takeTFun tt
         -> let t1'     = convertType kenv t1
                t2'     = convertType kenv t2
            in  G.FunTy t1' t2'

        -- Applied type constructors.
        D.TApp{}
         | Just (tc, tsArgs)      <- D.takeTyConApps tt
         -> let tsArgs' = map (convertType kenv) tsArgs
            in  trace (renderIndent $ text "converted" <+> ppr tt)
                $ convertTyConApp (envNames kenv) tc tsArgs'

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
 = do   let gt   = convertType kenv t
        gv       <- newDummyVar str gt
        let env' = env { envVars       = (n, gv) : envVars env }
        return   (env', gv)

bindVar kenv env (D.BNone t)
 = do   let gt   = convertType kenv t
        gv       <- newDummyVar "v" gt
        return  (env, gv)

bindVar _ _ b
        = error $ "repa-plugin.ToGHC.bindVar: can't bind " ++ show b


bindVarT kenv           = bindVar kenv kenv
bindVarX kenv tenv      = bindVar kenv tenv

