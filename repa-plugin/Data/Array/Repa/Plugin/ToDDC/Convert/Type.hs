
module Data.Array.Repa.Plugin.ToDDC.Convert.Type
        ( convertVarType
        , convertType)
where
import Data.Array.Repa.Plugin.ToDDC.Convert.Var
import Data.Array.Repa.Plugin.FatName

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D

import qualified Type                   as G
import qualified TypeRep                as G
import qualified TyCon                  as G
import qualified Var                    as G
import qualified FastString             as G


-- Variables ------------------------------------------------------------------
-- | Convert a type from a GHC variable.
convertVarType :: G.Var -> D.Type FatName
convertVarType v
        = convertType $ G.varType v


-- Type -----------------------------------------------------------------------
-- | Convert a type.
convertType :: G.Type -> D.Type FatName
convertType tt
 = case tt of
        G.TyVarTy v
         -> D.TVar    (D.UName (convertFatName v)) 

        G.AppTy t1 t2
         -> D.TApp    (convertType t1) (convertType t2)

        G.TyConApp tc ts
         -> D.tApps   (D.TCon (convertTyCon tc)) (map convertType ts)

        G.FunTy t1 t2
         -> D.tFunPE  (convertType t1) (convertType t2)

        G.ForAllTy v t
         -> D.TForall (D.BName (convertFatName v) D.kData)
                      (convertType t)

        G.LitTy (G.NumTyLit _) 
         -> error "repa-plugin.slurpType: numeric type literals not handled."

        G.LitTy tyLit@(G.StrTyLit fs)
         -> D.TVar  (D.UName (FatName (GhcNameTyLit tyLit)
                                      (D.NameCon (G.unpackFS fs))))


-- | Convert a tycon.
convertTyCon :: G.TyCon -> D.TyCon FatName
convertTyCon tc
        | G.isFunTyCon tc
        = D.TyConSpec D.TcConFun

        | otherwise
        = D.TyConBound
                (D.UName (FatName (GhcNameTyCon tc)
                         (convertName $ G.tyConName tc)))
                (D.kData)                                                       -- TODO: WRONG
