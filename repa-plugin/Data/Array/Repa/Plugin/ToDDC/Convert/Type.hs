
module Data.Array.Repa.Plugin.ToDDC.Convert.Type
        ( convertVarType
        , convertType
        , convertKind)
where
import Data.Array.Repa.Plugin.ToDDC.Convert.Base
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
convertVarType :: G.Var -> Either Fail (D.Type FatName)
convertVarType v
        = convertType $ G.varType v


-- Type -----------------------------------------------------------------------
-- | Convert a type.
convertType :: G.Type -> Either Fail (D.Type FatName)
convertType tt
 = case tt of
        G.TyVarTy v
         -> do  v'      <- convertFatName v
                return  $ D.TVar (D.UName v')

        G.AppTy t1 t2
         -> do  t1'     <- convertType t1
                t2'     <- convertType t2
                return  $ D.TApp t1' t2'

        G.TyConApp tc ts
         -> do  tc'     <- convertTyCon tc
                ts'     <- mapM convertType ts
                return  $ D.tApps (D.TCon tc') ts'

        G.FunTy t1 t2
         -> do  t1'     <- convertType t1
                t2'     <- convertType t2
                return  $ D.tFunPE t1' t2'

        G.ForAllTy v t
         -> do  v'      <- convertFatName v
                t'      <- convertType t
                return  $ D.TForall (D.BName v' D.kData) t'

        G.LitTy (G.NumTyLit _) 
         -> error "repa-plugin.slurpType: numeric type literals not handled."

        G.LitTy tyLit@(G.StrTyLit fs)
         ->     return  $ D.TVar  (D.UName (FatName (GhcNameTyLit tyLit)
                                                    (D.NameCon (G.unpackFS fs))))


-- TyCon ----------------------------------------------------------------------
-- | Convert a tycon.
convertTyCon :: G.TyCon -> Either Fail (D.TyCon FatName)
convertTyCon tc
        | G.isFunTyCon tc
        =       return $ D.TyConSpec D.TcConFun

        | otherwise
        = do    name'   <- convertName $ G.tyConName tc
                return  $ D.TyConBound
                                (D.UName (FatName (GhcNameTyCon tc) name'))
                                (D.kData)                       -- TODO: WRONG


-- Kind -----------------------------------------------------------------------
-- | Convert a kind: particularly function arrows are changed to kind arrows.
convertKind :: G.Type -> Either Fail (D.Type FatName)
convertKind tt
 = case tt of
        G.TyVarTy v
         -> do  v'      <- convertFatName v
                return  $ D.TVar (D.UName v')

        G.AppTy t1 t2
         -> do  t1'     <- convertKind t1
                t2'     <- convertKind t2
                return  $ D.TApp t1' t2'

        G.TyConApp tc ts
         -> do  tc'     <- convertTyCon tc
                ts'     <- mapM convertKind ts
                return  $ D.tApps (D.TCon tc') ts'

        G.FunTy t1 t2
         -> do  t1'     <- convertKind t1
                t2'     <- convertKind t2
                return  $ D.kFun t1' t2'

        G.ForAllTy v t
         -> do  v'      <- convertFatName v
                t'      <- convertKind t
                return  $ D.TForall (D.BName v' D.kData) t'

        G.LitTy (G.NumTyLit _) 
         -> error "repa-plugin.convertKind: numeric type literals not handled."

        G.LitTy tyLit@(G.StrTyLit fs)
         ->     return  $ D.TVar  (D.UName (FatName (GhcNameTyLit tyLit)
                                                    (D.NameCon (G.unpackFS fs))))


