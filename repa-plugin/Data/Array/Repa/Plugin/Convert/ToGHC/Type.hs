
module Data.Array.Repa.Plugin.Convert.ToGHC.Type
        (convertType)
where
import Data.Array.Repa.Plugin.Convert.FatName
import DDC.Base.Pretty
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import Data.Map                         (Map)

import qualified HscTypes                as G
import qualified Avail                   as G
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TypeRep                 as G
import qualified TysPrim                 as G
import qualified TysWiredIn              as G
import qualified TyCon                   as G
import qualified IdInfo                  as G
import qualified Coercion                as G
import qualified Var                     as G
import qualified DataCon                 as G
import qualified Literal                 as G
import qualified Id                      as G
import qualified PrimOp                  as G
import qualified Unique                  as G
import qualified UniqSupply              as G
import qualified FastString              as G
import qualified UniqFM                  as UFM
import qualified OccName                 as OccName
import qualified Name                    as Name

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Module         as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Prim      as D
import qualified DDC.Core.Flow.Compounds as D

import qualified Data.Map                as Map


convertType 
        :: Map D.Name GhcName
        -> D.Type D.Name -> G.Type

convertType names tt
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
         , Just (GhcNameTyCon tc) <- Map.lookup nStream names
         , Just tElem'            <- boxedGhcTypeOfFlowType tElem
         -> let tK'     = convertType names tK
            in  G.TyConApp tc [tK', tElem']


        -- Generic Conversion -------------------
        D.TVar (D.UName n)
         | Just (GhcNameVar gv)   <- Map.lookup n names
         -> G.TyVarTy gv

        D.TCon tc
         -> convertTyConApp names tc []

        D.TForall (D.BName n _) t
         | Just (GhcNameVar gv)   <- Map.lookup n names
         -> G.ForAllTy gv (convertType names t)

        D.TApp{}
         | Just (tc, tsArgs)      <- D.takeTyConApps tt
         -> let tsArgs' = map (convertType names) tsArgs
            in  convertTyConApp names tc tsArgs'

        _ -> error $ "repa-plugin.convertType: no match for " ++ show tt


-- TyConApp -------------------------------------------------------------------
-- Type constructor applications.
convertTyConApp 
        :: Map D.Name GhcName
        -> D.TyCon D.Name -> [G.Type] -> G.Type

convertTyConApp names tc tsArgs'
 = case tc of
        D.TyConBound (D.UName n) _
         | Just (GhcNameTyCon tc) <- Map.lookup n names
         -> G.TyConApp tc tsArgs'

        D.TyConBound (D.UPrim n _) _
         | Just tc'               <- convertTyConPrimName n
         -> G.TyConApp tc' tsArgs'

        D.TyConBound (D.UName (D.NameCon str)) _
         -> G.LitTy (G.StrTyLit $ G.fsLit str)

        D.TyConSpec D.TcConFun
         |  [t1, t2]     <- tsArgs'
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



