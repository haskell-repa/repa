
module Data.Array.Repa.Plugin.Convert.ToGHC
        (spliceModGuts)
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


-- | Splice top-level DDC module into a GHC core program.
spliceModGuts
        :: Map D.Name GhcName
        -> D.Module () D.Name
        -> G.ModGuts 
        -> G.UniqSM G.ModGuts

spliceModGuts names mm guts
 = do   -- Invert the map so it maps GHC names to DDC names.
        let names'      = Map.fromList 
                        $ map (\(x, y) -> (y, x)) 
                        $ Map.toList names

        binds'  <- mapM (spliceBind names names' mm) 
                $  G.mg_binds guts

        return  $ guts { G.mg_binds = binds' }


-- Splice ---------------------------------------------------------------------
-- | If a GHC core binding has a matching one in the provided DDC module
--   then convert the DDC binding from GHC core and use that instead.
spliceBind 
        :: Map D.Name  GhcName
        -> Map GhcName D.Name
        -> D.Module () D.Name
        -> G.CoreBind
        -> G.UniqSM G.CoreBind

spliceBind names names' mm (G.NonRec b _)
 -- If there is a matching binding in the Disciple module then use that.
 | Just dn      <- Map.lookup (GhcNameVar b) names'
 , Just dx      <- lookupModuleBindOfName mm dn
 = do   x'      <- convertTop names dx
        return  $ G.NonRec b x'

spliceBind names _ _ b
 = return b


-------------------------------------------------------------------------------
-- | Lookup a top-level binding from a DDC module.
--   TODO: don't require a top-level letrec.
lookupModuleBindOfName
        :: D.Module () D.Name 
        -> D.Name 
        -> Maybe (D.Exp () D.Name)

lookupModuleBindOfName mm n
 | D.XLet _ (D.LRec bxs) _   <- D.moduleBody mm
 = liftM snd $ find (\(b, _) -> D.takeNameOfBind b == Just n) bxs

 | otherwise
 = Nothing


-- Type ----------------------------------------------------------------------
convertType :: D.Type D.Name -> G.Type
convertType tt
 = error "blerk convertType"


-- Top -----------------------------------------------------------------------
-- Convert outermost lambdas then inject a variable to bind RealWord#
convertTop
        :: Map D.Name GhcName
        -> D.Exp () D.Name
        -> G.UniqSM G.CoreExpr 

convertTop names xx
 = case xx of
        -- Type abstractions.
        D.XLAM _ (D.BName dn _) x
         |  Just (GhcNameVar gv)    <- Map.lookup dn names
         -> do  x'      <- convertTop names x
                return  $  G.Lam gv x'

        -- Function abstractions.
        D.XLam _ (D.BName dn _) x
         |  Just (GhcNameVar gv)    <- Map.lookup dn names
         -> do  x'      <- convertTop names x
                return  $  G.Lam gv x'

        -- Enter into body of function.
        _ -> do
                vWorld  <- newWorldVar
                (x', _) <- convertStmt names vWorld xx
                return  $  G.Lam vWorld x'


-- Stmt -----------------------------------------------------------------------
convertStmt
        :: Map D.Name GhcName 
        -> G.Var
        -> D.Exp () D.Name 
        -> G.UniqSM (G.CoreExpr, G.Type)

convertStmt names vWorld xx
 -- new# [tElem] xZero
 |  D.XLet _ (D.LLet _ (D.BName n t) x1) x2      <- xx
 ,  Just (D.XVar _ uNew, [D.XType tElem, xZero]) <- D.takeXApps x1
 ,  D.UName  (D.NameOpStore D.OpStoreNew)        <- uNew
 = do   
        let op     = G.NewByteArrayOp_Char
        vNew       <- getPrimOpVar op
        let tElem' =  convertType tElem
        let xZero' =  convertExp  xZero
        vWorld'    <- newWorldVar
        (x2', t2') <- convertStmt names vWorld x2

        vScrut     <- newDummyVar tElem'
        return  $ (G.Case (G.mkApps (G.Var vNew) [G.Var vWorld])
                          vScrut t2'
                          [ (G.DataAlt G.unboxedPairDataCon, [], x2')] -- TODO bind data
                  , t2')

 | otherwise
 = return $ (G.Lit (G.MachChar 'a'), error "blerk type")


-- Exp ------------------------------------------------------------------------
convertExp = error "blerk convertExp"

-- Variable utils -------------------------------------------------------------
newWorldVar :: G.UniqSM G.Var
newWorldVar
 = do   let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName "w"
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName 
        let ty      = G.realWorldTy
        let info    = G.vanillaIdInfo
        return  $ G.mkLocalVar details name ty info

newDummyVar  :: G.Type -> G.UniqSM G.Var
newDummyVar ty
 = do   let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName "x"
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkLocalVar details name ty info

getPrimOpVar :: G.PrimOp -> G.UniqSM G.Var
getPrimOpVar op
 = do   let details = G.PrimOpId op
        let occName = G.primOpOcc op
        let ty      = G.primOpType op
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkLocalVar details name ty info
