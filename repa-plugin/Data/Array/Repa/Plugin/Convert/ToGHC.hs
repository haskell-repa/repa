
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


-- | Splice bindings from a DDC module into a GHC core program.
spliceModGuts
        :: Map D.Name GhcName   -- ^ Maps DDC names to GHC names.
        -> D.Module () D.Name   -- ^ DDC module.
        -> G.ModGuts            -- ^ GHC module guts.
        -> G.UniqSM G.ModGuts

spliceModGuts namesSource mm guts
 = do   -- Add the builtin names to the map we got from the source program.
        let names       = Map.union namesBuiltin namesSource

        -- Invert the map so it maps GHC names to DDC names.
        let names'      = Map.fromList 
                        $ map (\(x, y) -> (y, x)) 
                        $ Map.toList names

        binds'  <- mapM (spliceBind names names' mm) 
                $  G.mg_binds guts

        return  $ guts { G.mg_binds = binds' }


namesBuiltin :: Map D.Name GhcName
namesBuiltin 
 = Map.fromList
 $ [ (D.NameTyConFlow D.TyConFlowWorld,     GhcNameTyCon G.realWorldTyCon)
   , (D.NameTyConFlow (D.TyConFlowTuple 2), GhcNameTyCon G.unboxedPairTyCon)
   , (D.NameTyConFlow D.TyConFlowArray,     GhcNameTyCon G.byteArrayPrimTyCon)
   , (D.NamePrimTyCon D.PrimTyConInt,       GhcNameTyCon G.intPrimTyCon) ]


-- Splice ---------------------------------------------------------------------
-- | If a GHC core binding has a matching one in the provided DDC module
--   then convert the DDC binding from GHC core and use that instead.
spliceBind 
        :: Map D.Name  GhcName
        -> Map GhcName D.Name
        -> D.Module () D.Name
        -> G.CoreBind
        -> G.UniqSM G.CoreBind

-- If there is a matching binding in the Disciple module then use that.
spliceBind names names' mm (G.NonRec b _)
 | Just dn        <- Map.lookup (GhcNameVar b) names'
 , Just (_db, dx) <- lookupModuleBindOfName mm dn
 = do   (x', _)   <- convertExp names dx
        return  $ G.NonRec b x'

-- Otherwise leave the original GHC binding as it is.
spliceBind names _ _ b
 = return b


-------------------------------------------------------------------------------
-- | Lookup a top-level binding from a DDC module.
--   TODO: don't require a top-level letrec.
lookupModuleBindOfName
        :: D.Module () D.Name 
        -> D.Name 
        -> Maybe ( D.Bind D.Name
                 , D.Exp () D.Name)

lookupModuleBindOfName mm n
 | D.XLet _ (D.LRec bxs) _   <- D.moduleBody mm
 = find (\(b, _) -> D.takeNameOfBind b == Just n) bxs

 | otherwise
 = Nothing



-- Type ----------------------------------------------------------------------
convertType 
        :: Map D.Name GhcName
        -> D.Type D.Name -> G.Type

convertType names tt
 = case tt of
        -- DDC[World#]   => GHC[State# RealWorld#]
        D.TCon (D.TyConBound (D.UPrim (D.NameTyConFlow D.TyConFlowWorld) _) _)
         -> G.TyConApp G.statePrimTyCon [G.realWorldTy]

        -- DDC[Array# _] => GHC[MutableByteArray#]
        D.TApp{}
         | Just (D.NameTyConFlow D.TyConFlowArray, [_tA])
                <- D.takePrimTyConApps tt
         -> G.mkMutableByteArrayPrimTy G.realWorldTy

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


convertTyConApp 
        :: Map D.Name GhcName
        -> D.TyCon D.Name -> [G.Type] -> G.Type

convertTyConApp names tc tsArgs'
 = case tc of
        D.TyConBound (D.UName n) _
         | Just (GhcNameTyCon tc) <- Map.lookup n names
         -> G.TyConApp tc tsArgs'

        D.TyConBound (D.UPrim n _) _
         | Just (GhcNameTyCon tc) <- Map.lookup n names
         -> G.TyConApp tc tsArgs'

        D.TyConBound (D.UName (D.NameCon str)) _
         -> G.LitTy (G.StrTyLit $ G.fsLit str)

        D.TyConSpec D.TcConFun
         |  [t1, t2]     <- tsArgs'
         -> G.FunTy t1 t2

        _ -> error $ "repa-plugin.convertTyConApp: no match for " 
                   ++ show tc -- ++ " " ++ show (Map.keys names)


-- Top -----------------------------------------------------------------------
convertExp
        :: Map D.Name GhcName
        -> D.Exp () D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertExp names xx
 = case xx of
        -----------------------------------------
        -- Convert Core Flow's polymorphic array primops to monomorphic GHC primops.
        -- newArray# [tElem] xSize xWorld
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [D.BName nWorld tWorld, D.BName nArr tArr]) x1]
         | Just (  D.NameOpStore D.OpStoreNewArray
                , [D.XType tA, xNum, xWorld])
                <- D.takeXPrimApps xScrut
         -> do  
                (names1, vWorld') <- getExpBind  names  nWorld tWorld
                (names', vArr')   <- getExpBind  names1 nArr   tArr

                vOp               <- getPrimOpVar $ G.NewByteArrayOp_Char
                (xNum', _)        <- convertExp  names' xNum
                (xWorld', _)      <- convertExp  names' xWorld
                (x1', t1')        <- convertExp  names' x1

                let tScrut'       =  convertType names' (D.tTuple2 D.tWorld (D.tArray tA))
                let xScrut'       =  G.mkApps (G.Var vOp) 
                                        [G.Type G.realWorldTy, xNum', xWorld']
                vScrut'           <- newDummyVar tScrut'

                return  ( G.Case xScrut' vScrut' tScrut'
                                 [(G.DataAlt G.unboxedPairDataCon, [vWorld', vArr'], x1')]
                        , t1')

        -- writeArray# [tElem] xArr xIx xVal xWorld
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [D.BName nWorld tWorld, _bVoid]) x1]
         | Just (  D.NameOpStore D.OpStoreWriteArray
                , [D.XType tA, xArr, xIx, xVal, xWorld])
                <- D.takeXPrimApps xScrut
         -> do  
                (names', vWorld') <- getExpBind names nWorld tWorld

                let tA'          =  convertType names' tA
                let Just writeOp =  lookup tA writeByteArrayOps 
                vOp              <- getPrimOpVar writeOp
                (xArr',   _)     <- convertExp  names' xArr
                (xIx',    _)     <- convertExp  names' xIx
                (xVal',   _)     <- convertExp  names' xVal
                (xWorld', _)     <- convertExp  names' xWorld
                (x1', t1')       <- convertExp  names' x1

                let tScrut'      = convertType names' D.tWorld
                let xScrut'      = G.mkApps (G.Var vOp) 
                                        [G.Type G.realWorldTy, xArr', xIx', xVal', xWorld']

                return  ( G.Case xScrut' vWorld' tScrut'
                                [ (G.DEFAULT, [], x1')]
                        , t1')

        -- Generic Conversion -------------------
        -- Variables.
        D.XVar _ (D.UName dn)
         | Just (GhcNameVar gv) <- Map.lookup dn names
         -> do  return  ( G.Var gv
                        , G.varType gv)

        D.XVar _ (D.UPrim dn _)
         | Just (GhcNameVar gv) <- Map.lookup dn names
         -> do  return  ( G.Var gv
                        , G.varType gv)

        -- Constructors.
        D.XCon _ (D.DaCon dn dt _)
         -> case dn of
                D.DaConNamed (D.NameLitInt i)
                 -> return ( G.Lit (G.MachInt i)
                           , convertType names D.tInt)

                D.DaConNamed (D.NameLitNat i)
                 -> return ( G.Lit (G.MachInt i)
                           , convertType names D.tInt)

                _ -> error $ "repa-plugin.convertExp: no match for " ++ show xx

        -- Type abstractions.
        D.XLAM _ (D.BName dn _) xBody
         |  Just (GhcNameVar gv) <- Map.lookup dn names
         -> do  (xBody', tBody') <- convertExp names xBody
                return  ( G.Lam gv xBody'
                        , G.mkFunTy (G.varType gv) tBody')

        -- Function abstractions.
        D.XLam _ (D.BName dn dt) xBody
         -> do  (names', gv)     <- getExpBind names dn dt
                (xBody', tBody') <- convertExp names' xBody
                return  ( G.Lam gv xBody'
                        , G.mkFunTy (G.varType gv) tBody')

        -- General applications.
        D.XApp _ x1 x2
         -> do  (x1', t1')      <- convertExp names x1
                (x2', t2')      <- convertExp names x2
                let G.FunTy _ tResult' = t1'
                return  ( G.App x1' x2'
                        , t1')  -- WRONG

        -- Type arguments.
        D.XType t
         -> do  let t'          = convertType names t
                return  ( G.Type t'
                        , G.wordTy )    -- WRONG

        _ -> convertExp names (D.xNat () 666)


-- Exp ------------------------------------------------------------------------
-- | Get the GHC expression var corresponding to some DDC name.
getExpBind 
        :: Map D.Name GhcName 
        -> D.Name -> D.Type D.Name 
        -> G.UniqSM (Map D.Name GhcName, G.Var)

getExpBind names dn tName
 -- DDC name was created from a GHC name originally, or is the name of some
 -- primitive thing, so we can use the known GHC name for it.
 | Just (GhcNameVar gv) <- Map.lookup dn names
 = return (names, gv)

 -- DDC name was created during program transformation, so we need to create
 -- a new GHC name for it.
 | otherwise
 = do   let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName "x"
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let ty      = convertType names tName
        let info    = G.vanillaIdInfo
        let gv      = G.mkLocalVar details name ty info

        return  ( Map.insert dn (GhcNameVar gv) names
                , gv)


-- Variable utils -------------------------------------------------------------
getPrimOpVar :: G.PrimOp -> G.UniqSM G.Var
getPrimOpVar op
 = do   let details = G.PrimOpId   op
        let occName = G.primOpOcc  op
        let ty      = G.primOpType op
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkGlobalVar details name ty info

newDummyVar :: G.Type -> G.UniqSM G.Var
newDummyVar ty
 = do   let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName "x"
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkLocalVar details name ty info



writeByteArrayOps 
 = [ (D.tInt, G.WriteByteArrayOp_Int) ]


