
module Data.Array.Repa.Plugin.Convert.ToGHC
        (spliceModGuts)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Wrap
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.ToGHC.Prim
import Data.Array.Repa.Plugin.Convert.ToGHC.Env
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import Data.Array.Repa.Plugin.Convert.FatName
import Data.List
import Control.Monad
import Data.Map                         (Map)

import qualified HscTypes                as G
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TysPrim                 as G
import qualified TysWiredIn              as G
import qualified Var                     as G
import qualified DataCon                 as G
import qualified Literal                 as G
import qualified PrimOp                  as G
import qualified UniqSupply              as G

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

        binds'  <- liftM concat $ mapM (spliceBind guts names names' mm) 
                $  G.mg_binds guts

        return  $ guts { G.mg_binds = binds' }


namesBuiltin :: Map D.Name GhcName
namesBuiltin 
 = Map.fromList
 $ [ (D.NameTyConFlow D.TyConFlowWorld,     GhcNameTyCon G.realWorldTyCon)
   , (D.NameTyConFlow (D.TyConFlowTuple 2), GhcNameTyCon G.unboxedPairTyCon)
   , (D.NameTyConFlow D.TyConFlowArray,     GhcNameTyCon G.byteArrayPrimTyCon)
   , (D.NamePrimTyCon D.PrimTyConInt,       GhcNameTyCon G.intPrimTyCon) 
   , (D.NamePrimTyCon D.PrimTyConNat,       GhcNameTyCon G.intTyCon) ]  


-- Splice ---------------------------------------------------------------------
-- | If a GHC core binding has a matching one in the provided DDC module
--   then convert the DDC binding from GHC core and use that instead.
spliceBind 
        :: G.ModGuts
        -> Map D.Name  GhcName
        -> Map GhcName D.Name
        -> D.Module () D.Name
        -> G.CoreBind
        -> G.UniqSM [G.CoreBind]

-- If there is a matching binding in the Disciple module then use that.
spliceBind guts names names' mm (G.NonRec gbOrig _)
 | Just nOrig                  <- Map.lookup (GhcNameVar gbOrig) names'
 , Just (dbLowered, dxLowered) <- lookupModuleBindOfName mm nOrig
 = do   
        -- make a new binding for the lowered version.
        let dtLowered         = D.typeOfBind dbLowered
        let gtLowered         = convertType names dtLowered
        gvLowered             <- newDummyVar "lowered" gtLowered

        -- convert the lowered version
        let env = Env
                { envGuts       = guts
                , envNames      = names
                , envVarsT      = []
                , envVarsX      = [] }

        (gxLowered, _)        
                <- convertExp env dxLowered

        -- Call the lowered version from the original,
        --  adding a wrapper to (unsafely) pass the world token and
        --  marshal boxed to unboxed values.
        xCall   <- wrapLowered 
                        (G.varType gbOrig) gtLowered
                        [] 
                        gvLowered

        return  [ G.NonRec gbOrig  xCall
                , G.NonRec gvLowered gxLowered ]   -- TODO: attach NOINLINE pragma
                                                   --       so the realWorld token doesn't get
                                                   --       substituted.


-- Otherwise leave the original GHC binding as it is.
spliceBind _ _ _ _ b
 = return [b]


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


-- Top -----------------------------------------------------------------------
convertExp
        :: Env  
        -> D.Exp () D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertExp env xx
 = case xx of
        ---------------------------------------------------
        -- Convert Core Flow's polymorphic array primops to monomorphic GHC primops.
        -- newArray# [tElem] xSize xWorld
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [ bWorld@(D.BName _ _)
                                     ,   bArr@(D.BName _ _)]) x1]
         | Just (  D.NameOpStore D.OpStoreNewArray
                , [D.XType tA, xNum, xWorld])
                <- D.takeXPrimApps xScrut
         -> do  
                -- scrut
                vOp             <- getPrimOpVar $ G.NewByteArrayOp_Char
                (xNum', _)      <- convertExp env xNum
                (xWorld', _)    <- convertExp env xWorld

                let names       = envNames env
                let tScrut'     = convertType names (D.tTuple2 D.tWorld (D.tArray tA))
                let xScrut'     = G.mkApps (G.Var vOp) 
                                        [G.Type G.realWorldTy, xNum', xWorld']
                vScrut'         <- newDummyVar "scrut" tScrut'

                -- alt
                (env1,   vWorld') <- bindVarX env  bWorld
                (env2,   vArr')   <- bindVarX env1 bArr
                let env'        = env2
                (x1', t1')      <- convertExp env' x1

                return  ( G.Case xScrut' vScrut' t1'
                                 [(G.DataAlt G.unboxedPairDataCon, [vWorld', vArr'], x1')]
                        , t1')


        -- writeArray# [tElem] xArr xIx xVal xWorld
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [ _bWorld@(D.BName _ _)
                                     , _bVoid]) x1]
         | Just (  D.NameOpStore D.OpStoreWriteArray
                , [D.XType tA, xArr, xIx, xVal, xWorld])
                <- D.takeXPrimApps xScrut
         -> do  
                -- scrut
                Just vOp        <- getPrim_writeByteArrayOpM (envGuts env) tA
                (xArr',   _)    <- convertExp env xArr
                (xIx',    _)    <- convertExp env xIx
                (xVal',   _)    <- convertExp env xVal
                (xWorld', _)    <- convertExp env xWorld

                let names       = envNames env
                let tScrut'     = convertType names (D.tTuple2 D.tWorld tA)
                let xScrut'     = G.mkApps (G.Var vOp) 
                                        [G.Type G.realWorldTy, xArr', xIx', xVal', xWorld']
                vScrut'         <- newDummyVar "scrut" tScrut'

                -- alt
                (x1', t1')      <- convertExp env x1

                return  ( G.Case xScrut' vScrut' t1'
                                [ (G.DEFAULT, [], x1')]
                        , t1')


        -- Generic Conversion -----------------------------
        -- Variables.
        --  Names of plain variables should be in the name map, and refer
        --  other top-level bindings, or dummy variables that we've
        --  introduced locally in this function.
        D.XVar _ (D.UName dn)
         |  Just gv     <- lookup dn (envVarsX env)
         ->     return  ( G.Var gv
                        , G.varType gv)

        -- Primops.
        --  Polymorphic primops are handled specially in the code above, 
        --  for all others we should be able to map them to a top-level
        --  binding in the source module, or convert them directly to 
        --  a GHC primop.
        D.XVar _ (D.UPrim n _)
         -> do  gv      <- ghcVarOfPrimName (envGuts env) n
                return  ( G.Var gv
                        , G.varType gv)


        -- Data constructors.
        D.XCon _ (D.DaCon dn _ _)
         -> case dn of
                -- Unit constructor.
                D.DaConUnit
                 -> return ( G.Var (G.dataConWorkId G.unitDataCon)
                           , G.unitTy )

                -- Int# literal
                D.DaConNamed (D.NameLitInt i)
                 -> return ( G.Lit (G.MachInt i)
                           , G.intPrimTy)

                -- Nat# literal
                -- Disciple unsigned Nat#s just get squashed onto GHC Int#s.
                D.DaConNamed (D.NameLitNat i)
                 -> return ( G.Lit (G.MachInt i)
                           , G.intPrimTy)

                -- T2# data constructor
                D.DaConNamed (D.NameDaConFlow (D.DaConFlowTuple 2))
                 -> return ( G.Var      (G.dataConWorkId G.unboxedPairDataCon)
                           , G.varType $ G.dataConWorkId G.unboxedPairDataCon)

                -- Don't know how to convert this.
                _ -> error $ "repa-plugin.ToGHC.convertExp: "
                           ++ "Cannot convert DDC data constructor " 
                                ++ show xx ++ " to GHC Core."


        -- Type abstractions for rate variables.
        --   If we're binding a rate type variable then also inject the
        --   value level version.
        D.XLAM _ (b@(D.BName (D.NameVar ks) k)) xBody
         |  k == D.kRate
         -> do  
                (env1, gv)      <- bindVarT env b

                let ks_val      = ks ++ "_val"         -- TODO: handle singletons properly in DDC pass.
                let dn_val      = D.NameVar ks_val
                gv_val          <- newDummyVar "rate" G.intPrimTy
                let env'        = env1 { envVarsX = (dn_val, gv_val) : envVarsX env1 }

                (xBody', tBody') <- convertExp env' xBody

                return  ( G.Lam gv $ G.Lam gv_val xBody'
                        , G.mkFunTy (G.varType gv) tBody')


        -- Type abstractions.
        D.XLAM _ b@(D.BName{}) xBody
         -> do  
                (env',   gv)     <- bindVarT env b
                (xBody', tBody') <- convertExp env' xBody

                return  ( G.Lam gv xBody'
                        , G.mkForAllTy gv tBody')


        -- Function abstractions.
        D.XLam _ b@(D.BName{}) xBody
         -> do  
                (env',   gv)     <- bindVarX env b
                (xBody', tBody') <- convertExp env' xBody

                return  ( G.Lam gv  xBody'
                        , G.mkFunTy (G.varType gv) tBody')


        -- Application of a polymorphic primitive.
        -- In GHC core, functions cannot be polymorphic in unlifted primitive
        -- types. We convert most of the DDC polymorphic prims in a uniform way.
        D.XApp _ (D.XVar _ (D.UPrim n _)) (D.XType t)
         ->     convertPolyPrim env n t


        -- General applications.
        D.XApp _ x1 x2
         -> do  (x1', t1')      <- convertExp env x1
                (x2', _)        <- convertExp env x2
                return  ( G.App x1' x2'
                        , t1')                                          -- TODO: wrong type.

        -- Case expressions
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [ bWorld@(D.BName{})
                                     ,     b2]) x1]
         -> do  
                -- scrut
                (xScrut', tScrut') <- convertExp env xScrut
                vScrut'            <- newDummyVar "scrut" tScrut'

                -- alt
                (env1,    vWorld') <- bindVarX env  bWorld
                (env2,    v2')     <- bindVarX env1 b2
                let env' = env2

                (x1',     t1')     <- convertExp env' x1

                return ( G.Case xScrut' vScrut' t1'
                                [ (G.DataAlt G.unboxedPairDataCon, [vWorld', v2'], x1') ]
                       , t1')

        -- Type arguments.
        D.XType t
         -> do  let t'          = convertType (envNames env) t
                return  ( G.Type t'
                        , G.wordTy )                                -- TODO: wrong type.

        _ -> convertExp env (D.xNat () 666)

