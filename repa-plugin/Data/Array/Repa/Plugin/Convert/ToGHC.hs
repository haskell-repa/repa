
module Data.Array.Repa.Plugin.Convert.ToGHC
        (spliceModGuts)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.ToGHC.Prim
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import Data.Array.Repa.Plugin.Convert.FatName
import Data.List
import Control.Monad
import Data.Map                         (Map)

import qualified HscTypes                as G
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TypeRep                 as G
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
        gvLowered              <- newDummyVar "lowered" gtLowered

        -- convert the lowered version
        (gxLowered, _)        <- convertExp guts names dxLowered

        return  [ G.NonRec gbOrig  (G.Var gvLowered)    -- TODO: add wrapper
                , G.NonRec gvLowered gxLowered ]


-- Otherwise leave the original GHC binding as it is.
spliceBind _ _ _ _ b
 = return [b]

{-
callLowered 
        :: G.Type       -- ^ Type of original version.
        -> G.Type       -- ^ Type of lowered  version.
        -> [G.Var]      -- ^ Lambda bound variables in wrapper.
        -> G.Var        -- ^ Name of lowered version.
        -> G.CoreExpr

callLowered tOrig tLowered vsParam vLowered
        | G.ForAllTy vOrig tOrig'     <- tOrig
        , G.ForAllTy _     tLowered'  <- tLowered
        = G.Lam vOrig (callLowered tOrig' tLowered' vLowered)

        | G.FunTy tOrig1      tOrig2    <- tOrig
        , G.FunTy _tLowered1 _tLowered2 <- tLowered
        = G.FunTy tOrig1 tOrig2
-}

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
        :: G.ModGuts
        -> Map D.Name GhcName
        -> D.Exp () D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertExp guts names xx
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
                (names1, vWorld') <- getExpBind  names  bWorld
                (names', vArr')   <- getExpBind  names1 bArr

                vOp             <- getPrimOpVar $ G.NewByteArrayOp_Char
                (xNum', _)      <- convertExp  guts names' xNum
                (xWorld', _)    <- convertExp  guts names' xWorld
                (x1', t1')      <- convertExp  guts names' x1

                let tScrut'     =  convertType names' (D.tTuple2 D.tWorld (D.tArray tA))
                let xScrut'     =  G.mkApps (G.Var vOp) 
                                        [G.Type G.realWorldTy, xNum', xWorld']
                vScrut'           <- newDummyVar "scrut" tScrut'

                return  ( G.Case xScrut' vScrut' tScrut'
                                 [(G.DataAlt G.unboxedPairDataCon, [vWorld', vArr'], x1')]
                        , t1')

        -- writeArray# [tElem] xArr xIx xVal xWorld
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [ bWorld@(D.BName _ _)
                                     , _bVoid]) x1]
         | Just (  D.NameOpStore D.OpStoreWriteArray
                , [D.XType tA, xArr, xIx, xVal, xWorld])
                <- D.takeXPrimApps xScrut
         -> do  
                Just vOp          <- getPrim_writeByteArrayOpM guts tA
                
                (xArr',   _)      <- convertExp  guts names xArr
                (xIx',    _)      <- convertExp  guts names xIx
                (xVal',   _)      <- convertExp  guts names xVal
                (xWorld', _)      <- convertExp  guts names xWorld
 
                let xScrut'       =  G.mkApps (G.Var vOp) 
                                        [G.Type G.realWorldTy, xArr', xIx', xVal', xWorld']
                let tScrut'       =  convertType names  D.tWorld

                (names', vWorld') <- getExpBind  names bWorld
                (x1', t1')        <- convertExp  guts names' x1

                return  ( G.Case xScrut' vWorld' tScrut'
                                [ (G.DEFAULT, [], x1')]
                        , t1')


        -- Generic Conversion -----------------------------

        -- Variables.
        --  Names of plain variables should be in the name map, and refer
        --  other top-level bindings, or dummy variables that we've
        --  introduced locally in this function.
        D.XVar _ (D.UName dn)
         -> do  let Just (GhcNameVar gv) = Map.lookup dn names
                return  ( G.Var gv
                        , G.varType gv)

        -- Primops.
        --  Polymorphic primops are handled specially in the code above, 
        --  for all others we should be able to map them to a top-level
        --  binding in the source module, or convert them directly to 
        --  a GHC primop.
        D.XVar _ (D.UPrim n _)
         -> do  gv      <- ghcVarOfPrimName guts n
                return  ( G.Var gv
                        , G.varType gv)

        -- Data constructors.
        D.XCon _ (D.DaCon dn _ _)
         -> case dn of
                -- Unit constructor.
                D.DaConUnit
                 -> return ( G.Var (G.dataConWorkId G.unitDataCon)
                           , G.TyConApp G.unitTyCon [])

                -- Int# literal
                D.DaConNamed (D.NameLitInt i)
                 -> return ( G.Lit (G.MachInt i)
                           , convertType names D.tInt)

                -- Nat# literal
                D.DaConNamed (D.NameLitNat i)
                 -> return ( G.Lit (G.MachInt i)
                           , convertType names D.tInt)

                -- T2# data constructor
                D.DaConNamed (D.NameDaConFlow (D.DaConFlowTuple 2))
                 -> return ( G.Var (G.dataConWorkId G.unboxedPairDataCon)
                           , G.TyConApp G.unboxedPairTyCon [])

                _ -> error $ "repa-plugin.toGHC.convertExp: no match for " ++ show xx

        -- Type abstractions.
        --   If we're binding a rate type variable then also inject the
        --   value level version.
        D.XLAM _ (D.BName dn@(D.NameVar ks) k) xBody
         |  Just (GhcNameVar gv) <- Map.lookup dn names
         ,  k == D.kRate
         -> do  let ks_val       =  ks ++ "_val"                        -- HACKS!
                gv_val          <- newDummyVar "rate" G.intPrimTy
                let names'       =  Map.insert (D.NameVar ks_val) (GhcNameVar gv_val) names
                (xBody', tBody') <- convertExp guts names' xBody
                return  ( G.Lam gv $ G.Lam gv_val xBody'
                        , G.mkFunTy (G.varType gv) tBody')

        D.XLAM _ (D.BName dn _) xBody
         |  Just (GhcNameVar gv) <- Map.lookup dn names
         -> do  (xBody', tBody') <- convertExp guts names xBody
                return  ( G.Lam gv xBody'
                        , G.mkFunTy (G.varType gv) tBody')

        -- Function abstractions.
        D.XLam _ (D.BName dn dt) xBody
         -> do  (names1, gv)     <- getExpBind names (D.BAnon dt)       
                                        -- TODO: BRUTAL fresh name generator
                let names'       = Map.insert dn (GhcNameVar gv) names1
                (xBody', tBody') <- convertExp guts names' xBody
                return  ( G.Lam gv xBody'
                        , G.mkFunTy (G.varType gv) tBody')


        -- Application of a polymorphic primitive.
        -- In GHC core, functions cannot be polymorphic in unlifted primitive
        -- types. We convert most of the DDC polymorphic prims in a uniform way.
        D.XApp _ (D.XVar _ (D.UPrim n _)) (D.XType t)
         ->     convertPolyPrim guts names n t


        -- General applications.
        D.XApp _ x1 x2
         -> do  (x1', t1')      <- convertExp guts names x1
                (x2', _)        <- convertExp guts names x2
                return  ( G.App x1' x2'
                        , t1')  -- WRONG

        -- Case expressions
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [ bWorld@(D.BName{})
                                     ,     b2]) x1]
         -> do  
                (xScrut', _)       <- convertExp guts names xScrut

                (names1,  vWorld') <- getExpBind names  bWorld
                (names',  v2')     <- getExpBind names1 b2

                (x1',     t1')     <- convertExp guts names' x1

                return ( G.Case xScrut' vWorld' t1'
                                [ (G.DataAlt G.unboxedPairDataCon, [vWorld', v2'], x1') ]
                       , t1')

        -- Type arguments.
        D.XType t
         -> do  let t'          = convertType names t
                return  ( G.Type t'
                        , G.wordTy )    -- WRONG

        _ -> convertExp guts names (D.xNat () 666)



