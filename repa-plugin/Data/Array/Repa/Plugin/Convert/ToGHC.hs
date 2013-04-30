
module Data.Array.Repa.Plugin.Convert.ToGHC
        (spliceModGuts)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.FatName
import Control.Monad
import Data.List
import Data.Map                         (Map)

import qualified HscTypes                as G
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TypeRep                 as G
import qualified TysPrim                 as G
import qualified TysWiredIn              as G
import qualified IdInfo                  as G
import qualified Var                     as G
import qualified DataCon                 as G
import qualified Literal                 as G
import qualified PrimOp                  as G
import qualified UniqSupply              as G
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

        binds'  <- mapM (spliceBind guts names names' mm) 
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
        -> G.UniqSM G.CoreBind

-- If there is a matching binding in the Disciple module then use that.
spliceBind guts names names' mm (G.NonRec b _)
 | Just dn        <- Map.lookup (GhcNameVar b) names'
 , Just (_db, dx) <- lookupModuleBindOfName mm dn
 = do   (x', _)   <- convertExp guts names dx
        return  $ G.NonRec b x'

-- Otherwise leave the original GHC binding as it is.
spliceBind _ _ _ _ b
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


-- PolyPrim -------------------------------------------------------------------
convertPolyPrim 
        :: G.ModGuts
        -> Map D.Name GhcName
        -> D.Name -> D.Type D.Name 
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPolyPrim guts names n tArg
 = case n of
        D.NamePrimArith D.PrimArithAdd
         -> do  Just gv <- getPrim_add guts tArg
                return  (G.Var gv, G.varType gv)

        D.NamePrimArith D.PrimArithMul
         -> do  Just gv <- getPrim_mul guts tArg
                return  (G.Var gv, G.varType gv)

        D.NameOpStore D.OpStoreNext 
         | Just  gv     <- getPrim_next guts tArg
         ->     return  (G.Var gv, G.varType gv)

        D.NameOpStore D.OpStoreReadArray
         -> do  Just gv <- getPrim_readByteArrayOpM guts tArg
                return  (G.Var gv, G.varType gv)

        D.NameOpFlow D.OpFlowLengthOfRate
         |  D.TVar (D.UName (D.NameVar str)) <- tArg
         , Just gv              <- findImportedPrimVar guts "primLengthOfRate"
         , Just (GhcNameVar vk) <- Map.lookup (D.NameVar $ str ++ "_val")  names 
                        -- HACKS!. Store a proper mapping between rate vars
                        --         and their singleton types.
         -> return ( G.App (G.Var gv) (G.Var vk)
                   , convertType names D.tInt)

        _
         -> error $ "repa-plugin.toGHC.convertPolyPrim: no match for " ++ show n

getPrim_add _ t
 | t == D.tInt  = liftM Just $ getPrimOpVar G.IntAddOp
 | otherwise    = return Nothing

getPrim_mul _ t
 | t == D.tInt  = liftM Just $ getPrimOpVar G.IntMulOp
 | otherwise    = return Nothing

getPrim_next guts t
 | t == D.tInt  = findImportedPrimVar guts "primNext_Int"
 | otherwise    = Nothing

getPrim_writeByteArrayOpM _ t
 | t == D.tInt  = liftM Just $ getPrimOpVar G.WriteByteArrayOp_Int
 | otherwise    = return Nothing

getPrim_readByteArrayOpM _ t
 | t == D.tInt  = liftM Just $ getPrimOpVar G.ReadByteArrayOp_Int
 | otherwise    = return Nothing


-- Exp ------------------------------------------------------------------------
-- | Get the GHC expression var corresponding to some DDC name.
getExpBind 
        :: Map D.Name GhcName 
        -> D.Bind D.Name
        -> G.UniqSM (Map D.Name GhcName, G.Var)

getExpBind names b
 -- DDC name was created from a GHC name originally, or is the name of some
 -- primitive thing, so we can use the known GHC name for it.
 | D.BName dn _         <- b
 , Just (GhcNameVar gv) <- Map.lookup dn names
 = return (names, gv)

 -- DDC name was created during program transformation, so we need to create
 -- a new GHC name for it.
 | otherwise
 = do   let tName   = D.typeOfBind b

        let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName "x"
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let ty      = convertType names tName
        let info    = G.vanillaIdInfo
        let gv      = G.mkLocalVar details name ty info

        let names'  = case b of
                        D.BName dn _ -> Map.insert dn (GhcNameVar gv) names
                        _            -> names

        return  ( names', gv)


-- Variable utils -------------------------------------------------------------
ghcVarOfPrimName :: G.ModGuts -> D.Name -> G.UniqSM G.Var
ghcVarOfPrimName guts n
 = case n of
        D.NameOpLoop D.OpLoopLoop
         |  Just gv     <- findImportedPrimVar guts "primLoop"
         -> return gv
        _       
         -> error $ "repa-plugin.ToGHC.ghcVarOfPrimName: no match for " ++ show n


-- | Find the variable with this name from the source module.
--   TODO: right now it must be defined in the current module,
--         fix this to actually look in the import list.
findImportedPrimVar :: G.ModGuts -> String -> Maybe G.Var
findImportedPrimVar guts str
 = go (G.mg_binds guts)
 where  
        go (G.NonRec v _ : bs)
         | plainNameOfVar v == str = Just v
         | otherwise               = go bs

        go ( (G.Rec ((v, _) : vxs)) : bs)
         | plainNameOfVar v == str = Just v
         | otherwise               = go (G.Rec vxs : bs)

        go ( G.Rec [] : bs )       = go bs

        go []                      = Nothing


-- | Take the plain unqualified printable name of a GHC variable.
plainNameOfVar :: G.Var -> String
plainNameOfVar gv
 = let  name    = G.varName gv
        occ     = Name.nameOccName name
   in   OccName.occNameString occ


-- | Convert a GHC primop to a variable.
getPrimOpVar :: G.PrimOp -> G.UniqSM G.Var
getPrimOpVar op
 = do   let details = G.PrimOpId   op
        let occName = G.primOpOcc  op
        let ty      = G.primOpType op
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkGlobalVar details name ty info


-- | Create a fresh dummy GHC variable with the given type.
newDummyVar :: String -> G.Type -> G.UniqSM G.Var
newDummyVar basename ty
 = do   let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName basename
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkLocalVar details name ty info




