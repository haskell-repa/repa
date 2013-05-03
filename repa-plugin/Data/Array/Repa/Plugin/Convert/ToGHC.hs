
module Data.Array.Repa.Plugin.Convert.ToGHC
        (spliceModGuts)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Wrap
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.ToGHC.Prim
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import Data.Array.Repa.Plugin.Convert.FatName

import qualified HscTypes               as G
import qualified CoreSyn                as G
import qualified Type                   as G
import qualified TypeRep                as G
import qualified TysPrim                as G
import qualified TysWiredIn             as G
import qualified Var                    as G
import qualified DataCon                as G
import qualified Literal                as G
import qualified UniqSupply             as G

import DDC.Base.Pretty
import qualified DDC.Core.Exp           as D
import qualified DDC.Core.Module        as D
import qualified DDC.Core.Compounds     as D
import qualified DDC.Core.Flow          as D
import qualified DDC.Core.Flow.Prim     as D
import qualified DDC.Core.Flow.Compounds as D

import Data.List
import Control.Monad
import Data.Map                         (Map)
import qualified Data.Map               as Map



-------------------------------------------------------------------------------
-- | Splice bindings from a DDC module into a GHC core program.
--
--   If the GHC module contains a top-level binding that map onto a binding
--   in the DDC module then add the converted DDC binding to the GHC module
--   and patch the original GHC binding to call it.
--
spliceModGuts
        :: Map D.Name GhcName   -- ^ Maps DDC names to GHC names.
        -> D.Module () D.Name   -- ^ DDC module.
        -> G.ModGuts            -- ^ GHC module guts.
        -> G.UniqSM G.ModGuts

spliceModGuts names mm guts
 = do   
        -- Invert the map so it maps GHC names to DDC names.
        let names'      = Map.fromList 
                        $ map (\(x, y) -> (y, x)) 
                        $ Map.toList names

        binds'  <- liftM concat 
                $  mapM (spliceBind guts names names' mm) 
                $  G.mg_binds guts

        return  $ guts { G.mg_binds = binds' }


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
        -- starting environments.
        let kenv = Env
                { envGuts       = guts
                , envNames      = names
                , envVars       = [] }

        let tenv = Env
                 { envGuts      = guts
                 , envNames     = names
                 , envVars      = [] }

        -- make a new binding for the lowered version.
        let dtLowered   = D.typeOfBind dbLowered
        gtLowered       <- convertType kenv dtLowered
        gvLowered       <- newDummyVar "lowered" gtLowered       -- TODO: base on orig name.

        -- Convert the lowered version from DDC to GHC core.
        (gxLowered, _)  <- convertExp kenv tenv dxLowered

        -- Call the lowered version from the original, adding a wrapper
        --  to (unsafely) pass the world token and marshal boxed to
        --  unboxed values.
        xCall   <- wrapLowered 
                        (G.varType gbOrig) gtLowered
                        [] 
                        gvLowered

        return  [ G.NonRec gvLowered gxLowered
                , G.NonRec gbOrig  xCall ]
                        -- TODO: ensure the NOINLINE pragma is attached so we know
                        --       the faked realWorld token will never be substituted.

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
        :: Env -> Env
        -> D.Exp () D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertExp kenv tenv xx
 = case xx of
        -- Variables.
        --  Names of plain variables should be in the name map, and refer
        --  other top-level bindings, or dummy variables that we've
        --  introduced locally in this function.
        D.XVar _ (D.UName dn)
         -> case lookup dn (envVars tenv) of
                Nothing 
                 -> error $ unlines 
                          [ "repa-plugin.ToGHC.convertExp: variable " 
                                     ++ show dn ++ " not in scope"
                          , "env = " ++ show (map fst $ envVars tenv) ]
                Just gv
                 -> return ( G.Var gv
                           , G.varType gv)


        -- The unboxed tuple constructor.
        -- When we produce unboxed tuple we always want to preserve
        -- the unboxed versions of element types.
        D.XApp _ (D.XApp _ (D.XCon _ (D.DaCon dn _ _)) (D.XType t1)) (D.XType t2)
         | D.DaConNamed (D.NameDaConFlow (D.DaConFlowTuple 2)) <- dn
         -> do  t1'     <- convertType_unboxed kenv t1
                t2'     <- convertType_unboxed kenv t2

                let gv  = G.dataConWorkId G.unboxedPairDataCon
                let gt  = G.varType gv

                return  ( G.App (G.App (G.Var gv) (G.Type t1')) (G.Type t2')
                        , G.applyTys gt [t1', t2'] )


        -- Data constructors.                           
        D.XCon _ (D.DaCon dn _ _)
         -> case dn of                                          -- TODO: shift into Prim module.
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

                -- Don't know how to convert this.
                _ -> error $ "repa-plugin.ToGHC.convertExp: "
                           ++ "Cannot convert DDC data constructor " 
                                ++ show xx ++ " to GHC Core."


        -- Type abstractions.
        D.XLAM _ b@(D.BName{}) xBody
         -> do  
                (kenv',  gv)     <- bindVarT   kenv b
                (xBody', tBody') <- convertExp kenv' tenv xBody

                return  ( G.Lam gv xBody'
                        , G.mkForAllTy gv tBody')


        -- Function abstractions.
        D.XLam _ b@(D.BName{}) xBody
         -> do  
                (tenv',  gv)     <- bindVarX   kenv tenv b
                (xBody', tBody') <- convertExp kenv tenv' xBody

                return  ( G.Lam gv  xBody'
                        , G.mkFunTy (G.varType gv) tBody')


        -- Non-polytypic primops.
        D.XVar _ (D.UPrim n _)
         |  not $ isPolytypicPrimName n
         ->     convertPrim kenv tenv n

        
        -- Application of a polytypic primitive.
        -- In GHC core, functions cannot be polymorphic in unlifted primitive
        -- types. We convert most of the DDC polymorphic prims in a uniform way.
        D.XApp _ (D.XVar _ (D.UPrim n _)) (D.XType t)
         |  isPolytypicPrimName n
         ->     convertPolytypicPrim kenv tenv n t


        -- Value/Type applications.
        D.XApp _ x1 (D.XType t2)
         -> do  (x1', t1')      <- convertExp        kenv tenv x1
                t2'             <- convertType_boxed kenv t2

                let tResult
                     = case t1' of
                        G.ForAllTy{}    
                          -> G.applyTy t1' t2'

                        _ -> error 
                          $  renderIndent $ vcat
                                [ text $ "repa-plugin.ToGHC.convertExp: "
                                        ++ "type error during conversion."
                                , ppr x1
                                , ppr t2 ]

                return  ( G.App x1' (G.Type t2')
                        , tResult)

        -- Value/Value applications.
        D.XApp _ x1 x2
         -> do  (x1', t1')      <- convertExp kenv tenv x1
                (x2', _)        <- convertExp kenv tenv x2

                let tResult 
                     = case t1' of
                        G.FunTy    _ t12'  
                          -> t12'

                        _ -> error 
                           $ renderIndent $ vcat
                                [ text $ "repa-plugin.ToGHC.convertExp: "
                                        ++ "type error during conversion."
                                , text (show x1)
                                , text (show x2) ]

                return  ( G.App x1' x2'
                        , tResult)


        -- Case expresions, with a single binder.
        --  assume these are 1-tuples                           -- TODO: check really 1-tuples.
        D.XCase _ xScrut
                 [ D.AAlt (D.PData _ [ bWorld ]) x1]
         -> do
                (xScrut', _)       <- convertExp kenv tenv xScrut

                (tenv',   vWorld') <- bindVarX kenv tenv  bWorld
                (x1',     t1')     <- convertExp kenv tenv' x1

                return  ( G.Case xScrut' vWorld' t1'
                                [ (G.DEFAULT, [], x1') ]
                        , t1')


        -- Case expressions, with two binders.
        --  assume these are 2-tuples                           -- TODO: check really 2-tuples.
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData _ [ bWorld@(D.BName{})
                                     ,     b2]) x1]
         -> do  
                (xScrut', tScrut')  <- convertExp kenv tenv xScrut
                vScrut'             <- newDummyVar "scrut" tScrut'

                (tenv1,    vWorld') <- bindVarX kenv tenv  bWorld
                (tenv2,    v2')     <- bindVarX kenv tenv1 b2
                let tenv' = tenv2

                (x1',     t1')      <- convertExp kenv tenv' x1

                return ( G.Case xScrut' vScrut' t1'
                                [ (G.DataAlt G.unboxedPairDataCon
                                , [vWorld', v2'], x1') ]
                       , t1')


        _ -> error "repa-plugin.ToGHC.convertExp: no conversion."

