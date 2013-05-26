
module Data.Array.Repa.Plugin.ToGHC
        (spliceModGuts)
where
import Data.Array.Repa.Plugin.ToGHC.Wrap
import Data.Array.Repa.Plugin.ToGHC.Type
import Data.Array.Repa.Plugin.ToGHC.Prim
import Data.Array.Repa.Plugin.ToGHC.Var
import Data.Array.Repa.Plugin.Primitives
import Data.Array.Repa.Plugin.FatName

import qualified BasicTypes             as G
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
import qualified DDC.Base.Pretty        as D

import Data.List
import Control.Monad
import Data.Map                         (Map)
import qualified Data.Map               as Map
import Data.Maybe                       (catMaybes)


-------------------------------------------------------------------------------
-- | Splice bindings from a DDC module into a GHC core program.
--
--   If the GHC module contains a top-level binding that map onto a binding
--   in the DDC module then add the converted DDC binding to the GHC module
--   and patch the original GHC binding to call it.
--
spliceModGuts
        :: Primitives           -- ^ Table of Repa primitives
        -> Map D.Name GhcName   -- ^ Maps DDC names to GHC names.
        -> D.Module () D.Name   -- ^ DDC module.
        -> G.ModGuts            -- ^ GHC module guts.
        -> G.UniqSM G.ModGuts

spliceModGuts primitives names mm guts
 = do   
        -- Invert the map so it maps GHC names to DDC names.
        let names'      = Map.fromList 
                        $ map (\(x, y) -> (y, x)) 
                        $ Map.toList names

        binds'  <- liftM concat 
                $  mapM (spliceBind primitives guts names names' mm) 
                $  G.mg_binds guts

        return  $ guts { G.mg_binds = binds' }


-- Splice ---------------------------------------------------------------------
-- | If a GHC core binding has a matching one in the provided DDC module
--   then convert the DDC binding from GHC core and use that instead.
spliceBind 
        :: Primitives
        -> G.ModGuts
        -> Map D.Name  GhcName
        -> Map GhcName D.Name
        -> D.Module () D.Name
        -> G.CoreBind
        -> G.UniqSM [G.CoreBind]

-- If there is a matching binding in the Disciple module then use that.
spliceBind primitives guts names names' mm (G.NonRec gbOrig _)
 | Just nOrig                  <- Map.lookup (GhcNameVar gbOrig) names'
 , Just (dbLowered, dxLowered) <- lookupModuleBindOfName mm nOrig
 = do   
        -- starting environments.
        -- let imported            = importedNamesOfGuts guts

        let kenv = Env
                 { envGuts       = guts
                 , envPrimitives = primitives
                 , envNames      = names
                 , envVars       = [] }

        let tenv = Env
                 { envGuts       = guts
                 , envPrimitives = primitives
                 , envNames      = names
                 , envVars       = [] }

        -- make a new binding for the lowered version.
        let dtLowered   = D.typeOfBind dbLowered
        gtLowered       <- convertType kenv dtLowered
        gvLowered       <- newDummyVar "lowered" gtLowered       -- TODO: base on orig name.

        -- Convert the lowered version from DDC to GHC core.
        (gxLowered, _)  <- convertExp kenv tenv dxLowered

        -- Call the lowered version from the original, adding a wrapper
        --  to (unsafely) pass the world token and marshal boxed to
        --  unboxed values.
        xCall           <- wrapLowered 
                                (G.varType gbOrig) gtLowered
                                [] 
                                gvLowered

        return  [ G.NonRec gvLowered gxLowered
                , G.NonRec gbOrig  xCall ]
                        -- TODO: ensure the NOINLINE pragma is attached so we know
                        --       the faked realWorld token will never be substituted.

-- Otherwise leave the original GHC binding as it is.
spliceBind _ _ _ _ _ b
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
        --  If they're not in envVars, they may be imported
        --  functions in envNames.
        D.XVar _ (D.UName dn)
         -> case lookup dn (envVars tenv) of
                Nothing 
                 | Just (GhcNameVar gv) <- Map.lookup dn (envNames tenv)
                 -> return (G.Var gv, G.varType gv)
                Nothing
                 -> error $ unlines 
                          [ "repa-plugin.ToGHC.convertExp: variable " 
                                     ++ show dn ++ " not in scope"
                          , "env = " ++ show (map fst $ envVars tenv) ]
                Just gv
                 -> return ( G.Var gv
                           , G.varType gv)

        -- Non-polytypic primops.
        D.XVar _ (D.UPrim n _)
         |  not $ isPolytypicPrimName n
         ->     convertPrim kenv tenv n


        -- The unboxed tuple constructor.
        -- When we produce unboxed tuple we always want to preserve
        -- the unboxed versions of element types.
        D.XApp _ x1 x2
         | (D.XCon _ (D.DaCon dn _ _), args)                   <- D.takeXApps1 x1 x2
         , D.DaConNamed (D.NameDaConFlow (D.DaConFlowTuple n)) <- dn
         -- The first n arguments are type parameters, the rest are values
         , (tyxs, vals)                                        <- splitAt n args
         , tys                                                 <- catMaybes (map D.takeXType tyxs)
         -- Types must be fully applied, but we can get away with
         -- only partial value application
         , length tys  == n
         -> do  tys'    <- mapM (convertType_unboxed kenv)      tys
                vals'   <- mapM (convertExp          kenv tenv) vals

                let dacon    = G.tupleCon G.UnboxedTuple n
                -- Find type of tuple constructor, instantiate the foralls
                let gt       = G.varType (G.dataConWorkId dacon)
                let gt'      = G.applyTys gt tys'
                -- Get the result of the function type after applying the arguments in vals
                let (_,tRes) = G.splitFunTysN (length vals) gt'

                return  ( G.mkConApp dacon (map G.Type tys' ++ map fst vals')
                        , tRes )


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


        -- Application of a polytypic primitive.
        -- In GHC core, functions cannot be polymorphic in unlifted primitive
        -- types. We convert most of the DDC polymorphic prims in a uniform way.
        D.XApp _ (D.XApp _ (D.XVar _ (D.UPrim n _)) (D.XType t1)) (D.XType t2)
         |  isPolytypicPrimName n
         ->     convertPolytypicPrim kenv tenv n [t1, t2]

        D.XApp _ (D.XVar _ (D.UPrim n _)) (D.XType t)
         |  isPolytypicPrimName n
         ->     convertPolytypicPrim kenv tenv n [t]


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
                                , ppr x1
                                , ppr x2 ]

                return  ( G.App x1' x2'
                        , tResult)

        -- Non-recursive let bindings
        D.XLet _ (D.LLet _ b x1) x2
         -> do  (xScrut', _)      <- convertExp kenv tenv x1
                (tenv',  vBind')  <- bindVarX   kenv tenv b
                (x2',    t2')     <- convertExp kenv tenv' x2

                return  ( G.Case xScrut' vBind' t2'
                                [ ( G.DEFAULT, [], x2') ]
                        , t2')


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


        -- Case expressions over n-tuples
        D.XCase _ xScrut 
                 [ D.AAlt (D.PData dacon binders) x1]
         | D.DaCon dn _ _                                      <- dacon
         , D.DaConNamed (D.NameDaConFlow (D.DaConFlowTuple n)) <- dn
         , length binders == n
         -> do  
                (xScrut', tScrut')  <- convertExp kenv tenv xScrut
                vScrut'             <- newDummyVar "scrut" tScrut'

                let goBind (tenv', vs) b
                     = do   (tenv'', v) <- bindVarX kenv tenv' b
                            return (tenv'', v:vs)

                (tenv',vs)         <- foldM goBind (tenv,[]) binders
                (x1',  t1')        <- convertExp kenv tenv' x1

                return ( G.Case xScrut' vScrut' t1'
                                [ (G.DataAlt (G.tupleCon G.UnboxedTuple n)
                                , reverse vs, x1') ]
                       , t1')

        _ -> errorNoConversion xx


-- Errors ---------------------------------------------------------------------
errorNoConversion xx
 = error $ D.renderIndent $ D.vcat
 $      [ D.text "repa-plugin.ToGHC: cannot convert this to GHC Core"
        , D.empty
        , D.indent 8 $ D.ppr xx ]




