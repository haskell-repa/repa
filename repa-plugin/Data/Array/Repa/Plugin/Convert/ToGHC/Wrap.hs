
module Data.Array.Repa.Plugin.Convert.ToGHC.Wrap
        (wrapLowered)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import Data.Array.Repa.Plugin.GHC.Pretty ()
import DDC.Base.Pretty

import qualified CoreSyn                as G
import qualified DataCon                as G
import qualified Type                   as G
import qualified TypeRep                as G
import qualified TysPrim                as G
import qualified TysWiredIn             as G
import qualified MkId                   as G
import qualified UniqSupply             as G


-- | Make a wrapper to call a lowered version of a function from the original
--   binding. We need to unsafely pass it the world token, as well as marshall
--   between boxed and unboxed types.
wrapLowered 
        :: G.Type                       -- ^ Type of original version.
        -> G.Type                       -- ^ Type of lowered  version.
        -> [Either G.Var G.CoreExpr]    -- ^ Lambda bound variables in wrapper.
        -> G.Var                        -- ^ Name of lowered version.
        -> G.UniqSM G.CoreExpr

wrapLowered tOrig tLowered vsParam vLowered
        -- Decend into foralls.
        --  Bind the type argument with a new var so we can pass it to 
        --  the lowered function.
        | G.ForAllTy vOrig tOrig'       <- tOrig
        , G.ForAllTy _     tLowered'    <- tLowered
        = do    let vsParam'    = Left vOrig : vsParam
                xBody   <- wrapLowered tOrig' tLowered' vsParam' vLowered
                return  $  G.Lam vOrig xBody


        -- If the type of the lowered function says it needs 
        -- the realworld token, then just give it one.
        --  This effectively unsafePerformIOs it.
        | G.FunTy    tLowered1  tLowered2   <- tLowered
        , G.TyConApp tcState _              <- tLowered1
        , tcState == G.statePrimTyCon
        = do    let vsParam'    = Right (G.Var G.realWorldPrimId) : vsParam
                wrapLowered tOrig tLowered2 vsParam' vLowered


        -- Decend into functions.
        --  Bind the argument with a new var so we can pass it to the lowered
        --  function.
        | G.FunTy tOrig1      tOrig2    <- tOrig
        , G.FunTy _tLowered1  tLowered2 <- tLowered
        = do    v'              <- newDummyVar "arg" tOrig1
                let vsParam'    = Right (G.Var v') : vsParam
                xBody           <- wrapLowered tOrig2 tLowered2 vsParam' vLowered
                return  $  G.Lam v' xBody


        -- We've decended though all the foralls and lambdas and now need
        -- to call the actual lowered function, and marshall its result.
        | otherwise
        = do    -- Arguments to pass to the lowered function.
                let xsArg       = map   (either (G.Type . G.TyVarTy) id) 
                                        vsParam

                -- Actual call to the lowered function.
                let xLowered    = foldl G.App (G.Var vLowered) $ reverse xsArg

                callLowered tOrig tLowered xLowered


-- | Make the call site for the lowered function.
callLowered
        :: G.Type               -- ^ Type of result for original unlowered version.
        -> G.Type               -- ^ Type of result for lowered version.
        -> G.CoreExpr           -- ^ Exp that calls the lowered version.
        -> G.UniqSM G.CoreExpr

callLowered tOrig tLowered xLowered

        -- Assume this function returns a (# World#, a #)               -- TODO: check this.
        | G.TyConApp _ [_tWorld, tVal]  <- tLowered
        = do
                vScrut  <- newDummyVar "scrut"  tLowered
                vWorld  <- newDummyVar "world"  G.realWorldStatePrimTy
                vResult <- newDummyVar "result" tOrig
                vVal    <- newDummyVar "val"    tVal

                -- Unwrap the actual result value.
                let tOrigVal    = tOrig
                let tLoweredVal = tVal
                xResult <- unwrapResult tOrigVal tLoweredVal (G.Var vVal) 

                return  $ G.Case xLowered vScrut tOrig 
                                [ (G.DataAlt G.unboxedPairDataCon
                                        , [vWorld, vVal]
                                        , xResult) ]

        | otherwise
        = error "repa-plugin.Wrap.callLowered: no match"


unwrapResult 
        :: G.Type               -- ^ Type of result for original unlowered version.
        -> G.Type               -- ^ Type of result for lowered version.
        -> G.CoreExpr           -- ^ Expression for result value.
        -> G.UniqSM G.CoreExpr

unwrapResult tOrig tLowered xResult

        | G.TyConApp tcInt []    <- tOrig
        , tcInt == G.intTyCon
        , G.TyConApp tcIntU []   <- tLowered    
                        -- TODO: do a proper check. Is this supposed to be a TyLit? 

        = return $ G.App (G.Var (G.dataConWorkId G.intDataCon)) xResult

        | otherwise
        = error $ "repa-plugin.ToGHC.unwrapResult: don't know how to unwrap this."
                ++ " " ++ (renderIndent $ ppr tLowered)
