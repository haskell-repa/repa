
module Data.Array.Repa.Plugin.Convert.ToGHC.Wrap
        (wrapLowered)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TypeRep                 as G
import qualified TysPrim                 as G
import qualified MkId                    as G
import qualified UniqSupply              as G


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

                -- TODO: wrap in a case and unpack the result.
                return xLowered
