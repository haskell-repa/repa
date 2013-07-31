
module Data.Array.Repa.Plugin.ToGHC.Wrap
        ( wrapLowered
        , unwrapResult)
where
import Data.Array.Repa.Plugin.ToGHC.Var
import Data.Array.Repa.Plugin.GHC.Pretty ()

import qualified BasicTypes             as G
import qualified CoreSyn                as G
import qualified DataCon                as G
import qualified Type                   as G
import qualified TypeRep                as G
import qualified TysPrim                as G
import qualified TysWiredIn             as G
import qualified MkId                   as G
import qualified UniqSupply             as G
import Control.Monad


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
        , G.FunTy tLowered1  tLowered2 <- tLowered
        = do    v'              <- newDummyVar "arg" tOrig1
                -- Convert from type 'tOrig1' to 'tLowered1'
                arg'            <- unwrapResult tLowered1 tOrig1 (G.Var v')
                let vsParam'    = Right arg' : vsParam
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

        -- Assume this function returns a (# World#, ts.. #)               -- TODO: check this.
        | G.TyConApp _ (_tWorld : tsVal)  <- tLowered
        = do
                vScrut  <- newDummyVar "scrut"  tLowered
                vWorld  <- newDummyVar "world"  G.realWorldStatePrimTy
                vsVal   <- zipWithM (\i t -> newDummyVar ("val" ++ show i) t)
                                [0 :: Int ..] tsVal

                -- Unwrap the actual result value.
                let tOrigVal     = tOrig
                let tsLoweredVal = tsVal
                xResult         <- unwrapResultBits 
                                        tOrigVal 
                                        tsLoweredVal 
                                        (map G.Var vsVal)

                return  $ G.Case xLowered vScrut tOrig 
                                [ (G.DataAlt (G.tupleCon G.UnboxedTuple (1 + length tsVal))
                                        , (vWorld : vsVal)
                                        , xResult) ]

        | otherwise
        = error "repa-plugin.Wrap.callLowered: no match"


unwrapResultBits
        :: G.Type               -- ^ Type of result for original version.
        -> [G.Type]             -- ^ Types of arguments lowered arguments
        -> [G.CoreExpr]         -- ^ Types of components
        -> G.UniqSM G.CoreExpr

unwrapResultBits tOrig tsBits xsBits
        | [tBit]                <- tsBits
        , [xBit]                <- xsBits
        = unwrapResult tOrig tBit xBit 

        | G.TyConApp tcTup tsOrig <- tOrig
        , n                       <- length tsOrig
        , G.tupleTyCon G.BoxedTuple n   == tcTup
        = do    
                xsResult        <- mapM (\(tOrig', tLowered, xBit) 
                                        -> unwrapResult tOrig' tLowered xBit)
                                $  zip3 tsOrig tsBits xsBits

                return $ G.mkConApp (G.tupleCon G.BoxedTuple n)
                                    (map G.Type tsOrig ++ xsResult)

        | otherwise
        = error "unwrapResultBits: failed"


unwrapResult 
        :: G.Type               -- ^ Type of result for original unlowered version.
        -> G.Type               -- ^ Type of result for lowered version.
        -> G.CoreExpr           -- ^ Expression for result value.
        -> G.UniqSM G.CoreExpr

unwrapResult tOrig tLowered xResult

        -- Wrap Ints
        | G.TyConApp tcInt  []    <- tOrig,      tcInt  == G.intTyCon
        , G.TyConApp tcIntU []    <- tLowered,   tcIntU == G.intPrimTyCon
        = return $ G.App (G.Var (G.dataConWorkId G.intDataCon)) xResult

        -- Wrap Floats
        | G.TyConApp tcFloat  []  <- tOrig,      tcFloat  == G.floatTyCon
        , G.TyConApp tcFloatU []  <- tLowered,   tcFloatU == G.floatPrimTyCon
        = return $ G.App (G.Var (G.dataConWorkId G.floatDataCon)) xResult

        -- Wrap Doubles
        | G.TyConApp tcDouble  [] <- tOrig,      tcDouble  == G.doubleTyCon
        , G.TyConApp tcDoubleU [] <- tLowered,   tcDoubleU == G.doublePrimTyCon
        = return $ G.App (G.Var (G.dataConWorkId G.doubleDataCon)) xResult


        -- Unwrap Ints
        | G.TyConApp tcIntU []   <- tOrig
        , tcIntU == G.intPrimTyCon
        , G.TyConApp tcInt  []   <- tLowered    
        , tcInt  == G.intTyCon
        = do    vScrut  <- newDummyVar "scrut" tLowered
                v       <- newDummyVar "v"     tOrig
                return  $ G.Case xResult vScrut tOrig
                        [ (G.DataAlt G.intDataCon, [v], G.Var v)]



        -- Original is a boxed tuple and lowered version is unboxed:
        -- raise to a boxed tuple, boxing its elements too.
        | G.TyConApp tcTup tins          <- tOrig
        , G.TyConApp tcUnb touts         <- tLowered    
        , n                              <- length tins
        , G.tupleTyCon G.BoxedTuple   n  == tcTup
        , G.tupleTyCon G.UnboxedTuple n  == tcUnb
        = do
            -- Case on the unboxed tuple, raise the elements, then create a boxed tuple
            vScrut <- newDummyVar "scrut" tLowered
            vs     <- mapM (newDummyVar "v") touts

            let unwrap (t,t',v)
                    = unwrapResult t t' (G.Var v)

            xs     <- mapM unwrap (zip3 tins touts vs)

            return (G.Case xResult vScrut tOrig
                    [ (G.DataAlt (G.tupleCon G.UnboxedTuple n)
                    , vs,
                        G.mkConApp (G.tupleCon G.BoxedTuple n)
                         (map G.Type tins ++ xs))])

        -- Convert boxed tuple to unboxed, maybe unbox its elements too
        | G.TyConApp tcUnb tins          <- tOrig
        , G.TyConApp tcTup touts         <- tLowered    
        , n                              <- length tins
        , G.tupleTyCon G.UnboxedTuple n  == tcUnb
        , G.tupleTyCon G.BoxedTuple   n  == tcTup
        = do
            -- Case on the unboxed tuple, raise the elements, then create a boxed tuple
            vScrut <- newDummyVar "scrut" tLowered
            vs     <- mapM (newDummyVar "v") touts

            let unwrap (t,t',v)
                    = unwrapResult t t' (G.Var v)

            xs     <- mapM unwrap (zip3 tins touts vs)

            return (G.Case xResult vScrut tOrig
                    [ (G.DataAlt (G.tupleCon G.BoxedTuple n)
                    , vs,
                        G.mkConApp (G.tupleCon G.UnboxedTuple n)
                         (map G.Type tins ++ xs))])


        | otherwise
        = return xResult

