
module Data.Array.Repa.Plugin.ToGHC.Type
        ( convertType
        , convertType_boxed
        , convertType_unboxed

        , convertBoxed
        , convertUnboxed
        , Env(..)
        , bindVarT
        , bindVarX)
where
import Data.Array.Repa.Plugin.ToGHC.Var
import Data.Array.Repa.Plugin.Primitives
import Data.Array.Repa.Plugin.FatName
import Data.Map                         (Map)

import qualified BasicTypes              as G
import qualified HscTypes                as G
import qualified Type                    as G
import qualified TypeRep                 as G
import qualified TysPrim                 as G
import qualified TysWiredIn              as G
import qualified TyCon                   as G
import qualified UniqSupply              as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Compounds as D
import qualified DDC.Base.Pretty         as D

import qualified Data.Map                as Map


-- Boxed/Unboxed versions -----------------------------------------------------
convertType_boxed
        :: Env
        -> D.Type D.Name
        -> G.UniqSM G.Type

convertType_boxed env tt
 = case convertBoxed tt of
        Just t' -> return t'
        _       -> convertType env tt


convertType_unboxed
        :: Env
        -> D.Type D.Name
        -> G.UniqSM G.Type

convertType_unboxed env tt
 = case convertUnboxed tt of
        Just t' -> return t'
        _       -> convertType env tt


-- Type -----------------------------------------------------------------------
convertType 
        :: Env
        -> D.Type D.Name 
        -> G.UniqSM G.Type

convertType kenv tt
 = case tt of
        -- DDC[World#]    => GHC[State# RealWorld#]
        --   The GHC state token takes a phantom type to indicate
        --   what state thread it corresponds to.
        D.TCon (D.TyConBound (D.UPrim (D.NameTyConFlow D.TyConFlowWorld) _) _)
         -> return $ G.mkTyConApp G.statePrimTyCon [G.realWorldTy]

        -- DDC[Vector# a] => GHC[Vector# {Lifted a}]
        --   In the code we get from the lowering transform, for element
        --   types like Int# the "hash" refers to the fact that it is
        --   primitive, and not nessesarally unboxed. The type arguments 
        --   for 'Series' in GHC land need to be the boxed/lifted versions.
        D.TApp{}
         | Just (D.NameTyConFlow D.TyConFlowVector,  [tElem])
                <- D.takePrimTyConApps tt
         , Just tElem'  <- convertBoxed tElem
         -> do  return  $ G.applyTy  (prim_Vector (envPrimitives kenv)) 
                                     tElem'

        -- DDC[Ref# a] => GHC[Ref {Lifted a}]
        D.TApp{}
         | Just (D.NameTyConFlow D.TyConFlowRef, [tElem])
                <- D.takePrimTyConApps tt
         , Just tElem'  <- convertBoxed tElem
         -> do  return  $ G.applyTy  (prim_Ref (envPrimitives kenv))
                                     tElem'

        -- DDC[Series# k a] => GHC[Series k {Lifted a}]
        D.TApp{}
         | Just (D.NameTyConFlow D.TyConFlowSeries, [tK, tElem])
                <- D.takePrimTyConApps tt
         , Just tElem'  <- convertBoxed tElem
         -> do  tK'     <- convertType  kenv tK
                return  $ G.applyTys (prim_Series (envPrimitives kenv)) 
                                     [tK', tElem']

        -- DDC[Data] => GHC[*]
        D.TCon (D.TyConKind D.KiConData)
         -> return $ G.liftedTypeKind

        -- DDC[Rate] => GHC[*]
        D.TCon (D.TyConBound (D.UPrim (D.NameKiConFlow D.KiConFlowRate) _) _)
         -> return $ G.liftedTypeKind


        -- Generic Conversion -------------------
        D.TForall b t
         -> do  (kenv', gv)     <- bindVarT kenv b
                t'              <- convertType kenv' t
                return  $  G.mkForAllTy gv t'

        -- Function types.
        D.TApp{}
         | Just (t1, t2)        <- D.takeTFun tt
         -> do  t1'     <- convertType kenv t1
                t2'     <- convertType kenv t2
                return  $  G.mkFunTy t1' t2'

        -- Applied type constructors.
        D.TApp{}
         | Just (tc, tsArgs)      <- D.takeTyConApps tt
         -> do  tsArgs'   <- mapM (convertType kenv) tsArgs
                tsArgs_b' <- mapM (convertType_boxed kenv) tsArgs
                return $ convertTyConApp 
                                (envPrimitives kenv) (envNames kenv) 
                                tc tsArgs' tsArgs_b'

        D.TCon tc
         ->     return $ convertTyConApp 
                                (envPrimitives kenv) (envNames kenv) 
                                tc [] []

        D.TVar (D.UName n)
         -> case lookup n (envVars kenv) of
                Nothing
                 -> error $ unlines 
                          [ "repa-plugin.ToGHC.convertType: variable " 
                                     ++ show n ++ " not in scope"
                          , "env = " ++ show (map fst $ envVars kenv) ]

                Just gv  
                 -> return $ G.TyVarTy gv


        _ -> error $ "repa-plugin.convertType: no match for " ++ show tt


-- TyConApp -------------------------------------------------------------------
-- | Covnert a type constructor application.
--
--   Note that our baked-in types Series and Vector are handled by
--   convertType instead.
--
--   We require in the unboxed and boxed argument types:
--      user-defined types require boxed.
convertTyConApp 
        :: Primitives
        -> Map D.Name GhcName
        -> D.TyCon D.Name
        -> [G.Type]             -- ^ Normal (unboxed?) argument types
        -> [G.Type]             -- ^ Boxed argument types
        -> G.Type

convertTyConApp _prims names tc tsArgs' tsArgs_b'
 = case tc of
        -- Functions
        D.TyConSpec D.TcConFun
         |  [t1, t2] <- tsArgs'
         -> G.FunTy t1 t2

        -- Unit
        D.TyConSpec D.TcConUnit
         |  []       <- tsArgs'
         -> G.unitTy

        -- Tuples
        D.TyConBound (D.UPrim (D.NameTyConFlow (D.TyConFlowTuple n)) _) _
         |  length tsArgs' == n
         -> G.mkTyConApp (G.tupleTyCon G.UnboxedTuple n) tsArgs'

        -- RateNat
        D.TyConBound (D.UPrim n@(D.NameTyConFlow D.TyConFlowRateNat) _) _
         | Just (GhcNameTyCon tc') <- Map.lookup n names
         -> G.mkTyConApp tc' tsArgs'

        -- Machine types
        D.TyConBound (D.UPrim n _) _
         |  []       <- tsArgs'
         ,  Just tc'               <- convertTyConPrimName n
         -> G.mkTyConApp tc' tsArgs'

        -- User-defined types: use boxed arguments
        D.TyConBound (D.UName n) _
         | Just (GhcNameTyCon tc') <- Map.lookup n names
         -> G.mkTyConApp tc' tsArgs_b'

        -- Couldn't convert this type constructor application.
        _ -> error $ "repa-plugin.convertTyConApp: no match for " 
                   ++ show tc 


-- TyCon ----------------------------------------------------------------------
-- | Convert a Flow type constructor name to a GHC type constructor.
convertTyConPrimName :: D.Name -> Maybe G.TyCon
convertTyConPrimName n
 = case n of
        D.NamePrimTyCon D.PrimTyConBool         -> Just G.boolTyCon
        D.NamePrimTyCon D.PrimTyConNat          -> Just G.wordPrimTyCon
        D.NamePrimTyCon D.PrimTyConInt          -> Just G.intPrimTyCon
        D.NamePrimTyCon (D.PrimTyConFloat 32)   -> Just G.floatPrimTyCon
        D.NamePrimTyCon (D.PrimTyConFloat 64)   -> Just G.doublePrimTyCon
        _                                       -> Nothing


-------------------------------------------------------------------------------
-- | Get the GHC boxed type corresponding to this Flow series element type.
convertBoxed :: D.Type D.Name -> Maybe G.Type
convertBoxed t
 | t == D.tNat          = Just G.wordTy
 | t == D.tInt          = Just G.intTy
 | t == D.tFloat 32     = Just G.floatTy
 | t == D.tFloat 64     = Just G.doubleTy

 | Just (tc,args) <- D.takeTyConApps t
 , D.TyConBound (D.UPrim (D.NameTyConFlow (D.TyConFlowTuple n)) _) _
                  <- tc
 , Just args'     <- mapM convertBoxed args
 = Just $ G.mkTyConApp (G.tupleTyCon G.BoxedTuple n) args'

 | otherwise            = Nothing


-- | Get the GHC unboxed type corresponding to this Flow series element type.
convertUnboxed :: D.Type D.Name -> Maybe G.Type
convertUnboxed t
 | t == D.tNat          = Just G.wordPrimTy
 | t == D.tInt          = Just G.intPrimTy
 | t == D.tFloat 32     = Just G.floatPrimTy
 | t == D.tFloat 64     = Just G.doublePrimTy
 | otherwise            = Nothing


-- Env ------------------------------------------------------------------------
-- | Environment used to map DDC names to GHC names.
--   Used when converting DDC Core to GHC core.
data Env
        = Env 
        { -- | Guts of the original GHC module.
          envGuts       :: G.ModGuts

          -- | Table of Repa primitives
        , envPrimitives :: Primitives

          -- | Name map we got during the original GHC -> DDC conversion.
        , envNames      :: Map D.Name GhcName

          -- | Locally scoped variables.
        , envVars       :: [(D.Name, G.Var)]
        }


-- | Bind a fresh GHC variable for a DDC expression variable.
bindVarX :: Env -> Env -> D.Bind D.Name -> G.UniqSM (Env, G.Var)
bindVarX kenv tenv (D.BName n t)
 = do   gt        <- convertType kenv t
        let str   =  D.renderPlain (D.ppr n)
        gv        <- newDummyVar str gt
        let tenv' =  tenv { envVars     = (n, gv) : envVars tenv }
        return   (tenv', gv)

bindVarX kenv tenv (D.BNone t)
 = do   gt      <- convertType kenv t
        gv      <- newDummyVar "x" gt
        return  (tenv, gv)

bindVarX _ _ b
        = error $ "repa-plugin.ToGHC.bindVarX: can't bind " ++ show b



-- | Bind a fresh GHC type variable for a DDC type variable.
bindVarT :: Env -> D.Bind D.Name -> G.UniqSM (Env, G.Var)
bindVarT kenv (D.BName n _)
 = do   let str   =  D.renderPlain (D.ppr n)
        gv        <- newDummyTyVar str 
        let kenv' =  kenv { envVars     = (n, gv) : envVars kenv }
        return  (kenv', gv)

bindVarT _ b
        = error $ "repa-plugin.ToGHC.bindVarT: can't bind " ++ show b

