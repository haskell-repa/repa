
module Data.Array.Repa.Plugin.GHC.ToDDC
        (slurpModGuts)
where
import DDC.Base.Pretty

import qualified HscTypes               as G
import qualified Avail                  as G
import qualified CoreSyn                as G
import qualified Type                   as G
import qualified TypeRep                as G
import qualified TyCon                  as G
import qualified Coercion               as G
import qualified Var                    as G
import qualified OccName                as OccName
import qualified Name                   as Name
import qualified DataCon                as G
import qualified Literal                as G
import qualified Id                     as G
import qualified Unique                 as G
import qualified FastString             as G
import qualified UniqFM                 as UFM

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Module         as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Compounds as D

import qualified Data.Map               as Map
import Data.Maybe
import Data.List

-------------------------------------------------------------------------------
data GhcName
        = GhcNameVar     G.Var
        | GhcNameTyCon   G.TyCon
        | GhcNameTyLit   G.TyLit
        | GhcNameLiteral G.Literal

        | GhcNameIntU   
        deriving Eq


data FatName
        = FatName
        { fatNameGHC    :: GhcName
        , fatNameDDC    :: D.Name }
        deriving Eq

instance Pretty FatName where
 ppr (FatName _ name)   = ppr name


-------------------------------------------------------------------------------
-- | Slurp out interesting parts of a GHC module.
--
--   Not every top-level binding is considered "interesting", and expressions
--   involving type casts cannot be converted.
--
slurpModGuts :: G.ModGuts -> D.Module () FatName
slurpModGuts guts
        = D.ModuleCore
        { D.moduleName          = D.ModuleName ["Foo"]
        , D.moduleExportKinds   = Map.empty
        , D.moduleExportTypes   = Map.empty
        , D.moduleImportKinds   = Map.empty
        , D.moduleImportTypes   = Map.empty

        , D.moduleBody          
                = D.xLets () 
                          (slurpTopBinds $ G.mg_binds guts) 
                          (D.xUnit ()) 
        }


-- Names ----------------------------------------------------------------------
-- | Slurp a FatName from a GHC variable.
slurpFatName :: G.Var -> FatName
slurpFatName var
        = FatName (GhcNameVar var) (slurpVarName var)


-- | Slurp a printable DDC name from a GHC variable.
slurpVarName :: G.Var -> D.Name
slurpVarName var
        = slurpName (G.varName var)


-- | Slurp a DDC name from a GHC name.
slurpName :: Name.Name -> D.Name
slurpName name
 = let  baseName = text
                 $ OccName.occNameString
                 $ Name.nameOccName name

        unique   = text
                 $ show $ Name.nameUnique name

   in   D.NameVar
         $ renderPlain (baseName <> text "_" <> unique)


-- Variables ------------------------------------------------------------------
-- | Slurp a type from a GHC variable.
slurpVarType :: G.Var -> D.Type FatName
slurpVarType v
        = slurpType $ G.varType v


-- Bindings -------------------------------------------------------------------
-- | Slurp top-level bindings.
slurpTopBinds :: [G.CoreBind] -> [D.Lets () FatName]
slurpTopBinds bnds
        = mapMaybe slurpTopBind bnds


-- | Slurp a possibly recursive top-level binding.
slurpTopBind :: G.CoreBind -> Maybe (D.Lets () FatName)
slurpTopBind bnd
 = case bnd of
        G.NonRec b x      
         -> do  (b', x') <- slurpBinding (b, x)
                return   $  D.LLet D.LetStrict b' x'

        G.Rec bxs
         -> do  bxs'     <- mapM slurpBinding bxs
                return   $  D.LRec bxs'


-- | Slurp a single binding.
slurpBinding 
        :: (G.CoreBndr,     G.CoreExpr)
        -> Maybe (D.Bind FatName, D.Exp () FatName)

slurpBinding (b, x)
 | D.NameVar str  <- slurpVarName b
 , isPrefixOf "repa" str                -- TODO: select the bindings we care about
                                        --       more generally.
 = do   x'      <- slurpExpr x
        return  ( D.BName (slurpFatName b) (slurpVarType b)
                , x')

 | otherwise
 = Nothing


-- Type -----------------------------------------------------------------------
-- | Slurp a type.
slurpType :: G.Type -> D.Type FatName
slurpType tt
 = case tt of
        G.TyVarTy v
         -> D.TVar    (D.UName (slurpFatName v)) 

        G.AppTy t1 t2
         -> D.TApp    (slurpType t1) (slurpType t2)

        G.TyConApp tc ts
         -> D.tApps   (D.TCon (slurpTyCon tc)) (map slurpType ts)

        G.FunTy t1 t2
         -> D.tFunPE  (slurpType t1) (slurpType t2)

        G.ForAllTy v t
         -> D.TForall (D.BName (slurpFatName v) D.kData)
                      (slurpType t)

        G.LitTy (G.NumTyLit _) 
         -> error "repa-plugin.slurpType: numeric type literals not handled."

        G.LitTy tyLit@(G.StrTyLit fs)
         -> D.TVar  (D.UName (FatName (GhcNameTyLit tyLit)
                                      (D.NameCon (G.unpackFS fs))))


-- | Slurp a tycon
slurpTyCon :: G.TyCon -> D.TyCon FatName
slurpTyCon tc
        | G.isFunTyCon tc
        = D.TyConSpec D.TcConFun

        | otherwise
        = D.TyConBound
                (D.UName (FatName (GhcNameTyCon tc)
                         (slurpName $ G.tyConName tc)))
                (D.kData)                               -- TODO: WRONG


-- Expr -----------------------------------------------------------------------
-- | Slurp an expression.
--   TODO: Give warnings if we can't convert an expression.
slurpExpr :: G.CoreExpr 
          -> Maybe (D.Exp () FatName)
slurpExpr xx
 = case xx of
        G.Var v
         ->     return $ D.XVar () (D.UName (slurpFatName v))

        G.Lit l
         ->     return $ D.XCon () (slurpLiteral l)

        G.App x1 x2
         -> do  x1'     <- slurpExpr x1
                x2'     <- slurpExpr x2
                return  $ D.XApp () x1' x2'

        G.Lam b x
         -> do  x'      <- slurpExpr x
                let b'   = D.BName  (slurpFatName b) (slurpVarType b)
                return  $  D.XLam () b' x'

        G.Let (G.NonRec b x1) x2
         -> do  let b'  = D.BName (slurpFatName b) (slurpVarType b)
                x1'     <- slurpExpr x1
                x2'     <- slurpExpr x2
                return  $  D.XLet () (D.LLet D.LetStrict b' x1') x2'

        G.Case{}
         -> error "repa-plugin.slurpExpr: case not handled yet"

        -- We don't handle bindings containing type casts.
        G.Cast{}        -> Nothing

        -- Just ditch tick nodes, we probably don't need them.
        G.Tick _ x
         -> slurpExpr x

        -- Type arguments.
        G.Type t
         -> return $ D.XType (slurpType t)

        -- We don't handle coersions.
        G.Coercion{}    -> Nothing


-- Literals -------------------------------------------------------------------
-- | Slurp a literal.
slurpLiteral :: G.Literal -> D.DaCon FatName
slurpLiteral lit
 = case lit of
        G.MachInt i 
          -> D.mkDaConAlg (FatName (GhcNameLiteral lit) (D.NameLitInt i)) 
                          tIntU'

        _ -> error "repa-plugin.slurpLiteral: can't convert literal"


tIntU' =  D.TCon 
        $ D.TyConBound 
                (D.UPrim  (FatName GhcNameIntU (D.NamePrimTyCon D.PrimTyConInt))
                          D.kData)
                D.kData


