
-- TODO: Turn the generic conversion stuff into a separate module.
--       Make it easier to convert between GHC and DDC.
module Data.Array.Repa.Plugin.Convert.ToDDC
        (convertModGuts)
where
import Data.Array.Repa.Plugin.Convert.FatName
import DDC.Base.Pretty
import Data.Maybe
import Data.List
import Data.Char

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
import qualified DDC.Core.Flow.Prim      as D
import qualified DDC.Core.Flow.Compounds as D

import qualified Data.Map               as Map


-------------------------------------------------------------------------------
-- | Convert a GHC module to Disciple Core Flow.
--
--   This is a raw conversion of the AST. We still need to detect the primitive
--   flow operators before we can run the lowering pass.
--
--   Top-level bindings containing type casts cannot be converted because 
--   Disciple Core has no way to represent them. We shouldn't run across any
--   of these in the code we want to lower.
--
--    TODO: give a warning for unconverted bindings.
--
convertModGuts :: G.ModGuts -> D.Module () FatName
convertModGuts guts
        = D.ModuleCore
        { D.moduleName          = D.ModuleName ["Foo"]
        , D.moduleExportKinds   = Map.empty
        , D.moduleExportTypes   = Map.empty
        , D.moduleImportKinds   = Map.empty
        , D.moduleImportTypes   = Map.empty

        , D.moduleBody          
                = D.xLets () 
                          (convertTopBinds $ G.mg_binds guts) 
                          (D.xUnit ()) 
        }


-- Names ----------------------------------------------------------------------
-- | Convert a FatName from a GHC variable.
convertFatName :: G.Var -> FatName
convertFatName var
        = FatName (GhcNameVar var) (convertVarName var)


-- | Convert a printable DDC name from a GHC variable.
convertVarName :: G.Var -> D.Name
convertVarName var
        = convertName (G.varName var)


-- | Convert a DDC name from a GHC name.
convertName :: Name.Name -> D.Name
convertName name
 = let  baseName = OccName.occNameString
                 $ Name.nameOccName name

        unique   = show $ Name.nameUnique name
        str      = renderPlain (text baseName <> text "_" <> text unique)

   in   case baseName of
         []         -> error "repa-plugin.convertName: base name is empty"
         c : cs 
          | isUpper c   -> D.NameCon str
          | otherwise   -> D.NameVar str


-- Variables ------------------------------------------------------------------
-- | Convert a type from a GHC variable.
convertVarType :: G.Var -> D.Type FatName
convertVarType v
        = convertType $ G.varType v


-- Bindings -------------------------------------------------------------------
-- | Convert top-level bindings.
convertTopBinds :: [G.CoreBind] -> [D.Lets () FatName]
convertTopBinds bnds
        = mapMaybe convertTopBind bnds


-- | Convert a possibly recursive top-level binding.
convertTopBind :: G.CoreBind -> Maybe (D.Lets () FatName)
convertTopBind bnd
 = case bnd of
        G.NonRec b x      
         -> do  (b', x') <- convertBinding (b, x)
                return   $  D.LLet D.LetStrict b' x'

        G.Rec bxs
         -> do  bxs'     <- mapM convertBinding bxs
                return   $  D.LRec bxs'


-- | Convert a single binding.
convertBinding 
        :: (G.CoreBndr,     G.CoreExpr)
        -> Maybe (D.Bind FatName, D.Exp () FatName)

convertBinding (b, x)
 | D.NameVar str  <- convertVarName b
 , isPrefixOf "repa" str                                -- TODO: select the bindings we care about
                                                        --       more generally.
 = do   x'      <- convertExpr x
        return  ( D.BName (convertFatName b) (convertVarType b)
                , x')

 | otherwise
 = Nothing


-- Type -----------------------------------------------------------------------
-- | Convert a type.
convertType :: G.Type -> D.Type FatName
convertType tt
 = case tt of
        G.TyVarTy v
         -> D.TVar    (D.UName (convertFatName v)) 

        G.AppTy t1 t2
         -> D.TApp    (convertType t1) (convertType t2)

        G.TyConApp tc ts
         -> D.tApps   (D.TCon (convertTyCon tc)) (map convertType ts)

        G.FunTy t1 t2
         -> D.tFunPE  (convertType t1) (convertType t2)

        G.ForAllTy v t
         -> D.TForall (D.BName (convertFatName v) D.kData)
                      (convertType t)

        G.LitTy (G.NumTyLit _) 
         -> error "repa-plugin.slurpType: numeric type literals not handled."

        G.LitTy tyLit@(G.StrTyLit fs)
         -> D.TVar  (D.UName (FatName (GhcNameTyLit tyLit)
                                      (D.NameCon (G.unpackFS fs))))


-- | Convert a tycon.
convertTyCon :: G.TyCon -> D.TyCon FatName
convertTyCon tc
        | G.isFunTyCon tc
        = D.TyConSpec D.TcConFun

        | otherwise
        = D.TyConBound
                (D.UName (FatName (GhcNameTyCon tc)
                         (convertName $ G.tyConName tc)))
                (D.kData)                                                       -- TODO: WRONG


-- Expr -----------------------------------------------------------------------
-- | Slurp an expression.
--   TODO: Give warnings if we can't convert an expression.
convertExpr :: G.CoreExpr 
          -> Maybe (D.Exp () FatName)
convertExpr xx
 = case xx of
        G.Var v
         ->     return $ D.XVar () (D.UName (convertFatName v))

        G.Lit l
         ->     return $ D.XCon () (convertLiteral l)

        G.App x1 x2
         -> do  x1'     <- convertExpr x1
                x2'     <- convertExpr x2
                return  $ D.XApp () x1' x2'

        G.Lam b x
         -> do  x'      <- convertExpr x
                let b'   = D.BName  (convertFatName b) (convertVarType b)
                return  $  D.XLam () b' x'

        G.Let (G.NonRec b x1) x2
         -> do  let b'  = D.BName (convertFatName b) (convertVarType b)
                x1'     <- convertExpr x1
                x2'     <- convertExpr x2
                return  $  D.XLet () (D.LLet D.LetStrict b' x1') x2'

        -- TODO: convert cases.
        G.Case{}
         -> error "repa-plugin.slurpExpr: case not handled yet"

        -- We don't handle bindings containing type casts.
        G.Cast{}        -> Nothing

        -- Just ditch tick nodes, we probably don't need them.
        G.Tick _ x      -> convertExpr x

        -- Type arguments.
        G.Type t        -> return $ D.XType (convertType t)

        -- We can't convert coercions.
        G.Coercion{}    -> Nothing


-- Literals -------------------------------------------------------------------
-- | Slurp a literal.
convertLiteral :: G.Literal -> D.DaCon FatName
convertLiteral lit
 = case lit of
        G.MachInt i 
          -> D.mkDaConAlg (FatName (GhcNameLiteral lit) (D.NameLitInt i)) 
                          tIntU'

        -- TODO: convert the rest of the literals.
        _ -> error "repa-plugin.slurpLiteral: can't convert literal"


tIntU' =  D.TCon 
        $ D.TyConBound 
                (D.UPrim  (FatName GhcNameIntU (D.NamePrimTyCon D.PrimTyConInt))
                          D.kData)
                D.kData


