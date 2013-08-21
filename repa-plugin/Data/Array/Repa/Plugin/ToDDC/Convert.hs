
module Data.Array.Repa.Plugin.ToDDC.Convert
        (convertModGuts)
where
import Data.Array.Repa.Plugin.ToDDC.Convert.Base
import Data.Array.Repa.Plugin.ToDDC.Convert.Type
import Data.Array.Repa.Plugin.ToDDC.Convert.Var
import Data.Array.Repa.Plugin.FatName
import Control.Monad
import Data.Either
import Data.List
import           Data.Map                 (Map)
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Module         as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Collect        as D
import qualified DDC.Type.Env            as D

import qualified CoreSyn                 as G
import qualified DataCon                 as G
import qualified HscTypes                as G
import qualified TyCon                   as G
import qualified Type                    as G
import qualified Var                     as G

import qualified OccName                as OccName
import qualified Name                   as Name


-------------------------------------------------------------------------------
-- | Convert a GHC module to Disciple Core Flow.
--
--   This is a raw conversion of the AST. We still need to detect the primitive
--   flow operators before we can run the lowering pass.
--
--   We get back a Disciple Core Flow module containing all the top-level
--   bindings that we could convert, and a list of reasons why conversion 
--   for the other bindings failed.
--
convertModGuts 
        :: G.ModGuts 
        -> (D.Module () FatName, [Fail])

convertModGuts guts
 = let  (bnds', fails)  
                = convertTopBinds $ G.mg_binds guts
        body    = D.xLets () bnds' (D.xUnit ())

        -- Find the free variables in the module body
        freeX   = D.freeX D.empty body

        -- And add them all to the import types
        importT = foldl (insertImport convertType) Map.empty
                $ Set.toList freeX

        -- Then find the type constructors mentioned in the imports
        freeT   = Set.unions 
                $ map (D.supportTyCon . D.support D.empty D.empty . snd . snd) 
                $ Map.toList importT

        -- And add them to the import kinds
        importK = foldl (insertImport convertKind) Map.empty
                $ Set.toList freeT

        mm'     = D.ModuleCore
                { D.moduleName          = D.ModuleName ["Flow"]
                , D.moduleExportKinds   = Map.empty
                , D.moduleExportTypes   = Map.empty
                , D.moduleImportKinds   = importK
                , D.moduleImportTypes   = importT
                , D.moduleBody          = body }

   in   (mm', fails)


-- | Convert a type/kind and add it to the import map, if conversion succeeds.
insertImport :: (G.Type -> Either Fail (D.Type FatName))
             -> Map FatName (D.QualName FatName, D.Type FatName)
             -> D.Bound FatName
             -> Map FatName (D.QualName FatName, D.Type FatName)
insertImport c m bound
 | D.UName n@(FatName ghc _) <- bound
 , GhcNameVar v              <- ghc
 = ins n (c $ G.varType v)

 | D.UName n@(FatName ghc _) <- bound
 , GhcNameTyCon tc           <- ghc
 = ins n (c $ G.tyConKind tc)

 | otherwise
 = m
 where
  ins _ (Left _)  = m
  ins n (Right t) = Map.insert n (D.QualName (D.ModuleName []) n, t) m


-- Bindings -------------------------------------------------------------------
-- | Convert top-level bindings.
convertTopBinds 
        :: [G.CoreBind] 
        -> ([D.Lets () FatName], [Fail])

convertTopBinds bnds
 = let  results         = map convertTopBind bnds
        (fails, bnds')  = partitionEithers results
   in   (bnds', fails)


-- | Convert a possibly recursive top-level binding.
convertTopBind 
        :: G.CoreBind 
        -> Either Fail (D.Lets () FatName)

convertTopBind bnd
 = case bnd of
        G.NonRec b x      
         -> case convertBinding (b, x) of
                Left fails      -> Left   $ FailInBinding b fails
                Right (b', x')  -> return $ D.LLet b' x'

        G.Rec bxs
         -> do  ns'     <- mapM (convertFatName.fst) bxs
                ts'     <- mapM (convertVarType.fst) bxs
                xs'     <- mapM (convertExpr   .snd) bxs
                let bxs' = zip (zipWith D.BName ns' ts') xs'
                return  $  D.LRec bxs'



-- | Convert a single top-level binding.
--   The binding must be named "lower_something"
convertBinding 
        :: (G.CoreBndr, G.CoreExpr)
        -> Either Fail (D.Bind FatName, D.Exp () FatName)

convertBinding (b, x)
 = do   n       <- convertVarName b
        case n of
         D.NameVar str
           | isPrefixOf "lower" str
           -> do x'      <- convertExpr x
                 fn'     <- convertFatName b
                 t'      <- convertVarType b
                 return  $ (D.BName fn' t', x')

           | otherwise
           -> Left FailNotMarked

         _ -> Left (FailDodgyTopLevelBindingName n)


-- Expr -----------------------------------------------------------------------
-- | Slurp an expression.
convertExpr :: G.CoreExpr 
            -> Either Fail (D.Exp () FatName)

convertExpr xx
 = case xx of
        G.Var v
         -> do  name'   <- convertFatName v
                return  $ D.XVar () (D.UName name')

        G.Lit lit
         -> do  lit'    <- convertLiteral lit
                return  $ D.XCon () lit'

        G.App x1 x2
         -> do  x1'     <- convertExpr x1
                x2'     <- convertExpr x2
                return  $ D.XApp () x1' x2'

        G.Lam b x
         -> do  x'      <- convertExpr x
                n'      <- convertFatName b
                t'      <- convertVarType b
                return  $  D.XLam () (D.BName n' t') x'

        G.Let (G.NonRec b x1) x2
         -> do  n'      <- convertFatName b
                t'      <- convertVarType b
                x1'     <- convertExpr x1
                x2'     <- convertExpr x2
                return  $  D.XLet () (D.LLet (D.BName n' t') x1') x2'

        G.Let (G.Rec bxs) x
         -> do  ns'     <- mapM (convertFatName.fst) bxs
                ts'     <- mapM (convertVarType.fst) bxs
                xs'     <- mapM (convertExpr   .snd) bxs
                let bxs' = zip (zipWith D.BName ns' ts') xs'
                x'      <- convertExpr x
                return  $  D.XLet () (D.LRec bxs') x'


        -- Case expression matching against a unit.
        --
        --    case EXP1 of { () -> EXP2 }
        -- => let _ = EXP1 in EXP2
        --
        --   The target langauge is strict, so we don't need the extra
        --   demand on EXP1 from the pattern match.
        --
        G.Case x1 _b _tres [(G.DataAlt dc, [], x2)]
         | isPrefixOf "()"
                $ OccName.occNameString
                $ Name.nameOccName 
                $ G.dataConName dc
         -> do  x1'     <- convertExpr  x1
                x2'     <- convertExpr  x2

                return  $ D.XLet () (D.LLet (D.BNone D.tUnit) x1')
                                    x2'

        -- Simple case expressions with just DataAlts
        G.Case x b _tres alts
         -> do  b'      <- convertFatName b
                t'      <- convertVarType b
                x'      <- convertExpr    x
                alts'   <- mapM convertAlt alts

                -- Case
                return $ D.XLet  () (D.LLet (D.BName b' t') x')
                       $ D.XCase () (D.XVar () (D.UName b')) alts'


        -- We can't represent type casts,
        -- so just drop them on the floor.
        G.Cast x _      -> convertExpr x                        


        -- Just ditch tick nodes, we probably don't need them.
        G.Tick _ x      -> convertExpr x


        -- Type arguments.
        G.Type t        -> liftM D.XType (convertType t)


        -- Cannot convert coercions.
        G.Coercion{}    -> Left FailNoCoercions


-- Convert a case alternative
convertAlt :: G.Alt G.Var -> Either Fail (D.Alt () FatName)
convertAlt (con, bs, x)
 = do   ns' <- mapM convertFatName bs
        ts' <- mapM convertVarType bs
        x'  <-      convertExpr    x
        case con of
         G.DEFAULT 
          ->    return $ D.AAlt D.PDefault x'

         G.DataAlt dc
          -> do nm <- convertName $ G.dataConName    dc
                ty <- convertType $ G.dataConRepType dc
                let binds = zipWith D.BName ns' ts'
                let fat   = FatName (GhcNameTyCon $ G.promoteDataCon dc) nm

                -- It must be algebraic, since we are casing on it.
                let pat   = D.PData (D.mkDaConAlg fat ty) binds
                return $ D.AAlt pat x'

         G.LitAlt _ 
          ->    Left FailUnhandledCase

