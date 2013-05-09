
module Data.Array.Repa.Plugin.Primitives
        ( Primitives (..)
        , slurpPrimitives)
where
import Data.Array.Repa.Plugin.ToGHC.Var
import Data.List
import Data.Maybe
import Control.Monad

import qualified HscTypes       as G
import qualified CoreSyn        as G
import qualified MkCore         as G
import qualified DataCon        as G
import qualified TyCon          as G
import qualified Type           as G
import qualified Var            as G
import qualified OccName        as Occ
import qualified Name           as Name
import UniqSupply               as G


-------------------------------------------------------------------------------
-- | Table of GHC core expressions to use to invoke the primitives
--   needed by the lowering transform.
data Primitives
        = Primitives
        { prim_Series           :: !G.Type
        , prim_Vector           :: !G.Type
        , prim_addInt           :: (G.CoreExpr, G.Type)
        , prim_mulInt           :: (G.CoreExpr, G.Type)
        , prim_newIntVector     :: (G.CoreExpr, G.Type)
        , prim_readIntVector    :: (G.CoreExpr, G.Type)
        , prim_writeIntVector   :: (G.CoreExpr, G.Type)
        , prim_loop             :: (G.CoreExpr, G.Type)
        , prim_rateOfSeries     :: (G.CoreExpr, G.Type)
        , prim_nextInt          :: (G.CoreExpr, G.Type)
        }


-------------------------------------------------------------------------------
-- | Try to slurp the primitive table from a GHC module.
slurpPrimitives 
        :: G.ModGuts 
        -> UniqSM (Maybe Primitives)

slurpPrimitives guts
 = do   results <- mapM slurpTopBind $ G.mg_binds guts
        return  $ listToMaybe $ catMaybes results


-- | Try to slurp the primitive table from some top level bindings.
slurpTopBind 
        :: G.CoreBind 
        -> UniqSM (Maybe Primitives)

slurpTopBind bnd
 = case bnd of
        G.Rec{}         -> return Nothing
        G.NonRec b _x   -> slurpBinding b


-- | Try to slurp the primitive table from a single top-level binding
--   named "repa_primitives".
slurpBinding 
        :: G.CoreBndr 
        -> UniqSM (Maybe Primitives)

slurpBinding b
        | strName      <- Occ.occNameString 
                       $  Name.nameOccName 
                       $  G.varName b
        , strName == "repa_primitives"
        = slurpTable b

        | otherwise
        = return Nothing


-- | Slurp the primitive table from the the top-level 
--   @repa_primitive@ binding in the module to be transformed.
slurpTable 
        :: G.Var 
        -> UniqSM (Maybe Primitives)

slurpTable v
 | t                      <- G.varType v
 , Just tc                <- G.tyConAppTyCon_maybe t
 , G.isAlgTyCon tc
 , G.DataTyCon [dc] False <- G.algTyConRhs tc
 = do
        let labels
                = G.dataConFieldLabels dc

        -- Load types from their proxy fields.
        let Just tySeries   
                = liftM (G.dataConFieldType dc)
                $ find (\n -> stringOfName n ==  "prim_Series") labels

        let Just tyVector   
                = liftM (G.dataConFieldType dc)
                $ find (\n -> stringOfName n ==  "prim_Vector") labels

        -- Generic combinators.
        expr_loop               <- makeFieldProjection v "prim_loop"
        expr_rateOfSeries       <- makeFieldProjection v "prim_rateOfSeries"

        -- Int functions.
        expr_addInt             <- makeFieldProjection v "prim_addInt"
        expr_mulInt             <- makeFieldProjection v "prim_mulInt"
        expr_newIntVector       <- makeFieldProjection v "prim_newIntVector"
        expr_readIntVector      <- makeFieldProjection v "prim_readIntVector"
        expr_writeIntVector     <- makeFieldProjection v "prim_writeIntVector"
        expr_nextInt            <- makeFieldProjection v "prim_nextInt"


        return $ Just $ Primitives
         { prim_Series          = tySeries
         , prim_Vector          = tyVector
 
         , prim_loop            = expr_loop
         , prim_rateOfSeries    = expr_rateOfSeries
 
         , prim_addInt          = expr_addInt
         , prim_mulInt          = expr_mulInt
         , prim_newIntVector    = expr_newIntVector
         , prim_readIntVector   = expr_readIntVector
         , prim_writeIntVector  = expr_writeIntVector
         , prim_nextInt         = expr_nextInt }

 | otherwise
 = return Nothing


-------------------------------------------------------------------------------
-- | Build a CoreExpr that produces the primtive with the given name.
makeFieldProjection
        :: G.Var                -- ^ Core variable bound to our primtiive table.
        -> String               -- ^ Name of the primitive we want.
        -> UniqSM (G.CoreExpr, G.Type)

makeFieldProjection v strField
 | t                      <- G.varType v
 , Just tc                <- G.tyConAppTyCon_maybe t
 , G.isAlgTyCon tc
 , G.DataTyCon [dc] False <- G.algTyConRhs tc
 = do   
        -- Lookup the Name for the field we want.
        let labels      = G.dataConFieldLabels dc
        let Just field  = find (\n -> stringOfName n == strField) labels
        makeFieldProjection' dc field (G.Var v) (G.varType v)

 | otherwise
 = error "repa-plugin.makeFieldProjection: malformed primitive table"


makeFieldProjection'
        :: G.DataCon            -- ^ Data constructor for the primitive table.
        -> G.FieldLabel         -- ^ Name of the field to project out.
        -> G.CoreExpr           -- ^ Expression to produce the table.
        -> G.Type               -- ^ Type of the table.
        -> UniqSM (G.CoreExpr, G.Type)

makeFieldProjection' dc labelWanted xTable tTable
 = do   
        -- Make binders to match all fields,
        --      including one for the field we want.
        (bsAll, vWanted) <- makeFieldBinders dc labelWanted

        -- The type of the wanted field.
        let tResult      =  G.dataConFieldType dc labelWanted

        return  ( G.mkWildCase xTable tTable tResult
                        [ (G.DataAlt dc, bsAll, G.Var vWanted)]
                , tResult)


-- | Make a sequence of binders 
makeFieldBinders 
        :: G.DataCon               -- ^ Data constructor for the primtiive table.
        -> G.FieldLabel            -- ^ The field we want to project out.
        -> UniqSM ([G.Var], G.Var) -- ^ All binders, and the one for our desired field.

makeFieldBinders dc labelWanted
 = do   let tWanted =  G.dataConFieldType dc labelWanted
        vWanted     <- newDummyVar "wanted" tWanted
        let bsAll   =  go vWanted (G.dataConFieldLabels dc)
        return  (bsAll, vWanted)

 where  go _       []   = []
        go vWanted (l:ls)
         | l == labelWanted 
         = vWanted
                : go vWanted ls

         | otherwise        
         = (G.mkWildValBinder $ G.dataConFieldType dc l)
                : go vWanted ls


-- Utils ----------------------------------------------------------------------
-- | Convert a GHC name to a string
stringOfName :: Name.Name -> String
stringOfName name
 = Occ.occNameString $ Name.nameOccName name



