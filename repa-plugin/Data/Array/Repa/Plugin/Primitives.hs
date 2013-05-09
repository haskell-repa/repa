
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
import qualified UniqSet        as US


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
--
--   The table should be in a top-level binding named "repa_primitives".
--   If we find it, then we add more top-level functions to the module 
--   that select the individual primitives, then build a table of expressions
--   that can be used to access them.
--
slurpPrimitives 
        :: G.ModGuts 
        -> UniqSM (Maybe (Primitives, G.ModGuts))

slurpPrimitives guts
 | Just vTable  <- listToMaybe 
                $  mapMaybe findTableFromTopBind 
                $  G.mg_binds guts
 = do   
        Just (prims, bsMoar) <- makeTable vTable

        let hackedGuts          
                = guts  
                { G.mg_binds    
                        = insertAfterTable bsMoar 
                        $ G.mg_binds guts
                
                , G.mg_used_names       
                        = US.addListToUniqSet (G.mg_used_names guts)
                        $ [G.varName b | G.NonRec b _ <- bsMoar ]}

        return  $ Just (prims, hackedGuts)

 | otherwise
 =      return Nothing
        

-------------------------------------------------------------------------------
-- | Try to find the primitive table in this top level binding.
findTableFromTopBind :: G.CoreBind -> Maybe G.Var
findTableFromTopBind bnd
 = case bnd of
        G.Rec{}         -> Nothing
        G.NonRec b _    -> findTableFromBinding b


-- | Try to find the primitive table in this top level binding.
--   It needs to be named "repa_primitives"
findTableFromBinding :: G.CoreBndr -> Maybe G.Var
findTableFromBinding b
        | strName      <- Occ.occNameString 
                       $  Name.nameOccName 
                       $  G.varName b
        , strName == "repa_primitives"
        = Just b

        | otherwise
        = Nothing


-------------------------------------------------------------------------------
-- | Insert some top-level bindings after the primitive table.
insertAfterTable :: [G.CoreBind] -> [G.CoreBind] -> [G.CoreBind]
insertAfterTable bsMore bs
 = case bs of
        []                      
         -> bs
        
        bb@G.Rec{} : bs'         
         -> bb : insertAfterTable bsMore bs'
        
        bb@(G.NonRec b _) : bs'
         |  isJust $ findTableFromBinding b
         -> bb : bsMore ++ bs'

         | otherwise
         -> bb : insertAfterTable bsMore bs'


-------------------------------------------------------------------------------
-- | Create top-level projection functions based on the primitive table
--   attached to this variable.
makeTable 
        :: G.Var 
        -> UniqSM (Maybe (Primitives, [G.CoreBind]))

makeTable v
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
        (b1, expr_loop)                 <- makeSelector v "prim_loop"
        (b2, expr_rateOfSeries)         <- makeSelector v "prim_rateOfSeries"

        -- Int functions.
        (b3, expr_addInt)               <- makeSelector v "prim_addInt"
        (b4, expr_mulInt)               <- makeSelector v "prim_mulInt"
        (b5, expr_newIntVector)         <- makeSelector v "prim_newIntVector"
        (b6, expr_readIntVector)        <- makeSelector v "prim_readIntVector"
        (b7, expr_writeIntVector)       <- makeSelector v "prim_writeIntVector"
        (b8, expr_nextInt)              <- makeSelector v "prim_nextInt"
        let bs  = [b1, b2, b3, b4, b5, b6, b7, b8]

        let table      
                = Primitives
                { prim_Series           = tySeries
                , prim_Vector           = tyVector
                , prim_loop             = expr_loop
                , prim_rateOfSeries     = expr_rateOfSeries
                , prim_addInt           = expr_addInt
                , prim_mulInt           = expr_mulInt
                , prim_newIntVector     = expr_newIntVector
                , prim_readIntVector    = expr_readIntVector
                , prim_writeIntVector   = expr_writeIntVector
                , prim_nextInt          = expr_nextInt }


        return $ Just (table, bs)

 | otherwise
 = return Nothing


-------------------------------------------------------------------------------
-- | Build a CoreExpr that produces the primtive with the given name.
makeSelector
        :: G.Var                -- ^ Core variable bound to our primtiive table.
        -> String               -- ^ Name of the primitive we want.
        -> UniqSM (G.CoreBind, (G.CoreExpr, G.Type))

makeSelector v strField
 | t                      <- G.varType v
 , Just tc                <- G.tyConAppTyCon_maybe t
 , G.isAlgTyCon tc
 , G.DataTyCon [dc] False <- G.algTyConRhs tc
 = do   
        -- Lookup the Name for the field we want.
        let labels      = G.dataConFieldLabels dc
        let Just field  = find (\n -> stringOfName n == strField) labels
        makeSelector' dc field (G.Var v) (G.varType v)

 | otherwise
 = error "repa-plugin.Selector: malformed primitive table"


makeSelector'
        :: G.DataCon            -- ^ Data constructor for the primitive table.
        -> G.FieldLabel         -- ^ Name of the field to project out.
        -> G.CoreExpr           -- ^ Expression to produce the table.
        -> G.Type               -- ^ Type of the table.
        -> UniqSM (G.CoreBind, (G.CoreExpr, G.Type))

makeSelector' dc labelWanted xTable tTable
 = do   
        -- Make binders to match all fields,
        --      including one for the field we want.
        (bsAll, vWanted) <- makeFieldBinders dc labelWanted

        -- The type of the wanted field.
        let tResult     =  G.dataConFieldType dc labelWanted

        -- Top level name for this primitive.
        vPrim           <- newDummyExportedVar (stringOfName labelWanted) tResult
        
        let bPrim       = G.NonRec vPrim 
                        $ G.mkWildCase xTable tTable tResult
                                [ (G.DataAlt dc, bsAll, G.Var vWanted)]

        return  (bPrim, (G.Var vPrim, tResult))
                

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

