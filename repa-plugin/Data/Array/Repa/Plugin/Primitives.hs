
module Data.Array.Repa.Plugin.Primitives
        ( Primitives (..)
        , slurpPrimitives)
where
import Data.Array.Repa.Plugin.Primitives.Selectors
import DDC.Core.Flow.Prim
import Data.List
import Data.Maybe
import Control.Monad

import qualified HscTypes       as G
import qualified CoreSyn        as G
import qualified DataCon        as G
import qualified TyCon          as G
import qualified Type           as G
import qualified Var            as G
import qualified OccName        as Occ
import qualified Name           as Name

import UniqSupply               as G
import qualified UniqSet        as US
import qualified Data.Map       as Map

-------------------------------------------------------------------------------
-- | Table of GHC core expressions to use to invoke the primitives
--   needed by the lowering transform.
data Primitives
        = Primitives
        { prim_Series           :: !G.Type
        , prim_Vector           :: !G.Type
        , prim_Ref              :: !G.Type

          -- Loop
        , prim_loop             :: (G.CoreExpr, G.Type)
        , prim_guard            :: (G.CoreExpr, G.Type)
        , prim_rateOfSeries     :: (G.CoreExpr, G.Type)

        , prim_nextInt          :: (G.CoreExpr, G.Type)

        , prim_nextInt_T2       :: (G.CoreExpr, G.Type)

          -- Arith Int
        , prim_addInt           :: (G.CoreExpr, G.Type)
        , prim_subInt           :: (G.CoreExpr, G.Type)
        , prim_mulInt           :: (G.CoreExpr, G.Type)
        , prim_divInt           :: (G.CoreExpr, G.Type)
        , prim_modInt           :: (G.CoreExpr, G.Type)
        , prim_remInt           :: (G.CoreExpr, G.Type)

          -- Eq Int
        , prim_eqInt            :: (G.CoreExpr, G.Type)
        , prim_neqInt           :: (G.CoreExpr, G.Type)
        , prim_gtInt            :: (G.CoreExpr, G.Type)
        , prim_geInt            :: (G.CoreExpr, G.Type)
        , prim_ltInt            :: (G.CoreExpr, G.Type)
        , prim_leInt            :: (G.CoreExpr, G.Type)

          -- Ref Int
        , prim_newRefInt        :: (G.CoreExpr, G.Type)
        , prim_readRefInt       :: (G.CoreExpr, G.Type)
        , prim_writeRefInt      :: (G.CoreExpr, G.Type)

          -- Vector Int
        , prim_newVectorInt     :: (G.CoreExpr, G.Type)
        , prim_readVectorInt    :: (G.CoreExpr, G.Type)
        , prim_writeVectorInt   :: (G.CoreExpr, G.Type)
        , prim_sliceVectorInt   :: (G.CoreExpr, G.Type)

        }


-- | Map Core Flow primitive name to the base name used in the imported
--   primitive table. To turn this into the external name we need to add
--   the "prim_" prefix and TYPE suffix.
--   Like "add" => "prim_addInt"
--
primitive_baseName
 = Map.fromList
 $      [ (NamePrimArith PrimArithAdd,          "add")
        , (NamePrimArith PrimArithSub,          "sub")
        , (NamePrimArith PrimArithMul,          "mul")
        , (NamePrimArith PrimArithDiv,          "div")
        , (NamePrimArith PrimArithMod,          "mod")
        , (NamePrimArith PrimArithRem,          "rem")

        , (NamePrimArith PrimArithEq,           "eq")
        , (NamePrimArith PrimArithNeq,          "neq")
        , (NamePrimArith PrimArithGt,           "gt")
        , (NamePrimArith PrimArithLt,           "lt")
        , (NamePrimArith PrimArithLe,           "le")
        , (NamePrimArith PrimArithGe,           "ge")

        , (NameOpStore OpStoreNew,              "newRef")
        , (NameOpStore OpStoreRead,             "readRef")
        , (NameOpStore OpStoreWrite,            "writeRef")

        , (NameOpStore OpStoreNewVector,        "newVector")
        , (NameOpStore OpStoreReadVector,       "readVector")
        , (NameOpStore OpStoreWriteVector,      "writeVector")
        , (NameOpStore OpStoreSliceVector,      "sliceVector") ]


-- | Names of all the primitive types.
--   These should match the field names of `Primitives` above.
_primitive_types
 =      [ "Series"
        , "Vector"
        , "Ref" ]


-- | Names of all the primitive operators.
--   These should match the field names of `Primitives` above.
primitive_ops
 =      -- Arith Int
        [ "prim_addInt"
        , "prim_subInt"
        , "prim_mulInt"
        , "prim_divInt"
        , "prim_modInt"
        , "prim_remInt"

        -- Eq Int
        , "prim_eqInt"
        , "prim_neqInt"
        , "prim_gtInt"
        , "prim_geInt"
        , "prim_ltInt"
        , "prim_leInt"

        -- Ref Int
        , "prim_newRefInt"
        , "prim_readRefInt"
        , "prim_writeRefInt"

        -- Vector Int
        , "prim_newVectorInt"
        , "prim_readVectorInt"
        , "prim_writeVectorInt"
        , "prim_sliceVectorInt"

        -- Loop
        , "prim_loop"
        , "prim_guard"
        , "prim_rateOfSeries"
        , "prim_nextInt" 
        , "prim_nextInt_T2" ]


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

        let Just tyRef
                = liftM (G.dataConFieldType dc)
                $ find (\n -> stringOfName n ==  "prim_Ref") labels

        -- Build table of selectors for all the operators.
        (bs, selectors)     <- makeSelectors v primitive_ops
        let get name
                = let Just r    = lookup name selectors
                  in  r

        let table      
                = Primitives
                { prim_Series           = tySeries
                , prim_Vector           = tyVector
                , prim_Ref              = tyRef

                -- Arith Int
                , prim_addInt           = get "prim_addInt"
                , prim_subInt           = get "prim_subInt"
                , prim_mulInt           = get "prim_mulInt"
                , prim_divInt           = get "prim_divInt"
                , prim_modInt           = get "prim_modInt"
                , prim_remInt           = get "prim_remInt"

                -- Eq Int
                , prim_eqInt            = get "prim_eqInt"
                , prim_neqInt           = get "prim_neqInt"
                , prim_gtInt            = get "prim_gtInt"
                , prim_geInt            = get "prim_geInt"
                , prim_ltInt            = get "prim_ltInt"
                , prim_leInt            = get "prim_leInt"

                -- Ref Int
                , prim_newRefInt        = get "prim_newRefInt"
                , prim_readRefInt       = get "prim_readRefInt"
                , prim_writeRefInt      = get "prim_writeRefInt"

                -- Vector Int
                , prim_newVectorInt     = get "prim_newVectorInt"
                , prim_readVectorInt    = get "prim_readVectorInt"
                , prim_writeVectorInt   = get "prim_writeVectorInt"
                , prim_sliceVectorInt   = get "prim_sliceVectorInt"

                -- Loop
                , prim_rateOfSeries     = get "prim_rateOfSeries" 
                , prim_loop             = get "prim_loop"
                , prim_guard            = get "prim_guard"
                , prim_nextInt          = get "prim_nextInt"
                , prim_nextInt_T2       = get "prim_nextInt_T2" }


        return $ Just (table, bs)

 | otherwise
 = return Nothing


