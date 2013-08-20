
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
import Data.Map                 (Map)


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

          -- Hacks
        , prim_nextInt_T2       :: (G.CoreExpr, G.Type)

          -- Primitives per base type.
        , prim_baseInt          :: Map Name (G.CoreExpr, G.Type)
        , prim_baseFloat        :: Map Name (G.CoreExpr, G.Type)
        , prim_baseDouble       :: Map Name (G.CoreExpr, G.Type)
        }


-- | Names of loop combinators.
primitive_control :: [(Name, String)]
primitive_control
 =      [ (NameOpControl OpControlLoop,         "prim_loop")
        , (NameOpControl OpControlGuard,        "prim_guard") ]


-- | Name sof series primitives.
primitive_series :: [(Name, String)]
primitive_series 
 =      [ (NameOpSeries OpSeriesRateOfSeries,   "prim_rateOfSeries") ]


-- | Map Core Flow Name to the base name used in the imported primitive table.
--   To turn this into the external name we need to add  a "prim_" prefix and
--   TYPE suffix.  Like "add" => "prim_addInt"
--
primitive_baseTYPE :: [(Name, String)]
primitive_baseTYPE
 =      [ (NameOpSeries  (OpSeriesNext 1),      "next") 

        , (NamePrimArith PrimArithAdd,          "add")
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
        , (NameOpStore (OpStoreReadVector  1),  "readVector")
        , (NameOpStore (OpStoreWriteVector 1),  "writeVector")
        , (NameOpStore OpStoreSliceVector,      "sliceVector") ]


-- | Primitive table names for Int operators.
primitive_baseInt    :: [(Name, String)]
primitive_baseInt
 =      [ (n, "prim_" ++ s ++ "Int")    | (n, s) <- primitive_baseTYPE ]


-- | Primitive table names for Float operators.
primitive_baseFloat  :: [(Name, String)]
primitive_baseFloat
 =      [ (n, "prim_" ++ s ++ "Float")  | (n, s) <- primitive_baseTYPE ]


-- | Primitive table names for Double operators.
primitive_baseDouble :: [(Name, String)]
primitive_baseDouble
 =      [ (n, "prim_" ++ s ++ "Double") | (n, s) <- primitive_baseTYPE ]


-- | Names of all primitive operators imported from repa-series.
allPrimOpNames :: [String]
allPrimOpNames
 =      (map snd primitive_control)
 ++     (map snd primitive_series)
 ++     [ "prim_nextInt_T2" ]           -- HACKS: Needs to die.
 ++     (map snd primitive_baseInt)
 ++     (map snd primitive_baseFloat)
 ++     (map snd primitive_baseDouble)


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
        (bs, selectors)     <- makeSelectors v allPrimOpNames
        let get name
                = let Just r    = lookup name selectors
                  in  r

        let populate :: (Name, String) -> (Name, (G.CoreExpr, G.Type))
            populate (name, str)
             = let Just r = lookup str selectors
               in  (name, r)

        let table      
                = Primitives
                { prim_Series           = tySeries
                , prim_Vector           = tyVector
                , prim_Ref              = tyRef

                -- Loop
                , prim_rateOfSeries     = get "prim_rateOfSeries" 
                , prim_loop             = get "prim_loop"
                , prim_guard            = get "prim_guard"

                -- Hacks
                , prim_nextInt_T2       = get "prim_nextInt_T2"

                -- Primitives per base type
                , prim_baseInt          = Map.fromList
                                        $ map populate primitive_baseInt

                , prim_baseFloat        = Map.fromList
                                        $ map populate primitive_baseFloat

                , prim_baseDouble       = Map.fromList
                                        $ map populate primitive_baseDouble
                 }


        return $ Just (table, bs)

 | otherwise
 = return Nothing


