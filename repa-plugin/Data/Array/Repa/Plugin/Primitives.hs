
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
import qualified MkId           as G
import qualified PrimOp         as G
import qualified Var            as G
import qualified CmmType        as G
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
        { prim_Series                   :: !G.Type
        , prim_Vector                   :: !G.Type
        , prim_Ref                      :: !G.Type
        , prim_Down4                    :: !G.Type
        , prim_Tail4                    :: !G.Type

          -- Series
        , prim_natOfRateNat             :: (G.CoreExpr, G.Type)
        , prim_rateOfSeries             :: (G.CoreExpr, G.Type)
        , prim_down4                    :: (G.CoreExpr, G.Type)
        , prim_tail4                    :: (G.CoreExpr, G.Type)

          -- Vector
        , prim_tailVector4              :: (G.CoreExpr, G.Type)

          -- Loop combinators.
        , prim_makeProcess              :: (G.CoreExpr, G.Type)
        , prim_loop                     :: (G.CoreExpr, G.Type)
        , prim_guard                    :: (G.CoreExpr, G.Type)
        , prim_split4                   :: (G.CoreExpr, G.Type)

          -- Float4
        , prim_next4Float               :: (G.CoreExpr, G.Type)
        , prim_writeVectorFloatX4       :: (G.CoreExpr, G.Type)
        , prim_projFloatX4_0            :: (G.CoreExpr, G.Type)
        , prim_projFloatX4_1            :: (G.CoreExpr, G.Type)
        , prim_projFloatX4_2            :: (G.CoreExpr, G.Type)
        , prim_projFloatX4_3            :: (G.CoreExpr, G.Type)

         -- Double2
        , prim_next2Double              :: (G.CoreExpr, G.Type)
        , prim_writeVectorDoubleX2      :: (G.CoreExpr, G.Type)

          -- Primitives per base type.
        , prim_baseInt                  :: Map Name (G.CoreExpr, G.Type)
        , prim_baseWord                 :: Map Name (G.CoreExpr, G.Type)
        , prim_baseFloat32              :: Map Name (G.CoreExpr, G.Type)
        , prim_baseFloat64              :: Map Name (G.CoreExpr, G.Type)
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
-- Build a map of operators from bakedin and external atables.
buildOpMap
        :: [(String, (G.CoreExpr, G.Type))]   
                                -- ^ Selectors
        -> [(Name, G.Id)]       -- ^ Directly implemented operators.
        -> [(Name, String)]     -- ^ Operators from the repa-primitives table.
        -> Map Name (G.CoreExpr, G.Type)

buildOpMap selectors bakedin external
 = Map.fromList
        $ [(name, getPrim name) 
                | name <- map fst bakedin ++ map fst external ]

 where  getPrim name
         | Just gid <- lookup name bakedin 
         = (G.Var gid, G.varType gid)

         | otherwise
         = let Just str = lookup name external
               Just r   = lookup str selectors
           in  r


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
        let getTy str
                = let Just ty   = liftM (G.dataConFieldType dc)
                                $ find (\n -> stringOfName n == str) labels
                  in  ty

        let tySeries    = getTy "prim_Series"
        let tyVector    = getTy "prim_Vector"
        let tyRef       = getTy "prim_Ref"
        let tyDown4     = getTy "prim_Down4"
        let tyTail4     = getTy "prim_Tail4"


        -- Build table of selectors for all the external operators.
        -- Each selector projects the appropriate field from the primitives table.
        (bs, sels)      <- makeSelectors v allExternalNames
        let get name    =  let Just r = lookup name sels in  r
        
        let table      
                = Primitives
                { prim_Series                   = tySeries
                , prim_Vector                   = tyVector
                , prim_Ref                      = tyRef
                , prim_Down4                    = tyDown4
                , prim_Tail4                    = tyTail4

                -- Series
                , prim_natOfRateNat             = get "prim_natOfRateNat"
                , prim_rateOfSeries             = get "prim_rateOfSeries" 
                , prim_down4                    = get "prim_down4"
                , prim_tail4                    = get "prim_tail4"

                -- Vector
                , prim_tailVector4              = get "prim_tailVector4"

                -- Control
                , prim_makeProcess              = get "prim_makeProcess"
                , prim_loop                     = get "prim_loop"
                , prim_guard                    = get "prim_guard"
                , prim_split4                   = get "prim_split4"

                -- Float4
                , prim_next4Float               = get "prim_next4Float"
                , prim_writeVectorFloatX4       = get "prim_writeVectorFloatX4"
                , prim_projFloatX4_0            = get "prim_projFloatX4_0"
                , prim_projFloatX4_1            = get "prim_projFloatX4_1"
                , prim_projFloatX4_2            = get "prim_projFloatX4_2"
                , prim_projFloatX4_3            = get "prim_projFloatX4_3"

                -- Double2
                , prim_next2Double              = get "prim_next2Double"
                , prim_writeVectorDoubleX2      = get "prim_writeVectorDoubleX2"

                -- Primitives per base type
                , prim_baseInt          = buildOpMap sels bakedin_Int     external_Int
                , prim_baseWord         = buildOpMap sels bakedin_Word    external_Word
                , prim_baseFloat32      = buildOpMap sels bakedin_Float32 external_Float32
                , prim_baseFloat64      = buildOpMap sels bakedin_Float64 external_Float64
                }

        return $ Just (table, bs)

 | otherwise
 = return Nothing


-------------------------------------------------------------------------------
-- Baked in primitives are the ones that GHC implements directly.
--   Each of these tables map the DDC name for the primop at the appropriate
--   type onto the GHC Id for it.
--
bakedin_Int :: [(Name, G.Id)]
bakedin_Int 
 =      [ (NamePrimArith PrimArithAdd,  G.mkPrimOpId G.IntAddOp) 
        , (NamePrimArith PrimArithSub,  G.mkPrimOpId G.IntSubOp) 
        , (NamePrimArith PrimArithMul,  G.mkPrimOpId G.IntMulOp) 
        , (NamePrimArith PrimArithDiv,  G.mkPrimOpId G.IntQuotOp) 
        , (NamePrimArith PrimArithRem,  G.mkPrimOpId G.IntRemOp) 

        , (NamePrimArith PrimArithEq,   G.mkPrimOpId G.IntEqOp) 
        , (NamePrimArith PrimArithNeq,  G.mkPrimOpId G.IntNeOp) 
        , (NamePrimArith PrimArithGt,   G.mkPrimOpId G.IntGtOp) 
        , (NamePrimArith PrimArithGe,   G.mkPrimOpId G.IntGeOp) 
        , (NamePrimArith PrimArithLt,   G.mkPrimOpId G.IntLtOp) 
        , (NamePrimArith PrimArithLe,   G.mkPrimOpId G.IntLeOp) ]


bakedin_Word :: [(Name, G.Id)]
bakedin_Word 
 =      [ (NamePrimArith PrimArithAdd,  G.mkPrimOpId G.WordAddOp) 
        , (NamePrimArith PrimArithSub,  G.mkPrimOpId G.WordSubOp) 
        , (NamePrimArith PrimArithMul,  G.mkPrimOpId G.WordMulOp) 
        , (NamePrimArith PrimArithDiv,  G.mkPrimOpId G.WordQuotOp) 
        , (NamePrimArith PrimArithRem,  G.mkPrimOpId G.WordRemOp) 

        , (NamePrimArith PrimArithEq,   G.mkPrimOpId G.WordEqOp) 
        , (NamePrimArith PrimArithNeq,  G.mkPrimOpId G.WordNeOp) 
        , (NamePrimArith PrimArithGt,   G.mkPrimOpId G.WordGtOp) 
        , (NamePrimArith PrimArithGe,   G.mkPrimOpId G.WordGeOp) 
        , (NamePrimArith PrimArithLt,   G.mkPrimOpId G.WordLtOp) 
        , (NamePrimArith PrimArithLe,   G.mkPrimOpId G.WordLeOp) ]


bakedin_Float32 :: [(Name, G.Id)]
bakedin_Float32
 =      [ (NamePrimArith PrimArithAdd,  G.mkPrimOpId G.FloatAddOp) 
        , (NamePrimArith PrimArithSub,  G.mkPrimOpId G.FloatSubOp) 
        , (NamePrimArith PrimArithMul,  G.mkPrimOpId G.FloatMulOp) 
        , (NamePrimArith PrimArithDiv,  G.mkPrimOpId G.FloatDivOp) 

        , (NamePrimArith PrimArithEq,   G.mkPrimOpId G.FloatEqOp) 
        , (NamePrimArith PrimArithNeq,  G.mkPrimOpId G.FloatNeOp) 
        , (NamePrimArith PrimArithGt,   G.mkPrimOpId G.FloatGtOp) 
        , (NamePrimArith PrimArithGe,   G.mkPrimOpId G.FloatGeOp) 
        , (NamePrimArith PrimArithLt,   G.mkPrimOpId G.FloatLtOp) 
        , (NamePrimArith PrimArithLe,   G.mkPrimOpId G.FloatLeOp) 

        , (NamePrimVec (PrimVecNeg 4),  G.mkPrimOpId (G.VecNegOp G.FloatVec 4 G.W32))
        , (NamePrimVec (PrimVecAdd 4),  G.mkPrimOpId (G.VecAddOp G.FloatVec 4 G.W32))
        , (NamePrimVec (PrimVecSub 4),  G.mkPrimOpId (G.VecSubOp G.FloatVec 4 G.W32))
        , (NamePrimVec (PrimVecMul 4),  G.mkPrimOpId (G.VecMulOp G.FloatVec 4 G.W32))
        , (NamePrimVec (PrimVecDiv 4),  G.mkPrimOpId (G.VecDivOp G.FloatVec 4 G.W32))
        , (NamePrimVec (PrimVecRep 4),  G.mkPrimOpId (G.VecBroadcastOp G.FloatVec 4 G.W32)) ]


bakedin_Float64 :: [(Name, G.Id)]
bakedin_Float64 
 =      [ (NamePrimArith PrimArithAdd,  G.mkPrimOpId G.DoubleAddOp) 
        , (NamePrimArith PrimArithSub,  G.mkPrimOpId G.DoubleSubOp) 
        , (NamePrimArith PrimArithMul,  G.mkPrimOpId G.DoubleMulOp) 
        , (NamePrimArith PrimArithDiv,  G.mkPrimOpId G.DoubleDivOp) 

        , (NamePrimArith PrimArithEq,   G.mkPrimOpId G.DoubleEqOp) 
        , (NamePrimArith PrimArithNeq,  G.mkPrimOpId G.DoubleNeOp) 
        , (NamePrimArith PrimArithGt,   G.mkPrimOpId G.DoubleGtOp) 
        , (NamePrimArith PrimArithGe,   G.mkPrimOpId G.DoubleGeOp) 
        , (NamePrimArith PrimArithLt,   G.mkPrimOpId G.DoubleLtOp) 
        , (NamePrimArith PrimArithLe,   G.mkPrimOpId G.DoubleLeOp) ]


-------------------------------------------------------------------------------
-- External primitives are the ones implemented in the repa-series package.
--   We get their names from the primitives table exported by repa-series,
--   and build a selector function to project out the element of the table.
--

-- | Names of all external operators imported from the repa-series package.
allExternalNames :: [String]
allExternalNames
 =      (map snd external_control)
 ++     (map snd external_series)

 ++     [ "prim_projFloatX4_0" ]         
 ++     [ "prim_projFloatX4_1" ]         
 ++     [ "prim_projFloatX4_2" ]         
 ++     [ "prim_projFloatX4_3" ]         

 ++     [ "prim_next4Float"
        , "prim_writeVectorFloatX4" ]
 
 ++     [ "prim_next2Double"
        , "prim_writeVectorDoubleX2" ]
 
 ++     [ "prim_makeProcess"]

 ++     (map snd external_Int)
 ++     (map snd external_Word)
 ++     (map snd external_Float32)
 ++     (map snd external_Float64)


-- | Names of loop combinators.
external_control :: [(Name, String)]
external_control
 =      [ (NameOpControl OpControlLoop,         "prim_loop")
        , (NameOpControl OpControlGuard,        "prim_guard") 
        , (NameOpControl OpControlGuard,        "prim_split4") ]


-- | Name of series primitives.
external_series :: [(Name, String)]
external_series 
 =      [ (NameOpConcrete OpConcreteRateOfSeries,   "prim_rateOfSeries") 
        , (NameOpConcrete OpConcreteNatOfRateNat,   "prim_natOfRateNat")
        , (NameOpConcrete (OpConcreteDown 4),       "prim_down4")
        , (NameOpConcrete (OpConcreteTail 4),       "prim_tail4") 
        , (NameOpStore    (OpStoreTailVector 4),    "prim_tailVector4") ]


-- External scalar operators --------------------------------------------------
-- | These functions are defined for every scalar type.
--  
--   The table maps the Core Flow name to the base name used in the imported
--   primitive table. To turn this into the external name we add a "prim_"
--   prefix and TYPE suffix.  Like "add" => "prim_addInt"
--
external_scalarTYPE :: [(Name, String)]
external_scalarTYPE
 =      [ (NameOpConcrete  (OpConcreteNext 1),  "next") 

        , (NameOpStore OpStoreNew,              "newRef")
        , (NameOpStore OpStoreRead,             "readRef")
        , (NameOpStore OpStoreWrite,            "writeRef")

        , (NameOpStore OpStoreNewVector,        "newVector")
        , (NameOpStore (OpStoreReadVector  1),  "readVector")
        , (NameOpStore (OpStoreWriteVector 1),  "writeVector")
        , (NameOpStore OpStoreSliceVector,      "sliceVector") ]


-- | Primitive table names for Int operators.
external_Int    :: [(Name, String)]
external_Int
 =      [ (n, "prim_" ++ s ++ "Int")    | (n, s) <- external_scalarTYPE ]


-- | Primitive table names for Word operators.
external_Word   :: [(Name, String)]
external_Word
 =      [ (n, "prim_" ++ s ++ "Word")   | (n, s) <- external_scalarTYPE ]


-- | Primitive table names for Float32 operators.
external_Float32  :: [(Name, String)]
external_Float32
 =      [ (n, "prim_" ++ s ++ "Float")  | (n, s) <- external_scalarTYPE ]


-- | Primitive table names for Double operators.
external_Float64 :: [(Name, String)]
external_Float64
 =      [ (n, "prim_" ++ s ++ "Double") | (n, s) <- external_scalarTYPE ]

