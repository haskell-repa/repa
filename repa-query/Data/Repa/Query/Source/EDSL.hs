{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Repa query EDSL.
--
--   A query written with this EDSL expresses an AST/operator graph that
--   compiled into an executable that computes the result of the query.
--
--   The produced AST includes enough type information about the the source
--   data that it can be checked for well-formedness without access to
--   external metadata -- namely the format of tables and the types of
--   their columns.
--
--   This meta-data can either be embedded directly in the query, 
--   or read from a local copy of the table metadata, depending on what
--   operators are used.
--   
module Data.Repa.Query.Source.EDSL
        ( -- * Types
          Query, Flow, Value

          -- * Query builder
        , query

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.

          -- ** Sourcing
        , fromFile
        , fromTable

          -- ** Mapping
        , map

          -- ** Folding
        , fold
        , folds

          -- ** Filtering
        , filter

          -- ** Grouping
        , groups
        , groupsBy

          -- * Scalar operators
        , negate, abs, signum
        , (+),  (-),  (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=)

        , get2_1, get2_2
        , get3_1, get3_2, get3_3
        , get4_1, get4_2, get4_3, get4_4
        , get5_1, get5_2, get5_3, get5_4, get5_5)
where
import Control.Monad.State.Strict
import System.FilePath
import Data.Repa.Query.Source.Builder           as B
import Data.Repa.Product
import Data.Word
import Data.Int
import qualified Data.Repa.Store.Object.Table   as Table
import qualified Data.Repa.Query.Graph          as G
import qualified Data.Repa.Store.Format         as F
import qualified Prelude                        as P
import Prelude   
 hiding ( map, filter
        , negate, abs, signum
        , (+),  (-), (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))


---------------------------------------------------------------------------------------------------
-- | Produce a query, using the given deliminator and field types
--   for the output format.
query   :: F.Delim              -- ^ Output delimitor.
        -> F.Field a            -- ^ Output field types.
        -> Q (Flow a)           -- ^ Output flow.
        -> Q Query

query delim field mkFlow
 = do   flow    <- mkFlow
        return  $ Query delim field flow


---------------------------------------------------------------------------------------------------
-- | Read complete rows from a flat file.
fromFile  :: FilePath
          -> F.Delim 
          -> F.Field a 
          -> Q (Flow a)

fromFile path delim field
 = do   fOut    <- newFlow
        addNode $ G.NodeSource 
                $ G.SourceFile () path delim (F.flattens field) 
                $ takeFlow fOut
        return  fOut


-- | Read complete rows from a table.
--   TODO: load meta-data at graph construction time.
fromTable :: FilePath -> Q (Flow a)
fromTable path 
 = do   
        -- Load meta-data for the table.
        pathRoot <- B.getRootDataQ
        emeta    <- B.liftIO $ Table.loadMeta (pathRoot </> path)

        case emeta of
         Left errLoadMeta
          -> failQ $ show errLoadMeta

         Right _table
          -> do
                -- Load the table meta data.
                -- We do this at query build time 
                fOut    <- newFlow
                addNode $ G.NodeSource
                        $ G.SourceTable () path 
                                F.Fixed            -- TODO: lookup real delim from meta-data.
                                []                 -- TODO: lookup real fields from meta-data.
                        $ takeFlow fOut
                return fOut


-- | Apply a scalar function to every element of a flow.
map :: (Value a -> Value b) -> Flow a -> Q (Flow b)
map fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


-- | Combine all elements of a flow using the given operator and neutral value.
--   The result is returned in a new flow of a single element.
--
--   To support parallel evaluation, the neutral value must be both a left and
--   right unit of the combining operator, otherwise the result is undefined.
--   For example use, (+) as the operator and 0 as the neutral value to perform
--   a sum.
--
-- @ 
--   x + 0  = x  (0 is left-unit  of (+))
--   0 + x  = x  (0 is right-unit of (+))
-- @
--
fold    :: (Value a -> Value a -> Value a)
        -> Value a 
        -> Flow a -> Q (Flow a)

fold fun (Value z) fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFoldI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                        in  x)
                z

        return fOut


-- | Segmented fold. Like `fold`, but combine consecutive runs of elements. 
--   The length of each run is taken from a second flow.
folds   :: (Value a -> Value a -> Value a)
        -> Value a
        -> Flow Int -> Flow a
        -> Q (Flow a)

folds fun (Value z) fLens fElems
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFoldsI
                (takeFlow fLens)
                (takeFlow fElems)
                (takeFlow fOut)
                (xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                        in  x)
                z

        return fOut


-- | Keep only the elements that match the given predicate.
filter  :: (Value a -> Value Bool)
        -> Flow a -> Q (Flow a)

filter fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFilterI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


-- | Scan through a flow to find runs of consecutive values,
--   yielding the value and the length of each run.
groups :: Flow a -> Q (Flow (a :*: Int))
groups fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp  
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ xLam $ xOp G.SopEq [xVar 0, xVar 1])

        return fOut


-- | Like `groups` but use the given predicate to decide whether
--   consecutive values should be placed into the same group.
groupsBy :: (Value a -> Value a -> Value Bool) 
         -> Flow a -> Q (Flow (a :*: Int))
groupsBy fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam   $ xLam 
                        $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                          in  x)

        return $ fOut


---------------------------------------------------------------------------------------------------
-- Wrappers for scalar operators.

-- | Scalar negation.
negate :: Value a -> Value a
negate = makeScalarOp1 G.SopNeg

-- | Scalar absolute value.
abs :: Value a -> Value a
abs    = makeScalarOp1 G.SopAbs

-- | Scalar sign of number.
signum :: Value a -> Value a
signum = makeScalarOp1 G.SopSignum

-- | Scalar addition.
(+) :: Value a -> Value a -> Value a
(+) = makeScalarOp2 G.SopAdd 

-- | Scalar subtraction.
(-) :: Value a -> Value a -> Value a
(-) = makeScalarOp2 G.SopSub

-- | Scalar multiplication.
(*) :: Value a -> Value a -> Value a
(*) = makeScalarOp2 G.SopMul

-- | Scalar division.
(/) :: Value a -> Value a -> Value a
(/) = makeScalarOp2 G.SopMul

-- | Scalar equality.
(==) :: Value a -> Value a -> Value Bool
(==) = makeScalarOp2 G.SopEq

-- | Scalar negated equality.
(/=) :: Value a -> Value a -> Value Bool
(/=) = makeScalarOp2 G.SopNeq

-- | Scalar greater-than.
(>) :: Value a -> Value a -> Value Bool
(>) = makeScalarOp2 G.SopGt

-- | Scalar greater-than-equal.
(>=) :: Value a -> Value a -> Value Bool
(>=) = makeScalarOp2 G.SopGe

-- | Scalar less-than.
(<) :: Value a -> Value a -> Value Bool
(<)  = makeScalarOp2 G.SopLt

-- | Scalar less-than-equal.
(<=) :: Value a -> Value a -> Value Bool
(<=) = makeScalarOp2 G.SopLe


makeScalarOp1 :: G.ScalarOp -> Value a -> Value c
makeScalarOp1 sop (Value x)
        = Value $ xOp sop [x]


makeScalarOp2 :: G.ScalarOp -> Value a -> Value b -> Value c
makeScalarOp2 sop (Value x1) (Value x2)
        = Value $ xOp sop [x1, x2]


---------------------------------------------------------------------------------------------------
-- | Promote scalar literals.
instance Num (Value Word) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt x))


instance Num (Value Float) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LFloat (fromIntegral x)))


instance Num (Value Double) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LFloat (fromIntegral x)))


instance Num (Value Word8) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int8) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Word16) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int16) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Word32) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int32) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Word64) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int64) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


---------------------------------------------------------------------------------------------------
-- Projection.
-- 2
get2_1  :: Value (a :*: b)                        -> Value a
get2_1  = makeScalarOp1 $ G.SopProj 2 1

get2_2  :: Value (a :*: b)                        -> Value b
get2_2  = makeScalarOp1 $ G.SopProj 2 1


-- 3
get3_1  :: Value (a :*: b :*: c)                  -> Value a
get3_1  = makeScalarOp1 $ G.SopProj 3 1

get3_2  :: Value (a :*: b :*: c)                  -> Value b
get3_2  = makeScalarOp1 $ G.SopProj 3 2

get3_3  :: Value (a :*: b :*: c)                  -> Value c
get3_3  = makeScalarOp1 $ G.SopProj 3 3


-- 4
get4_1  :: Value (a :*: b :*: c :*: d)            -> Value a
get4_1  = makeScalarOp1 $ G.SopProj 4 1

get4_2  :: Value (a :*: b :*: c :*: d)            -> Value b
get4_2  = makeScalarOp1 $ G.SopProj 4 2

get4_3  :: Value (a :*: b :*: c :*: d)            -> Value c
get4_3  = makeScalarOp1 $ G.SopProj 4 3

get4_4  :: Value (a :*: b :*: c :*: d)            -> Value d
get4_4  = makeScalarOp1 $ G.SopProj 4 4


-- 5
get5_1  :: Value (a :*: b :*: c :*: d :*: e)      -> Value a
get5_1  = makeScalarOp1 $ G.SopProj 5 1

get5_2  :: Value (a :*: b :*: c :*: d :*: e)      -> Value b
get5_2  = makeScalarOp1 $ G.SopProj 5 2

get5_3  :: Value (a :*: b :*: c :*: d :*: e)      -> Value c
get5_3  = makeScalarOp1 $ G.SopProj 5 3

get5_4  :: Value (a :*: b :*: c :*: d :*: e)      -> Value d
get5_4  = makeScalarOp1 $ G.SopProj 5 4

get5_5  :: Value (a :*: b :*: c :*: d :*: e)      -> Value e
get5_5  = makeScalarOp1 $ G.SopProj 5 5


---------------------------------------------------------------------------------------------------
-- Utils
xVar i          = G.XVar () i
xLam x          = G.XVal () (G.VLam () () x)
xOp s args      = G.XOp  () s args

