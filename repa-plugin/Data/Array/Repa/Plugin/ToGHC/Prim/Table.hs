
module Data.Array.Repa.Plugin.ToGHC.Prim.Table
        ( findPrimitive
        , isPolytypicPrimName

        , Prim (..)
        , primitives)
where
import Data.Array.Repa.Plugin.ToGHC.Prim.Imported
import Data.List

import qualified Type                    as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Prim      as D
import qualified DDC.Core.Flow.Compounds as D


-- | Find the primitive function for the given name and type arguments.
findPrimitive 
        :: ImportedNames
        -> D.Name 
        -> [D.Type D.Name] 
        -> Maybe G.Var

findPrimitive imports name ts
        | Just prim'    <- find (\p -> primName p == name) primitives
        , length ts == length (primTypeArgs prim')
        , all id $ zipWith matchesTypeParam ts (primTypeArgs prim')
        = findImportedPrimVar imports (primSymbol prim')

        | otherwise
        = Nothing


-- | Check whether the function with this name must be handled polytypically. 
--   This is try when it has an entry in the primitive table that 
--   requires type arguments.
isPolytypicPrimName :: D.Name -> Bool
isPolytypicPrimName n
        | Just prim'    <- find (\p -> primName p == n) primitives
        = not $ null (primTypeArgs prim')

        | otherwise
        = False


-------------------------------------------------------------------------------
data Prim
        = Prim
        { -- | Name of the primitive function in the Disciple Core code.
          primName      :: D.Name

          -- | The type arguments that a function must be applied to to
          --   match this primitive, or `Nothing` to ignore that argument.
        , primTypeArgs  :: [Maybe (D.Type D.Name)]

          -- | Name of the symbol in the GHC Core code.
        , primSymbol    :: String }


-- | Check if this type argument matches the required parameter.
matchesTypeParam 
        :: D.Type D.Name 
        -> Maybe (D.Type D.Name) 
        -> Bool

matchesTypeParam t mt
 = case mt of
        Nothing -> True
        Just t' -> t == t'


-------------------------------------------------------------------------------
-- | The prim table.
--   All primitives that the lowering transform uses should be listed here.
--
primitives :: [Prim]
primitives
 = [    -- Arithmetic Operators
        Prim    (D.NamePrimArith D.PrimArithAdd)
                [Just D.tInt]
                "repa_addInt"

   ,    Prim    (D.NamePrimArith D.PrimArithMul)
                [Just D.tInt]
                "repa_mulInt"

        -- Flow Operators
   ,    Prim    (D.NameOpFlow D.OpFlowRateOfStream)
                []
                "repa_rateOfStream"

        -- Store Operators
   ,    Prim    (D.NameOpStore D.OpStoreNext)
                [Just D.tInt]
                "repa_nextInt"

   ,    Prim    (D.NameOpStore D.OpStoreNewArray)
                [Nothing]
                "repa_newByteArray"

   ,    Prim    (D.NameOpStore D.OpStoreReadArray)
                [Just D.tInt]
                "repa_readIntArray"

   ,    Prim    (D.NameOpStore D.OpStoreWriteArray)
                [Just D.tInt]
                "repa_writeIntArray"

        -- Loop Combinators
   ,    Prim    (D.NameOpLoop D.OpLoopLoopN)
                [Nothing]
                "repa_loop"
   ]

