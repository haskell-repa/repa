
module Data.Array.Repa.Plugin.ToDDC.Convert.Base
        (Fail (..))
where
import DDC.Base.Pretty
import qualified DDC.Core.Flow          as D

import qualified Literal                as G
import qualified Var                    as G
import qualified Kind                   as G
import qualified OccName                as Occ
import qualified Name                   as Name

-- | A reason why we didn't convert a GHC Core thing to Disciple Core.
data Fail
        -- Atomic Failures ------------
        -- | Top level binding was not marked for conversion.
        = FailNotMarked

        -- | Cannot convert kind.
        | FailUnhandledKind G.Kind

        -- | Cannot convert numeric type literals.
        | FailNoNumericTypeLiterals

        -- | Cannot convert recursive binding groups.
        | FailNoRecursion [G.Var]

        -- | Cannot convert type casts.
        | FailNoCasts

        -- | Cannot convert coercions.
        | FailNoCoercions

        -- | Unhandled literal value
        | FailUnhandledLiteral G.Literal

        -- | Case expressions not handled yet.
        | FailUnhandledCase

        -- | Name read from GHC Core is empty.
        | FailEmptyName

        -- | Dodgy top-level binding name.
        | FailDodgyTopLevelBindingName D.Name

        -- Fail combinators -----------
        -- | Failure in a top-level binding.
        | FailInBinding G.Var Fail


instance Pretty Fail where
 ppr FailNotMarked
  = text "Top level binding not marked for conversion."

 ppr (FailUnhandledKind _)
  = text "Unhandled GHC kind"

 ppr FailNoNumericTypeLiterals
  = text "Cannot convert numeric type literals."

 ppr (FailNoRecursion _)
  = text "Cannot convert recursive binding groups."

 ppr FailNoCasts
  = text "Cannnot convert type casts."

 ppr FailNoCoercions
  = text "Cannot convert coercions."

 ppr (FailUnhandledLiteral _)
  = text "Unhandled literal value."

 ppr (FailUnhandledCase)
  = text "Unhandled case expresson."

 ppr FailEmptyName
  = text "Empty name in GHC Core program."

 ppr (FailDodgyTopLevelBindingName _)
  = text "Dodgy top level binding name."

 ppr (FailInBinding v fails)
  = vcat [ text "In binding "
                <> text "'" 
                <> (text $ Occ.occNameString $ Name.occName $ G.varName v)
                <> text "'"
         , ppr fails]
