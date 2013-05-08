
module Data.Array.Repa.Plugin.ToDDC.Convert.Var
        ( convertFatName
        , convertVarName
        , convertName
        , convertLiteral)
where
import Data.Array.Repa.Plugin.FatName
import DDC.Base.Pretty
import Data.Char

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D

import qualified Type                   as G
import qualified Var                    as G
import qualified OccName                as OccName
import qualified Name                   as Name
import qualified Literal                as G


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
         c : _ 
          | isUpper c   -> D.NameCon str
          | otherwise   -> D.NameVar str


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

