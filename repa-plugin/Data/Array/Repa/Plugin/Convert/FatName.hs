
module Data.Array.Repa.Plugin.Convert.FatName
        ( GhcName (..)
        , FatName (..))
where
import DDC.Base.Pretty
import qualified Var                    as G
import qualified Literal                as G
import qualified TyCon                  as G
import qualified TypeRep                as G

import qualified DDC.Core.Exp           as D
import qualified DDC.Core.Flow.Name     as D


-------------------------------------------------------------------------------
data GhcName
        = GhcNameVar     G.Var
        | GhcNameTyCon   G.TyCon
        | GhcNameTyLit   G.TyLit
        | GhcNameLiteral G.Literal
        | GhcNameIntU   
        deriving (Eq, Ord)


data FatName
        = FatName
        { fatNameGHC    :: GhcName
        , fatNameDDC    :: D.Name }
        deriving Eq

instance Pretty FatName where
 ppr (FatName _ name)   = ppr name

