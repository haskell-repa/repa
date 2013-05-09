
module Data.Array.Repa.Plugin.ToGHC.Var
        ( plainNameOfVar
        , newDummyVar
        , newDummyExportedVar
        , newDummyTyVar)
where
import qualified Type                   as G
import qualified IdInfo                 as G
import qualified Var                    as G
import qualified UniqSupply             as G
import qualified FastString             as G
import qualified OccName                as Occ
import qualified Name                   as Name


-- Variable utils -------------------------------------------------------------
-- | Take the plain unqualified printable name of a GHC variable.
plainNameOfVar :: G.Var -> String
plainNameOfVar gv
 = let  name    = G.varName gv
        occ     = Name.nameOccName name
   in   Occ.occNameString occ


-- | Create a fresh dummy GHC expression variable with the given type.
newDummyExportedVar :: String -> G.Type -> G.UniqSM G.Var
newDummyExportedVar basename ty
 = do   let details = G.VanillaId
        let occName = Occ.mkOccName Occ.varName basename
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkExportedLocalVar details name ty info


-- | Create a fresh dummy GHC expression variable with the given type.
newDummyVar :: String -> G.Type -> G.UniqSM G.Var
newDummyVar basename ty
 = do   let details = G.VanillaId
        let occName = Occ.mkOccName Occ.varName basename
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkLocalVar details name ty info


-- | Create a fresh dummy GHC type variable with the given type.
newDummyTyVar :: String -> G.UniqSM G.Var
newDummyTyVar basename
 = do   unique      <- G.getUniqueUs
        let name    =  Name.mkSysTvName unique (G.fsLit basename)
        return  $ G.mkTyVar name G.liftedTypeKind




