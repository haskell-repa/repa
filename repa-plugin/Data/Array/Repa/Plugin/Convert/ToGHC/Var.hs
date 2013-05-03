module Data.Array.Repa.Plugin.Convert.ToGHC.Var
        ( findImportedPrimVar
        , plainNameOfVar
        , getPrimOpVar
        , newDummyVar
        , newDummyTyVar)
where
import qualified HscTypes               as G
import qualified CoreSyn                as G
import qualified Type                   as G
import qualified IdInfo                 as G
import qualified Var                    as G
import qualified PrimOp                 as G
import qualified UniqSupply             as G
import qualified FastString             as G
import qualified OccName                as OccName
import qualified Name                   as Name


-- Variable utils -------------------------------------------------------------
-- | Find the variable with this name from the source module.
--   TODO: right now it must be defined in the current module,
--         fix this to actually look in the import list.
findImportedPrimVar :: G.ModGuts -> String -> Maybe G.Var
findImportedPrimVar guts str
 = go (G.mg_binds guts)
 where  
        go (G.NonRec v _ : bs)
         | plainNameOfVar v == str = Just v
         | otherwise               = go bs

        go ( (G.Rec ((v, _) : vxs)) : bs)
         | plainNameOfVar v == str = Just v
         | otherwise               = go (G.Rec vxs : bs)

        go ( G.Rec [] : bs )       = go bs

        go []                      = Nothing


-- | Take the plain unqualified printable name of a GHC variable.
plainNameOfVar :: G.Var -> String
plainNameOfVar gv
 = let  name    = G.varName gv
        occ     = Name.nameOccName name
   in   OccName.occNameString occ


-- | Convert a GHC primop to a variable.
getPrimOpVar :: G.PrimOp -> G.UniqSM G.Var
getPrimOpVar op
 = do   let details = G.PrimOpId   op
        let occName = G.primOpOcc  op
        let ty      = G.primOpType op
        unique      <- G.getUniqueUs
        let name    = Name.mkSystemName unique occName
        let info    = G.vanillaIdInfo
        return  $ G.mkGlobalVar details name ty info


-- | Create a fresh dummy GHC expression variable with the given type.
newDummyVar :: String -> G.Type -> G.UniqSM G.Var
newDummyVar basename ty
 = do   let details = G.VanillaId
        let occName = OccName.mkOccName OccName.varName basename
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




