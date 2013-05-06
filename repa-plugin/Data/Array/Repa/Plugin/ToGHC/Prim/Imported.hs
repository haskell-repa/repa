
module Data.Array.Repa.Plugin.ToGHC.Prim.Imported
        ( ImportedNames (..)
        , importedNamesOfGuts

        , findImportedPrimVar
        , isPrimishName)
where
import Data.Array.Repa.Plugin.ToGHC.Var
import Data.List
import qualified HscTypes               as G
import qualified Type                   as G
import qualified Name                   as G
import qualified OccName                as Occ

import qualified CoreSyn                as G


-- | When we want to find the implementation of one of our primitives we
--   can't use the RdrEnv directly. The OccMap holding the imported names
--   is keyed on the Unique for the FastString that the desugar used, but
--   we don't have access to this anymore. Instead we slurp out the raw names
--   and stash them in this table.
data ImportedNames
        = ImportedNames 
        { importedNames :: G.ModGuts }


-- | Slurp out the list of imported names from some guts.
--   TODO: ditch names of things that are definitely not our primitives.
--   TODO: build a map for easy lookup.
importedNamesOfGuts :: G.ModGuts -> ImportedNames
importedNamesOfGuts guts
 = let  
        -- Grab all the imported names that look like they might be
        -- primitives that the lowering transform targets.
        -- elts    = filter (any (isPrimishName . G.gre_name))
        --         $ Occ.occEnvElts
        --         $ G.mg_rdr_env guts

   in   ImportedNames guts


-- | Find the top-level variable with this name.
--   It can be defined in the current module or imported from somewhere else.
findImportedPrimVar :: ImportedNames -> String -> Maybe G.Var
findImportedPrimVar (ImportedNames guts) str
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


-- | Check whether this name might be a primitive that the lowering
--   transform targets. All the primitive variables the plugin uses
--   start with @repa_@
isPrimishName :: G.Name -> Bool
isPrimishName name
 = let  occ     = G.nameOccName name
        str     = Occ.occNameString occ
   in   isPrefixOf "repa_" str
