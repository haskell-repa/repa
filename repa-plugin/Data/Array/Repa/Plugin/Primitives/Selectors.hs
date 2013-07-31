
module Data.Array.Repa.Plugin.Primitives.Selectors
        ( makeSelectors
        , stringOfName)
where
import Data.Array.Repa.Plugin.ToGHC.Var
import Data.List
import Control.Monad

import qualified CoreSyn        as G
import qualified MkCore         as G
import qualified DataCon        as G
import qualified TyCon          as G
import qualified Type           as G
import qualified Var            as G
import qualified OccName        as Occ
import qualified Name           as Name

import UniqSupply               as G


-------------------------------------------------------------------------------
-- | Make the selector table.
makeSelectors
        :: G.Var                -- ^ Core variable bound to our primitive table.
        -> [String]             -- ^ Names of all the primitives.
        -> UniqSM ( [G.CoreBind]
                  , [(String, (G.CoreExpr, G.Type))])

makeSelectors v strs
 = do
        (bs, xts)       <- liftM unzip
                        $  mapM (makeSelector v) strs

        return  $ (bs, zip strs xts)


-------------------------------------------------------------------------------
-- | Build a CoreExpr that produces the primtive with the given name.
makeSelector
        :: G.Var                -- ^ Core variable bound to our primtiive table.
        -> String               -- ^ Name of the primitive we want.
        -> UniqSM (G.CoreBind, (G.CoreExpr, G.Type))

makeSelector v strField
 | t                      <- G.varType v
 , Just tc                <- G.tyConAppTyCon_maybe t
 , G.isAlgTyCon tc
 , G.DataTyCon [dc] False <- G.algTyConRhs tc
 , labels                 <- G.dataConFieldLabels dc
 , Just field             <- find (\n -> stringOfName n == strField) labels
 = makeSelector' dc field (G.Var v) (G.varType v)

 | otherwise
 = error $ "repa-plugin.makeSelector: can't find primitive named " ++ strField


makeSelector'
        :: G.DataCon            -- ^ Data constructor for the primitive table.
        -> G.FieldLabel         -- ^ Name of the field to project out.
        -> G.CoreExpr           -- ^ Expression to produce the table.
        -> G.Type               -- ^ Type of the table.
        -> UniqSM (G.CoreBind, (G.CoreExpr, G.Type))

makeSelector' dc labelWanted xTable tTable
 = do   
        -- Make binders to match all fields,
        --      including one for the field we want.
        (bsAll, vWanted) <- makeFieldBinders dc labelWanted

        -- The type of the wanted field.
        let tResult     =  G.dataConFieldType dc labelWanted

        -- Top level name for this primitive.
        vPrim           <- newDummyExportedVar (stringOfName labelWanted) tResult
        
        let bPrim       = G.NonRec vPrim 
                        $ G.mkWildCase xTable tTable tResult
                                [ (G.DataAlt dc, bsAll, G.Var vWanted)]

        return  (bPrim, (G.Var vPrim, tResult))
                

-- | Make a sequence of binders 
makeFieldBinders 
        :: G.DataCon               -- ^ Data constructor for the primtiive table.
        -> G.FieldLabel            -- ^ The field we want to project out.
        -> UniqSM ([G.Var], G.Var) -- ^ All binders, and the one for our desired field.

makeFieldBinders dc labelWanted
 = do   let tWanted =  G.dataConFieldType dc labelWanted
        vWanted     <- newDummyVar "wanted" tWanted
        let bsAll   =  go vWanted (G.dataConFieldLabels dc)
        return  (bsAll, vWanted)

 where  go _       []   = []
        go vWanted (l:ls)
         | l == labelWanted 
         = vWanted
                : go vWanted ls

         | otherwise        
         = (G.mkWildValBinder $ G.dataConFieldType dc l)
                : go vWanted ls


-- Utils ----------------------------------------------------------------------
-- | Convert a GHC name to a string
stringOfName :: Name.Name -> String
stringOfName name
 = Occ.occNameString $ Name.nameOccName name

