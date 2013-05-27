
module Data.Array.Repa.Plugin.Pass.Lower
        (passLower)
where
import Data.Array.Repa.Plugin.Primitives
import Data.Array.Repa.Plugin.ToDDC.Detect
import Data.Array.Repa.Plugin.ToDDC
import Data.Array.Repa.Plugin.ToGHC
import Data.Array.Repa.Plugin.GHC.Pretty
import DDC.Core.Exp

import qualified DDC.Core.Flow                          as Flow
import qualified DDC.Core.Flow.Profile                  as Flow
import qualified DDC.Core.Flow.Transform.Prep           as Flow
import qualified DDC.Core.Flow.Transform.Slurp          as Flow
import qualified DDC.Core.Flow.Transform.Schedule       as Flow
import qualified DDC.Core.Flow.Transform.Extract        as Flow
import qualified DDC.Core.Flow.Transform.Concretize     as Flow
import qualified DDC.Core.Flow.Transform.Thread         as Flow

import qualified DDC.Core.Module                        as Core
import qualified DDC.Core.Check                         as Core
import qualified DDC.Core.Simplifier                    as Core
import qualified DDC.Core.Transform.Namify              as Core
import qualified DDC.Core.Transform.Flatten             as Core
import qualified DDC.Core.Transform.Forward             as Core
import qualified DDC.Core.Transform.Thread              as Core
import qualified DDC.Core.Transform.Reannotate          as Core
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Core.Transform.Eta                 as Eta
import qualified DDC.Type.Env                           as Env

import qualified HscTypes                               as G
import qualified CoreMonad                              as G
import qualified UniqSupply                             as G
import qualified DDC.Base.Pretty                        as D
import qualified Data.Map                               as Map
import System.IO.Unsafe
import Control.Monad.State.Strict
import Data.List


-- | We use this unique when generating fresh names.
--   If this is not actually unique relative to the rest of the compiler
--   and other plugins then we're complety screwed.
letsHopeThisIsUnique    :: Char
letsHopeThisIsUnique    = 's'


-- | Run the lowering pass on this module.
passLower :: String -> G.ModGuts -> G.CoreM G.ModGuts
passLower name guts0
 = unsafePerformIO
 $ do
        -- Here's hoping this is really unique
        us      <- G.mkSplitUniqSupply letsHopeThisIsUnique

        -- Input ------------------------------------------
        writeFile ("dump." ++ name ++ ".01-ghc.hs")
         $ D.render D.RenderIndent (pprModGuts guts0)

        -- Primitives -------------------------------------
        -- Build a table of expressions to access our primitives.
        let (Just (primitives, guts), us2) 
                = G.initUs us (slurpPrimitives guts0)

        -- Convert ----------------------------------------
        -- Convert the GHC Core module to Disciple Core.
        let (mm_dc, failsConvert) = convertModGuts guts

        writeFile ("dump." ++ name ++ ".02-dc-raw.dcf")
         $ D.renderIndent (D.ppr mm_dc)

        writeFile ("dump." ++ name ++ ".02-dc-raw.fails")
         $ D.renderIndent (D.vcat $ intersperse D.empty $ map D.ppr failsConvert)


        -- Detect -----------------------------------------
        -- Detect flow operators and primitives.
        --  We also get a map of DDC names to GHC names
        let (mm_detect, names) = detectModule mm_dc

        writeFile ("dump." ++ name ++ ".03-dc-detect.dcf")
         $ D.render D.RenderIndent (D.ppr mm_detect)

        writeFile ("dump." ++ name ++ ".03-dc-detect.names")
         $ D.renderIndent (D.vcat $ map D.ppr $ Map.toList names)


        -- Norm -------------------------------------------
        -- Eta expand everything so we have names for parameters.
        let etaConfig   = Eta.configZero { Eta.configExpand = True }
        let mm_eta      = Core.result $ Eta.etaModule etaConfig Flow.profile mm_detect

        -- A-normalize module for the Prep transform.
        let mkNamT   = Core.makeNamifier Flow.freshT
        let mkNamX   = Core.makeNamifier Flow.freshX

        --  Snip and flatten the code to create new let-bindings
        --  for flow combinators. This ensures all the flow combinators
        --  and workers are bound at the top-level of the function.
        let snipConfig  = Snip.configZero { Snip.configSnipLetBody = True }
        let mm_snip'    = Core.flatten $ Snip.snip snipConfig mm_eta
        let mm_snip     = evalState (Core.namifyUnique mkNamT mkNamX mm_snip') 0

        writeFile ("dump." ++ name ++ ".04-dc-norm.dcf")
         $ D.render D.RenderIndent (D.ppr mm_snip)


        -- Prep -------------------------------------------
        --  2. Eta-expand worker functions passed to flow combinators.
        --     We also get back a map containing the types of parameters
        --     to worker functions.
        --  NOTE: We're not using the module result of prep now that 
        --        we have real eta-expansion.
        let (_, workerNameArgs) 
                        = Flow.prepModule mm_snip

        --  3. Move worker functions forward so they are directly
        --     applied to flow combinators.
        let isFloatable lts
             = case lts of
                LLet _ (BName n _) _    
                  | Just{}       <- Map.lookup n workerNameArgs
                  -> Core.FloatForce
                _ -> Core.FloatAllow

        let result_forward      = Core.forwardModule Flow.profile 
                                        isFloatable mm_snip
        
        let mm_forward          = Core.result result_forward

        writeFile ("dump." ++ name ++ ".05-dc-prep.1-forward.dcf")
         $ D.render D.RenderIndent (D.ppr mm_forward)


        --  4. Create fresh names for anonymous binders.
        --     The lowering pass needs them all to have real names.
        let mm_namify   = evalState (Core.namifyUnique mkNamT mkNamX mm_forward) 0

        writeFile ("dump." ++ name ++ ".05-dc-prep.2-namify.dcf")
         $ D.render D.RenderIndent (D.ppr mm_namify)

        --  5. Type check add type annots on all binders.
        let mm_prep     = checkFlowModule_ mm_namify

        writeFile ("dump." ++ name ++ ".05-dc-prep.3-check.dcf")
         $ D.renderIndent (D.ppr mm_prep)


        -- Lower ------------------------------------------
        -- Slurp out flow processes from the preped module.
        let processes   = Flow.slurpProcesses mm_prep

        -- Schedule processes into abstract loops.
        let procs       = map Flow.scheduleProcess processes

        -- Extract concrete code from the abstract loops.
        let mm_lowered' = Flow.extractModule mm_prep procs
        let mm_lowered  = evalState (Core.namifyUnique mkNamT mkNamX mm_lowered') 0

        writeFile ("dump." ++ name ++ ".06-dc-lowered.1-processes.txt")
         $ D.renderIndent $ D.vcat $ intersperse D.empty $ map D.ppr $ processes

        writeFile ("dump." ++ name ++ ".06-dc-lowered.dcf")
         $ D.renderIndent $ D.ppr mm_lowered


        -- Concretize ------------------------------------
        -- Concretize rate variables.
        let mm_concrete = Flow.concretizeModule mm_lowered

        writeFile ("dump." ++ name ++ ".07-dc-concrete.dcf")
         $ D.renderIndent $ D.ppr mm_concrete


        -- Storage ---------------------------------------
        -- -- Assign mutable variables to array storage.
        -- let mm_storage  = Flow.storageModule mm_concrete

        -- writeFile ("dump." ++ name ++ ".08-dc-storage.dcf")
        --  $ D.renderIndent $ D.ppr mm_storage
        let mm_storage  = mm_concrete


        -- Check -----------------------------------------
        -- Type check the module,
        --  the thread transform wants type annotations at each node.
        let mm_checked  = checkFlowModule mm_storage

        writeFile ("dump." ++ name ++ ".09-dc-checked.dcf")
         $ D.renderIndent $ D.ppr mm_checked


        -- Thread -----------------------------------------
        -- Thread the World# token through stateful functions in preparation
        -- for conversion back to GHC core.
        let mm_thread'  = Core.thread Flow.threadConfig Env.empty Env.empty mm_checked
        let mm_thread   = evalState (Core.namifyUnique mkNamT mkNamX mm_thread') 0

        writeFile ("dump." ++ name ++ ".10-dc-threaded.dcf")
         $ D.renderIndent $ D.ppr mm_thread


        -- Splice -----------------------------------------
        -- Splice the lowered functions back into the GHC core program.
        let guts'       = G.initUs_ us2 
                        $ spliceModGuts primitives names mm_thread guts

        writeFile ("dump." ++ name ++ ".11-ghc-spliced.dcf")
         $ D.render D.RenderIndent (pprModGuts guts')

        return (return guts')


-- | Type check a Core Flow module
checkFlowModule_ 
        :: Core.Module () Flow.Name 
        -> Core.Module () Flow.Name

checkFlowModule_ mm
        = Core.reannotate Core.annotTail 
        $ checkFlowModule mm


-- | Type check a Core Flow module, producing type annotations on every node.
checkFlowModule 
        :: Core.Module () Flow.Name 
        -> Core.Module (Core.AnTEC () Flow.Name) Flow.Name

checkFlowModule mm
 = let  result  = Core.checkModule 
                        (Core.configOfProfile Flow.profile)
                        mm
   in   case result of
         Right mm'      -> mm'
         Left err
          -> error $ D.renderIndent $ D.indent 8 $ D.vcat
                   [ D.empty
                   , D.text "repa-plugin:"
                   , D.indent 2 
                        $ D.vcat [ D.text "Type error in generated code"
                                 , D.ppr err ] ]


