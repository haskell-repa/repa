
module Data.Array.Repa.Plugin.Pass.Lower
        (passLower)
where
import Data.Array.Repa.Plugin.Convert.ToDDC
import Data.Array.Repa.Plugin.Convert.ToGHC
import Data.Array.Repa.Plugin.Convert.Detect
import Data.Array.Repa.Plugin.GHC.Pretty
import DDC.Core.Exp

import qualified DDC.Core.Flow                          as Flow
import qualified DDC.Build.Language.Flow                as Flow
import qualified DDC.Core.Flow.Transform.Prep           as Flow
import qualified DDC.Core.Flow.Transform.Slurp          as Flow
import qualified DDC.Core.Flow.Transform.Schedule       as Flow
import qualified DDC.Core.Flow.Transform.Extract        as Flow
import qualified DDC.Core.Flow.Transform.Storage        as Flow
import qualified DDC.Core.Flow.PrimState.Thread         as Flow

import qualified DDC.Core.Simplifier                    as Core
import qualified DDC.Core.Transform.Namify              as Core
import qualified DDC.Core.Transform.Snip                as Core
import qualified DDC.Core.Transform.Flatten             as Core
import qualified DDC.Core.Transform.Forward             as Core
import qualified DDC.Core.Transform.Thread              as Core
import qualified DDC.Type.Env                           as Env

import qualified HscTypes                               as G
import qualified CoreMonad                              as G
import qualified UniqSupply                             as G
import qualified DDC.Base.Pretty                        as D
import qualified Data.Map                               as Map
import System.IO.Unsafe
import Control.Monad.State.Strict


-- | Dump a module.
passLower :: String -> G.ModGuts -> G.CoreM G.ModGuts
passLower name guts
 = unsafePerformIO
 $ do
        -- Input ------------------------------------------
        writeFile ("dump." ++ name ++ ".1-ghc.hs")
         $ D.render D.RenderIndent (pprModGuts guts)


        -- Convert ----------------------------------------
        -- Convert the GHC Core module to Disciple Core.
        let mm_dc       = convertModGuts guts

        writeFile ("dump." ++ name ++ ".2-dc-raw.dcf")
         $ D.render D.RenderIndent (D.ppr mm_dc)


        -- Detect -----------------------------------------
        -- Detect flow operators and primitives.
        --  We also get a map of DDC names to GHC names
        let (mm_detect, names) = detectModule mm_dc

        writeFile ("dump." ++ name ++ ".3-dc-detect.dcf")
         $ D.render D.RenderIndent (D.ppr mm_detect)

        writeFile ("dump." ++ name ++ ".3-dc-detect.names")
         $ D.renderIndent (D.vcat $ map D.ppr $ Map.toList names)


        -- Prep -------------------------------------------
        -- Prep module for lowering.
        let namifierT   = Core.makeNamifier Flow.freshT Env.empty
        let namifierX   = Core.makeNamifier Flow.freshX Env.empty

        --  1. Snip and flatten the code to create new let-bindings
        --     for flow combinators. This ensures all the flow combinators
        --     and workers are bound at the top-level of the function.
        let mm_snip'    = Core.flatten $ Core.snip False mm_detect
        let mm_snip     = evalState (Core.namify namifierT namifierX mm_snip') 0

        writeFile ("dump." ++ name ++ ".4-dc-prep.1-snip.dcf")
         $ D.render D.RenderIndent (D.ppr mm_snip)


        --  2. Eta-expand worker functions passed to flow combinators.
        --     We also get back a map containing the types of parameters
        --     to worker functions.
        let (mm_prepanon, workerNameArgs) 
                        = Flow.prepModule mm_snip

        writeFile ("dump." ++ name ++ ".4-dc-prep.2-prepanon.dcf")
         $ D.render D.RenderIndent (D.ppr mm_prepanon)


        --  3. Move worker functions forward so they are directly
        --     applied to flow combinators.
        let isFloatable lts
             = case lts of
                LLet _ (BName n _) _    
                  | Just{}       <- Map.lookup n workerNameArgs
                  -> Core.FloatForce
                _ -> Core.FloatAllow

        let result_forward      = Core.forwardModule Flow.profile 
                                        isFloatable mm_prepanon
        let mm_forward          = Core.result result_forward

        writeFile ("dump." ++ name ++ ".4-dc-prep.3-forward.dcf")
         $ D.render D.RenderIndent (D.ppr mm_forward)


        --  4. Create fresh names for anonymous binders.
        --     The lowering pass needs them all to have real names.
        let mm_namify   = evalState (Core.namify namifierT namifierX mm_forward) 0

        writeFile ("dump." ++ name ++ ".4-dc-prep.4-namify.dcf")
         $ D.render D.RenderIndent (D.ppr mm_namify)

        let mm_prep     = mm_namify


        -- Lower ------------------------------------------
        -- Slurp out flow processes from the preped module.
        let processes   = Flow.slurpProcesses mm_prep

        -- Schedule processes into abstract loops.
        let procs       = map Flow.scheduleProcess processes

        -- Extract concrete code from the abstract loops.
        let mm_lowered' = Flow.extractModule mm_prep procs
        let mm_lowered  = evalState (Core.namify namifierT namifierX mm_lowered') 0

        writeFile ("dump." ++ name ++ ".5-dc-lowered.dcf")
         $ D.renderIndent $ D.ppr mm_lowered


        -- Storage ---------------------------------------
        -- Assign mutable variables to array storage.
        let mm_storage  = Flow.storageModule mm_lowered

        writeFile ("dump." ++ name ++ ".6-dc-storage.dcf")
         $ D.renderIndent $ D.ppr mm_storage


        -- Thread -----------------------------------------
        -- Thread the World# token through stateful functions in preparation
        -- for conversion back to GHC core.
        let mm_thread'  = Core.thread Flow.threadConfig mm_storage
        let mm_thread   = evalState (Core.namify namifierT namifierX mm_thread') 0

        writeFile ("dump." ++ name ++ ".7-dc-threaded.dcf")
         $ D.renderIndent $ D.ppr mm_thread


        -- Splice -----------------------------------------
        -- Splice the lowered functions back into the GHC core program.
        us              <- G.mkSplitUniqSupply 's'              -- Here's hoping this is unique...
        let guts'       = G.initUs_ us (spliceModGuts names mm_thread guts)

        writeFile ("dump." ++ name ++ ".8-ghc-spliced.dcf")
         $ D.render D.RenderIndent (pprModGuts guts')

        return (return guts')
