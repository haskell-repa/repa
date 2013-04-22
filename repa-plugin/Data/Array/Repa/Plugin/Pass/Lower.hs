
module Data.Array.Repa.Plugin.Pass.Lower
        (passLower)
where
import Data.Array.Repa.Plugin.Convert.ToDDC
import Data.Array.Repa.Plugin.Convert.Detect
import Data.Array.Repa.Plugin.GHC.Pretty

import qualified DDC.Build.Language.Flow                as Flow
import qualified DDC.Core.Flow.Transform.Prep           as Flow
import qualified DDC.Core.Flow.Transform.Slurp          as Flow
import qualified DDC.Core.Flow.Transform.Schedule       as Flow
import qualified DDC.Core.Flow.Transform.Extract        as Flow

import qualified DDC.Core.Transform.Namify              as Core
import qualified DDC.Core.Transform.Snip                as Core
import qualified DDC.Core.Transform.Flatten             as Core
import qualified DDC.Type.Env                           as Env

import qualified HscTypes                               as G
import qualified CoreMonad                              as G
import qualified DDC.Base.Pretty                        as D
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
        let mm_detect   = detectModule mm_dc

        writeFile ("dump." ++ name ++ ".3-dc-detect.dcf")
         $ D.render D.RenderIndent (D.ppr mm_detect)

        -- Prep -------------------------------------------
        -- Prep module for lowering.
        let namifierT   = Core.makeNamifier Flow.freshT Env.empty
        let namifierX   = Core.makeNamifier Flow.freshX Env.empty

        --  1. Snip and flatten the code to create new let-bindings
        --     for flow combinators.
        let mm_snip'    = Core.flatten $ Core.snip False mm_detect
        let mm_snip     = evalState (Core.namify namifierT namifierX mm_snip') 0

        --  2. Eta-expand worker functions passed to flow combinators.
        let mm_prepanon = Flow.prepModule mm_snip

        --  3. Move worker functions forward so they are directly
        --     applied to flow combinators.

        --  4. Create fresh names for anonymous binders.
        --     The lowering pass needs them all to have real names.
        let namifierT   = Core.makeNamifier Flow.freshT Env.empty
        let namifierX   = Core.makeNamifier Flow.freshX Env.empty
        let mm_prep     = evalState (Core.namify namifierT namifierX mm_prepanon) 0

        writeFile ("dump." ++ name ++ ".4-dc-prep.dcf")
         $ D.render D.RenderIndent (D.ppr mm_prep)


        -- Lower ------------------------------------------
        -- Slurp out flow processes from the preped module.
        let processes   = Flow.slurpProcesses mm_prep

        -- Schedule processes into abstract loops.
        let procs       = map Flow.scheduleProcess processes

        -- Extract concrete code from the abstract loops.
        let mm_lowered  = Flow.extractModule mm_prep procs

        writeFile ("dump." ++ name ++ ".5-dc-lowered.dcf")
         $ D.render D.RenderIndent (D.ppr mm_lowered)

        return (return guts)
