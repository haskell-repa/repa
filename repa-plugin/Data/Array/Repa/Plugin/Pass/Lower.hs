
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
import qualified DDC.Core.Flow.Transform.Slurp          as Flow
import qualified DDC.Core.Flow.Transform.Concretize     as Flow
import qualified DDC.Core.Flow.Transform.Melt           as Flow
import qualified DDC.Core.Flow.Transform.Wind           as Flow
import qualified DDC.Core.Flow.Transform.Thread         as Flow

import qualified DDC.Core.Module                        as Core
import qualified DDC.Core.Check                         as Core
import qualified DDC.Core.Simplifier                    as Core
import qualified DDC.Core.Fragment                      as Core
import qualified DDC.Core.Transform.Namify              as Core
import qualified DDC.Core.Transform.Flatten             as Flatten
import qualified DDC.Core.Transform.Forward             as Forward
import qualified DDC.Core.Transform.Thread              as Core
import qualified DDC.Core.Transform.Reannotate          as Core
import qualified DDC.Core.Transform.Deannotate          as Core
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Core.Transform.Eta                 as Eta

import qualified HscTypes                               as G
import qualified CoreMonad                              as G
import qualified UniqSupply                             as G
import qualified DDC.Base.Pretty                        as D
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set
import System.IO.Unsafe
import Control.Monad.State.Strict                       as S
import Data.List


-- | We use this unique when generating fresh names.
--
--   If this is not actually unique relative to the rest of the compiler
--   then we're completely screwed.
--
--   GHC doesn't seem to have an API to generate unique prefixes.
--
letsHopeThisIsUnique    :: Char
letsHopeThisIsUnique    = 's'


-- | Run the lowering pass on this module.
passLower :: [G.CommandLineOption] -> String -> G.ModGuts -> G.CoreM G.ModGuts
passLower options name guts0
 = unsafePerformIO
 $ do
        -- Here's hoping this is really unique
        us      <- G.mkSplitUniqSupply letsHopeThisIsUnique

        -- Decide whether to dump intermediate files
        let shouldDump      = elem "dump" options
        let dump thing str  = when shouldDump 
                            $ writeFile ("dump." ++ name ++ "." ++ thing) str

        -- Input ------------------------------------------
        -- Dump the GHC core code that we start with.
        dump "01-ghc.hs" 
         $ D.renderIndent (pprModGuts guts0)


        -- Primitives -------------------------------------
        -- Build a table of expressions to access our primitives.
        let (Just (primitives, guts), us2) 
                = G.initUs us (slurpPrimitives guts0)


        -- Convert ----------------------------------------
        -- Convert the GHC Core module to Disciple Core.
        let (mm_dc, failsConvert) = convertModGuts guts

        dump "02-raw.dcf"
         $ D.renderIndent (D.ppr mm_dc)

        dump "02-raw.fails"
         $ D.renderIndent (D.vcat $ intersperse D.empty $ map D.ppr failsConvert)


        -- Detect -----------------------------------------
        -- Detect flow operators and primitives.
        --  We also get a map of DDC names to GHC names
        let (mm_detect, names) = detectModule mm_dc

        dump "03-detect.dcf"
         $ D.renderIndent (D.ppr mm_detect)

        dump "03-detect.names"
         $ D.renderIndent (D.vcat $ map D.ppr $ Map.toList names)


        -- Prep -------------------------------------------
        -- Eta expand everything so we have names for parameters.
        let etaConfig   = Eta.configZero { Eta.configExpand = True }
        let mm_eta      = Core.result $ Eta.etaModule Flow.profile etaConfig mm_detect

        dump "04-prep.1-eta.dcf"
         $ D.renderIndent (D.ppr mm_eta)

        -- A-normalize module for the Prep transform.
        let mkNamT   = Core.makeNamifier Flow.freshT
        let mkNamX   = Core.makeNamifier Flow.freshX

        --  Snip and flatten the code to create new let-bindings
        --  for flow combinators. This ensures all the flow combinators
        --  and workers are bound at the top-level of the function.
        let snipConfig  = Snip.configZero { Snip.configSnipLetBody = True }
        let mm_snip'    = Flatten.flatten (Snip.snip snipConfig mm_eta)
        let mm_snip     = evalState (Core.namifyUnique mkNamT mkNamX mm_snip') 0

        dump "04-prep.2-snip.dcf"
         $ D.renderIndent (D.ppr mm_snip)

        --  Move worker functions forward so they are directly
        --  applied to flow combinators.
        let nsTop       = Core.moduleTopBinds mm_snip
        let isFloatable lts
             = case lts of
                LLet (BName n _) x
                  |   Set.member n nsTop
                   || Flow.isFlowOperator (Core.deannotate (const Nothing) x)
                  -> Forward.FloatDeny
                _ -> Forward.FloatForce

        let config              = Forward.Config isFloatable False
        let result_forward      = Forward.forwardModule Flow.profile config mm_snip
        
        let mm_forward          = Core.result result_forward

        dump "05-prep.3-float.dcf"
         $ D.renderIndent (D.ppr mm_forward)

        --  3. Create fresh names for anonymous binders.
        --     The lowering pass needs them all to have real names.
        let mm_namify   = evalState (Core.namifyUnique mkNamT mkNamX mm_forward) 0

        dump "05-prep.4-namify.dcf"
         $ D.renderIndent (D.ppr mm_namify)

        --  4. Type check add type annots on all binders.
        let mm_prep     = checkFlowModule_ mm_namify

        dump "05-prep.5-check.dcf"
         $ D.renderIndent (D.ppr mm_prep)


        -- Lower ------------------------------------------
        let mm_lowered_fail
                | elem "vector" options
                = Flow.lowerModule Flow.defaultConfigVector mm_prep

                | otherwise
                = Flow.lowerModule Flow.defaultConfigScalar mm_prep

        -- TODO: do something sensibler if we can't lower the code.
        let mm_lowered
                = case mm_lowered_fail of
                        Left fails      -> error $ show fails
                        Right mm'       -> mm'

        dump "06-lowered.dcf"
         $ D.renderIndent (D.ppr mm_lowered)


        -- Concretize ------------------------------------
        -- Concretize rate variables.
        let mm_concrete = Flow.concretizeModule mm_lowered

        dump "07-concrete.dcf"
         $ D.renderIndent (D.ppr mm_concrete)


        -- Melt ------------------------------------------
        -- Melt mutable references to tuples into references to their components.
        -- let mm_melt     = mm_concrete
        let (mm_melt, _) = Flow.meltModule mm_concrete
        dump "08-melt.dcf"
         $ D.renderIndent (D.ppr mm_melt)


        -- Wind ------------------------------------------
        -- Convert uses of the  loop# and guard# combinator to real tail-recursive
        -- loops.
        let mm_wind     = Core.result $ Forward.forwardModule Flow.profile 
                                (Forward.Config (const Forward.FloatAllow) True)
                        $ Core.result $ Forward.forwardModule Flow.profile 
                                (Forward.Config (const Forward.FloatAllow) True)
                        $ Core.result $ Forward.forwardModule Flow.profile 
                                (Forward.Config (const Forward.FloatAllow) True)
                        $ Core.result $ Forward.forwardModule Flow.profile 
                                (Forward.Config (const Forward.FloatAllow) True)
                        $ Flow.windModule mm_melt

        dump "09-wind.dcf"
         $ D.renderIndent (D.ppr mm_wind)


        -- Check -----------------------------------------
        -- Type check the module,
        --  the thread transform wants type annotations at each node.
        let mm_checked  = checkFlowModule mm_wind

        dump "10-checked.dcf"
         $ D.renderIndent (D.ppr mm_checked)


        -- Thread -----------------------------------------
        -- Thread the World# token through stateful functions in preparation
        -- for conversion back to GHC core.
        let mm_thread'  = Core.thread Flow.threadConfig 
                                (Core.profilePrimKinds Flow.profile)
                                (Core.profilePrimTypes Flow.profile)
                                mm_checked
        let mm_thread   = evalState (Core.namifyUnique mkNamT mkNamX mm_thread') 0

        dump "11-threaded.dcf"
         $ D.renderIndent (D.ppr mm_thread)


        -- Splice -----------------------------------------
        -- Splice the lowered functions back into the GHC core program.
        let guts'       = G.initUs_ us2 
                        $ spliceModGuts primitives names mm_thread guts

        dump "12-spliced.fc"
         $ D.renderIndent (pprModGuts guts')

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

