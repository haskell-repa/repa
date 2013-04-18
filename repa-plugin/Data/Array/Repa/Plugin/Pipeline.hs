
module Data.Array.Repa.Plugin.Pipeline 
        (vectoriserPipeline)
where
import Data.Array.Repa.Plugin.Pass.Dump
import GhcPlugins


-- DPH Compilation phase numbers. 
-- These are defined in dph-base:include/fusion-phases.h
dphPhaseClosures        = 6
dphPhasePA              = 5
dphPhasePar             = 4
dphPhaseDist            = 3
dphPhaseSeq             = 2
dphPhaseStream          = 1
dphPhaseInner           = 0


-- | Our vectoriser pipeline.
--   This replaces the standard compilation pipeline defined in 
--   SimplCore.lhs of the main compiler.
vectoriserPipeline :: [CoreToDo]
vectoriserPipeline
 = [    
        -- Our transform requires the desugared code to be pre-simplified
        CoreDoSimplify 10
                SimplMode 
                { sm_names      = ["Vectorise", "PreSimplify"]
                , sm_phase      = InitialPhase
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = False
                , sm_case_case  = False }

        -- Dump the simplified code
   ,    CoreDoPluginPass "Dump" (passDump "1-dump")


        ---------------------
        -- From this point onwards we've got the code expressed as
        -- imperative-style loops, and need to optimise this low-level code.

        -- Do Worker/Wrapper to try to eliminate leftover boxings and unboxings
        -- from recursive functions.
   ,    CoreDoStrictness
   ,    CoreDoWorkerWrapper
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Post Worker-Wrapper"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True }

        -- Do Constructor Specialisation.
        -- Data.Vector code relies on this.
   ,    CoreDoSpecConstr
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Post SpecConstr"]
                , sm_phase      = Phase 0
                , sm_rules      = False
                , sm_eta_expand = False
                , sm_inline     = False
                , sm_case_case  = True }

        -- Do Floating and CSE to shift unboxings outwards and combine common
        -- constants. 
   ,    CoreDoFloatOutwards
                FloatOutSwitches
                { floatOutLambdas               = Nothing
                , floatOutConstants             = False 
                , floatOutPartialApplications   = False }
   ,    CoreCSE
   ,    CoreDoFloatInwards

   ,    CoreDoStrictness
   ,    CoreDoStaticArgs

        -- Final simplification.
   ,    CoreDoSimplify 20
                SimplMode
                { sm_names      = ["Vectorise", "Final"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True }

   ,    CoreDoPluginPass "Dump"   (passDump "7-final")
   ]
