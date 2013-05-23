
module Data.Array.Repa.Plugin.Pipeline 
        (vectoriserPipeline)
where
import Data.Array.Repa.Plugin.Pass.Dump
import Data.Array.Repa.Plugin.Pass.Lower
import GhcPlugins


-- DPH Compilation phase numbers. 
-- These are defined in dph-base:include/fusion-phases.h


-- | Our vectoriser pipeline.
--
--   Inject the lowering transform just after the first simplification stage,
--   or add a simplification and lowering at the end if there is none.
--
vectoriserPipeline :: [CoreToDo] -> [CoreToDo]
vectoriserPipeline todos
 -- If an initial simplifier exists, lower straight afterwards
 | (before, (simp:after)) <- break findPreSimplifier todos
 = before ++ [simp] ++ lower ++ after ++ dump
 -- There is no simplifier (eg not compiled with -O)
 -- So add our own at the end
 | otherwise
 = todos ++ preSimp ++ lower ++ dump
 where
  findPreSimplifier c
   = case c of
     CoreDoSimplify _
      SimplMode { sm_phase = InitialPhase }
      -> True
     _
      -> False

  preSimp
   = [ CoreDoSimplify 10
                SimplMode 
                { sm_names      = ["Vectorise", "PreSimplify"]
                , sm_phase      = InitialPhase
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = False
                , sm_case_case  = False } ]

  -- Dump the simplified code,
  -- then lower the series expressions in the code.
  lower
   = [ CoreDoPluginPass "Dump" (passDump "1-dump")
     , CoreDoPluginPass "Lower" (passLower "2-lower") ]

  -- Dump the final result, after GHC optimises the lowered code
  dump
   = [ CoreDoPluginPass "Dump"   (passDump "3-final") ]

