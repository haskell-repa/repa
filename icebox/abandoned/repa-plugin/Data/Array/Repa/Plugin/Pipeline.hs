
module Data.Array.Repa.Plugin.Pipeline 
        (vectoriserPipeline)
where
import Data.Array.Repa.Plugin.Pass.Dump
import Data.Array.Repa.Plugin.Pass.Lower
import GhcPlugins


-- | Our vectoriser pipeline.
--
--   Inject the lowering transform just after the first simplification stage,
--   or add a simplification and lowering at the end if there is none.
--
vectoriserPipeline :: [CommandLineOption] -> [CoreToDo] -> [CoreToDo]
vectoriserPipeline options todos
 -- If an initial simplifier exists, lower straight afterwards
 | (before, (simp:after)) <- break isPreSimplifier todos
 = before ++ [simp] ++ todoLower options ++ after ++ todoDump options

 -- There is no simplifier (eg not compiled with -O)
 -- So add our own at the end
 | otherwise
 = todos ++ todoPreSimplifier ++ todoLower options ++ todoDump options


-- Do our own pre simplification.
todoPreSimplifier :: [CoreToDo]
todoPreSimplifier
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
todoLower :: [CommandLineOption] -> [CoreToDo]
todoLower options
 =      [ CoreDoPluginPass "Dump"  (passDump  options "1-dump")
        , CoreDoPluginPass "Lower" (passLower options "2-lower") ]


-- Dump the final result, after GHC optimises the lowered code
todoDump  :: [CommandLineOption] -> [CoreToDo]
todoDump options
 =      [ CoreDoPluginPass "Dump"   (passDump options "3-final") ]


-- | Check if a `CoreToDo` looks like the pre-simplifier.
isPreSimplifier :: CoreToDo -> Bool
isPreSimplifier c
 = case c of
        CoreDoSimplify _ SimplMode { sm_phase = InitialPhase }
          -> True
        _ -> False


