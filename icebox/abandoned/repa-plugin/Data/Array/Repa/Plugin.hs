
-- | This GHC plugin performs Data Flow Fusion as described in the following paper:
--
--   > Data Flow Fusion with Series Expressions in Haskell
--   > Ben Lippmeier, Manuel Chakravarty, Gabriele Keller, Amos Robinson.
--   > Haskell Sympoium, 2013.
--
--   <http://www.cse.unsw.edu.au/~benl/papers/flow/flow-Haskell2013.pdf>
--
--   The user-facing API is defined by the repa-series package.
--
--   To run the transform on a program do something like:
--
--   > ghc -O2 -fplugin=Data.Array.Repa.Plugin --make Main.hs
--
--   To see intermediate code as it is transformed, pass the 'dump' flag to the plugin.
--
--   > ghc -O2 -fplugin=Data.Array.Repa.Plugin -fplugin-opt Data.Array.Repa.Plugin:dump --make Main.hs
--
--   There is example code at: <http://code.ouroborus.net/repa/repa-head/repa-plugin/test/>
--
--
--   This is an EXPERIMENTAL implementation that some CURRENT LIMITATIONS:
--
--   * Only supports Series of element types @Int@ and (@Int@, @Int@). 
--     You can't yet fuse code using the @Float@ type, or anything else.
--
--   * You can't use case-expressions in the worker functions passed
--     to combinators like @map@ and @fold@. 
-- 
--   * The plugin lacks support for many common list functions, 
--     such as @append@.
--
--   * If your code cannot be fused then you may get an unhelpful error message.
-- 
module Data.Array.Repa.Plugin 
        (plugin)
where
import Data.Array.Repa.Plugin.Pipeline
import GhcPlugins
import StaticFlags
import System.IO.Unsafe

-- | The Data Flow Fusion plugin.
plugin :: Plugin
plugin  = defaultPlugin 
        { installCoreToDos = install }


-- | Install a plugin into the GHC compilation pipeline.
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos
 = do   
        -- Initialize the staticflags so that we can pretty print core code.
        --   The pretty printers depend on static flags and will `error` if 
        --   we don't do this first.
        unsafePerformIO
         $ do   addOpt "-dsuppress-all"
                addOpt "-dsuppress-idinfo"
                addOpt "-dsuppress-uniques"
                addOpt "-dppr-case-as-let"
                addOpt "-dppr-cols200"

                initStaticOpts
                return (return ())

        -- Replace the standard GHC pipeline with our one.
        return (vectoriserPipeline options todos)

