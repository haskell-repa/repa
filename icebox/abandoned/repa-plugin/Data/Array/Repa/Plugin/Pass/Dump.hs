
module Data.Array.Repa.Plugin.Pass.Dump
        (passDump)
where
import Data.Array.Repa.Plugin.GHC.Pretty
import DDC.Base.Pretty
import HscTypes
import CoreMonad
import System.IO.Unsafe
import Control.Monad


-- | Dump a module.
passDump :: [CommandLineOption] -> String -> ModGuts -> CoreM ModGuts
passDump options name guts
 = unsafePerformIO
 $ do
        when (elem "dump" options)
         $ writeFile ("dump." ++ name ++ ".hs")
         $ render RenderIndent (pprModGuts guts)

        return (return guts)
