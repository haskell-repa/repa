
module Data.Array.Repa.Plugin.Pass.Lower
        (passLower)
where
import Data.Array.Repa.Plugin.Convert.ToDDC
import Data.Array.Repa.Plugin.Convert.Detect
import Data.Array.Repa.Plugin.GHC.Pretty

import qualified HscTypes               as G
import qualified CoreMonad              as G

import qualified DDC.Base.Pretty        as D

import System.IO.Unsafe


-- | Dump a module.
passLower :: String -> G.ModGuts -> G.CoreM G.ModGuts
passLower name guts
 = unsafePerformIO
 $ do
        writeFile ("dump." ++ name ++ ".1-input-ghc.hs")
         $ D.render D.RenderIndent (pprModGuts guts)

        let ddcModule   = detectModule $ convertModGuts guts

        writeFile ("dump." ++ name ++ ".2-input-ddc.dcf")
         $ D.render D.RenderIndent (D.ppr ddcModule)

        return (return guts)
