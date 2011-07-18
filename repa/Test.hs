
module Main where

import Text.Printf
import Test.QuickCheck
import Data.Array.Repa.Properties

main :: IO ()
main
  = mapM_ test props_DataArrayRepa


test :: Testable prop => (String, prop) -> IO ()
test (name, prop)
  = printf "%-58s: " name >> quickCheck (within 5000000 prop)

