
import Test.QuickCheck
import Data.Array.Repa

main	= mapM_ test props_DataArrayRepa
	

test (name, prop)
 = do	putStr $ "Test " ++ name ++ "\n"
	putStr $ "     "
	quickCheck prop
	putStr $ "\n"
