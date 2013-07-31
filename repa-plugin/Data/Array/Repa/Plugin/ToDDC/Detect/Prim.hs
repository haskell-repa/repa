
module Data.Array.Repa.Plugin.ToDDC.Detect.Prim
        ( matchPrimArith)
where
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Type.Exp
import Data.List


-- | Get the name, instance type, and type scheme for one of GHC's
--   primitive arithmetic operators. 
--
--   Done by string matching on the GHC symbol name.
--
matchPrimArith 
        :: String 
        -> Maybe (Name, Type Name, Type Name)

matchPrimArith str
 | Just (_, (ty, prim)) <- find (\(s, _) -> isPrefixOf s str) primArithTable
 = Just (NamePrimArith prim, ty, typePrimArith prim)

 | otherwise
 = Nothing


primArithTable
 =      [ ("$fNumInt_$c+_",             (tInt, PrimArithAdd))
        , ("$fNumInt_$c-_",             (tInt, PrimArithSub))
        , ("$fNumInt_$c*_",             (tInt, PrimArithMul))
        , ("$fIntegralInt_$cdiv_",      (tInt, PrimArithDiv))
        , ("$fIntegralInt_$crem_",      (tInt, PrimArithRem))
        , ("$fIntegralInt_$cmod_",      (tInt, PrimArithMod))
        , ("eqInt_",                    (tInt, PrimArithEq))
        , ("gtInt_",                    (tInt, PrimArithGt))
        , ("ltInt_",                    (tInt, PrimArithLt)) ]

