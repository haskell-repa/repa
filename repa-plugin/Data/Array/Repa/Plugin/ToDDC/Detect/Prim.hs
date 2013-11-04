
module Data.Array.Repa.Plugin.ToDDC.Detect.Prim
        ( detectPrimTyConName
        , detectPrimArithName )
where
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Type.Exp
import Data.List

-------------------------------------------------------------------------------
-- | Get the name, and kind for a primitive type constructor.
--
--   Done by dodgy string matching on the GHC symbol name.
--
detectPrimTyConName
        :: String
        -> Maybe (Name, Kind Name)

detectPrimTyConName str
 | Just tcn     <- detectPrimTyCon str  
 = Just (NamePrimTyCon tcn, kindPrimTyCon tcn)

 | Just tcn     <- detectTyConFlow str  
 = Just (NameTyConFlow tcn, kindTyConFlow tcn)

 | otherwise    = Nothing


-- | Detect a primitive type constructor name.
detectPrimTyCon :: String -> Maybe PrimTyCon
detectPrimTyCon str
 | Just (_, tcf) <- find (\(s, _) -> isPrefixOf s str) table    
                = Just tcf

 | otherwise    = Nothing
 where  
   table
    =   [ ("Bool_",     PrimTyConBool)
        , ("Int_",      PrimTyConInt)
        , ("Word_",     PrimTyConNat)
        , ("Float_",    PrimTyConFloat 32)
        , ("Double_",   PrimTyConFloat 64) ]


-- | Detect a Flow type constructor name.
detectTyConFlow :: String -> Maybe TyConFlow
detectTyConFlow str
 | Just (_, tcf) <- find (\(s, _) -> isPrefixOf s str) table    
                 = Just tcf

 -- N-tuples: (,)_ etc. Holds one more than the number of commas
 | '(':rest             <- str
 , (commas,aftercommas) <- span (==',') rest
 , isPrefixOf ")_" aftercommas
 , size                 <- length commas + 1
 = Just $ TyConFlowTuple size

 | otherwise     = Nothing
 where  
   table
     =  [ ("RateNat_",  TyConFlowRateNat)
        , ("Vector_",   TyConFlowVector)
        , ("Series_",   TyConFlowSeries)
        , ("Ref_",      TyConFlowRef)
        , ("Sel1",      TyConFlowSel 1)
        , ("Process",   TyConFlowProcess) ]


-------------------------------------------------------------------------------
-- | Get the name, instance type, and type scheme for one of GHC's
--   primitive arithmetic operators. 
--
--   Done by dodgy string matching on the GHC symbol name.
--
detectPrimArithName
        :: String 
        -> Maybe (Name, Type Name, Type Name)

detectPrimArithName str
 | Just (_, (prim, ty)) <- find (\(s, _) -> isPrefixOf s str) table
 = Just (NamePrimArith prim, ty, typePrimArith prim)

 | otherwise
 = Nothing

 where
  table 
   =    -- Int
        [ ("$fNumInt_$c+_",             (PrimArithAdd,  tInt))
        , ("$fNumInt_$c-_",             (PrimArithSub,  tInt))
        , ("$fNumInt_$c*_",             (PrimArithMul,  tInt))
        , ("$fIntegralInt_$cdiv_",      (PrimArithDiv,  tInt))
        , ("$fIntegralInt_$crem_",      (PrimArithRem,  tInt))
        , ("$fIntegralInt_$cmod_",      (PrimArithMod,  tInt))
        , ("eqInt_",                    (PrimArithEq,   tInt))
        , ("gtInt_",                    (PrimArithGt,   tInt))
        , ("ltInt_",                    (PrimArithLt,   tInt)) 

        -- Float
        , ("plusFloat_",                (PrimArithAdd,  tFloat 32))
        , ("minusFloat_",               (PrimArithSub,  tFloat 32))
        , ("timesFloat_",               (PrimArithMul,  tFloat 32))
        , ("eqFloat_",                  (PrimArithEq,   tFloat 32))
        , ("gtFloat_",                  (PrimArithGt,   tFloat 32))
        , ("ltFloat_",                  (PrimArithLt,   tFloat 32)) 

        -- Double
        , ("$fNumDouble_$c+_",          (PrimArithAdd,  tFloat 64))
        , ("$fNumDouble_$c-_",          (PrimArithSub,  tFloat 64))
        , ("$fNumDouble_$c*_",          (PrimArithMul,  tFloat 64))
        , ("eqDouble_",                 (PrimArithEq,   tFloat 64))
        , ("gtDouble_",                 (PrimArithGt,   tFloat 64))
        , ("ltDouble_",                 (PrimArithLt,   tFloat 64)) ]

