
module Data.Array.Repa.Plugin.ToDDC.Detect
        (detectModule)
where
import Data.Array.Repa.Plugin.FatName
import Data.Array.Repa.Plugin.ToDDC.Detect.Base
import Data.Array.Repa.Plugin.ToDDC.Detect.Type  ()

import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds

import Control.Monad.State.Strict

import qualified Data.Map       as Map
import Data.Map                 (Map)
import Data.List


detectModule 
        :: Module a FatName 
        -> (Module a Name, Map Name GhcName)

detectModule mm
 = let  (mm', state')    = runState (detect mm) $ zeroState
   in   (mm', stateNames state')



-- Module ---------------------------------------------------------------------
instance Detect (Module a) where
 detect mm
  = do  body'   <- detect (moduleBody mm)
        return  $ ModuleCore
                { moduleName            = moduleName mm
                , moduleExportKinds     = Map.empty
                , moduleExportTypes     = Map.empty
                , moduleImportKinds     = Map.empty
                , moduleImportTypes     = Map.empty
                , moduleBody            = body' }


-- DaCon ----------------------------------------------------------------------
instance Detect DaCon where
 detect (DaCon dcn t isAlg)
  = do  dcn'    <- detect dcn
        t'      <- detect t
        return  $  DaCon dcn' t' isAlg


instance Detect DaConName where
 detect dcn
  = case dcn of
        DaConUnit       
         -> return DaConUnit

        DaConNamed (FatName g d)
         -> do  collect d g
                return $ DaConNamed d


-- Exp ------------------------------------------------------------------------
instance Detect (Exp a) where
 detect xx

  -- Set kind of detected rate variables to Rate.
  | XLam a b x          <- xx
  = do  b'      <- detect b
        x'      <- detect x
        case b' of
         BName n _
          -> do rateVar <- isRateVar n
                if rateVar 
                 then return $ XLAM a (BName n kRate) x'
                 else return $ XLam a b' x'

         _ -> error "repa-plugin.detect[Exp] no match"

  -- Detect vectorOfSeries
  | XApp a _ _                           <- xx
  , Just  (XVar _ u,     [xTK, xTA, _xD, xS]) 
                                        <- takeXApps xx
  , UName (FatName _ (NameVar v))        <- u
  , isPrefixOf "toVector_" v
  = do  args'   <- mapM detect [xTK, xTA, xS]
        return  $ xApps a (XVar a (UPrim (NameOpFlow OpFlowVectorOfSeries)
                                         (typeOpFlow OpFlowVectorOfSeries)))
                          args'

  -- Detect folds.
  | XApp a _ _                          <- xx
  , Just  (XVar _ uFold, [xTK, xTA, xTB, _xD, xF, xZ, xS])    
                                        <- takeXApps xx
  , UName (FatName _ (NameVar vFold))   <- uFold
  , isPrefixOf "fold_" vFold
  = do  args'   <- mapM detect [xTK, xTA, xTB, xF, xZ, xS]
        return  $  xApps a (XVar a (UPrim (NameOpFlow OpFlowFold) 
                                          (typeOpFlow OpFlowFold)))
                           args'

  -- Detect maps
  | XApp a _ _                          <- xx
  , Just  (XVar _ uMap,  [xTK, xTA, xTB, _xD1, _xD2, xF, xS ])
                                        <- takeXApps xx
  , UName (FatName _ (NameVar vMap))    <- uMap
  , isPrefixOf "map_" vMap
  = do  args'   <- mapM detect [xTK, xTA, xTB, xF, xS]
        return  $ xApps a (XVar a (UPrim (NameOpFlow (OpFlowMap 1))
                                         (typeOpFlow (OpFlowMap 1))))
                          args'

  -- Detect 2-tuples
  | XApp a _ _                          <- xx
  , Just  (XVar _ uTuple,  [xTA, xTB, xA, xB ])
                                        <- takeXApps xx
  , UName (FatName _ (NameVar vTuple))  <- uTuple
  , isPrefixOf "(,)_" vTuple
  = do  args'   <- mapM detect [xTA, xTB, xA, xB]
        return  $ xApps a (XCon a dcTuple2)
                          args'



  -- Inject type arguments for arithmetic ops.
  --   In the Core code, arithmetic operations are expressed as monomorphic
  --   dictionary methods, which we convert to polytypic DDC primops.
  | XVar a (UName (FatName nG (NameVar str)))    <- xx
  , Just (nD', tArg, tPrim)  <- matchPrimArith str
  = do  collect nD' nG
        return  $ xApps a (XVar a (UPrim nD' tPrim)) [XType tArg]


  -- Strip boxing constructors from literal values.
  | XApp _ (XVar _ (UName (FatName _ (NameCon str1)))) x2 <- xx
  , isPrefixOf "I#_" str1
  = detect x2

  
  -- Boilerplate traversal.
  | otherwise
  = case xx of
        XVar  a u       -> liftM2 XVar  (return a) (detect u)
        XCon  a u       -> liftM2 XCon  (return a) (detect u)
        XLAM  a b x     -> liftM3 XLAM  (return a) (detect b)   (detect x)
        XLam  a b x     -> liftM3 XLam  (return a) (detect b)   (detect x)
        XApp  a x1 x2   -> liftM3 XApp  (return a) (detect x1)  (detect x2)
        XLet  a lts x   -> liftM3 XLet  (return a) (detect lts) (detect x)
        XType t         -> liftM  XType (detect t)

        XCase{}         -> error "repa-plugin.detect: XCase not handled"
        XCast{}         -> error "repa-plugin.detect: XCast not handled"
        XWitness{}      -> error "repa-plugin.detect: XWitness not handled"


-- Match arithmetic operators.
matchPrimArith :: String -> Maybe (Name, Type Name, Type Name)
matchPrimArith str
 | isPrefixOf "$fNumInt_$c+_" str       
 = Just (NamePrimArith PrimArithAdd, tInt, typePrimArith PrimArithAdd)

 | isPrefixOf "$fNumInt_$c*_" str
 = Just (NamePrimArith PrimArithMul, tInt, typePrimArith PrimArithMul)

 | otherwise
 = Nothing


--- Lets ----------------------------------------------------------------------
instance Detect (Lets a) where
 detect ll
  = case ll of
        LLet _ b x      
         -> do  b'      <- detect b
                x'      <- detect x
                return  $ LLet LetStrict b' x'

        LRec bxs        
         -> do  let (bs, xs) = unzip bxs
                bs'     <- mapM detect bs
                xs'     <- mapM detect xs
                return  $ LRec $ zip bs' xs'

        LLetRegions{}   -> error "repa-plugin.detect: LLetRegions not handled"
        LWithRegion{}   -> error "repa-plugin.detect: LWithRegions not handled"

