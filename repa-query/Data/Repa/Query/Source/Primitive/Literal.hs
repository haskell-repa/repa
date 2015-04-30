{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Query.Source.Primitive.Literal () where
import Data.Repa.Query.Source.Builder                   as S
import Data.Repa.Query.Graph                            as G
import Data.Repa.Query.Graph.Compounds                  as G
import Data.Word
import Data.Int

instance Num (Value Word) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt x))


instance Num (Value Float) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LFloat (fromIntegral x)))


instance Num (Value Double) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LFloat (fromIntegral x)))


instance Num (Value Word8) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int8) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Word16) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int16) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Word32) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int32) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Word64) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))


instance Num (Value Int64) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = makeScalarOp1 G.SopNeg
 abs            = makeScalarOp1 G.SopAbs
 signum         = makeScalarOp1 G.SopSignum
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt (fromIntegral x)))




