
module Data.Repa.Array.Material.Auto.Operator.Lines
        (findRowsStartEnd)
where
import Data.Repa.Array.Material.Auto.Base
import Data.Repa.Array.Material.Auto.InstInt                    ()
import Data.Repa.Array.Material.Auto.InstWord
import Data.Repa.Array.Material.Auto.InstTuple

import qualified Data.Repa.Array.Generic                        as G
import qualified Data.Repa.Array.Generic.Target                 as G
import qualified Data.Repa.Array.Generic.Index                  as G

import qualified Data.Repa.Stream                               as S
import qualified Data.Repa.Eval.Stream                          as S
import qualified Data.Vector.Fusion.Stream.Monadic              as S

import Data.Word
import System.IO.Unsafe

-- | Given a row ending byte and a buffer containing encoded rows,
--   get buffers of row starting and ending indices.
findRowsStartEnd
        :: Word8
        -> G.Array A Word8
        -> (G.Buffer A Int, G.Buffer A Int)

findRowsStartEnd wSepLine (AArray_Word8 arrWords)
 = unsafePerformIO
 $ do
        let len = G.extent $ G.layout arrWords

        let (AArray_T2 (T2Array arr1 arr2))
                = S.unstreamToArray A
                $ S.findSegmentsS (const True) (== wSepLine) (len - 1)
                $ S.indexed
                $ S.streamOfArray
                $ arrWords

        buf1    <- G.unsafeThawBuffer arr1
        buf2    <- G.unsafeThawBuffer arr2

        return (buf1, buf2)
{-# NOINLINE findRowsStartEnd #-}