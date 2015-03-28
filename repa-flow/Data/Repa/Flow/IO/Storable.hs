
module Data.Repa.Flow.IO.Storable
        ( Storable      (..)
        , Spec          (..))
where
import Data.Repa.Array.Meta                     as A
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Material                 as A
import Data.Repa.Array.Material.Foreign         as AF
import Data.Repa.Array.Material.Strided         as AS
import Data.Repa.Flow.IO.Bucket                 as F
import qualified Foreign.Storable               as S
import Data.Int
#include "repa-flow.h"


---------------------------------------------------------------------------------------------------
-- | Class of element types that we can load and store to the file system.
--
--   TODO: change to Persistable. 
-- 
class Bulk (Rep a) a
   => Storable a where

 -- | Specification of how the elements are arranged in the file.
 -- 
 --   For atomic elements the specification is the storage type.
 --
 --   For fixed-length arrays of elements, the specification contains the 
 --   element storage type as well as the array length.
 --   
 data Spec a   

 -- | Representation tag used for an array of these elements.
 --
 --   For atomic elements this will be `F`, for foreign arrays that 
 --   do not require extra copying after the data is read.
 --
 --   For tuples of elements, this will be a tuple of strided arrays,
 --   so the elements can also be used without copying.
 --
 type Rep  a

 -- | Get the size of a single element, in bytes.
 sizeElem :: Spec a -> Int

 -- | Read an array of the given length from a bucket.
 --   If the bucket does not contain a whole number of elements remaining 
 --   then `Nothing`.
 getArray
        :: Spec  a              -- ^ Element specification.
        -> Integer              -- ^ Number of elements to read.
        -> Bucket               -- ^ Bucket to read from.
        -> IO (Maybe (Array (Rep a) a))

 -- | Write an array to a bucket.
{-
 putArray
        :: Spec  a              -- ^ Element specification.
        -> Bucket               -- ^ Bucket to write to.
        -> Array (Rep a) a      -- ^ Array to write.
        -> IO ()
-}

---------------------------------------------------------------------------------------------------
-- | A native 32-bit integer.
instance Storable Int32  where
 data Spec Int32  = SInt32
 type Rep  Int32  = F

 sizeElem SInt32  = 4
 {-# INLINE_FLOW sizeElem #-}

 getArray SInt32 lenElems bucket
  = do  let bytesElem   = sizeElem SInt32
        arr8            <- bGetArray bucket (lenElems * fromIntegral bytesElem)
        let (startBytes, lenBytes, fptrBuf)
                        = AF.toForeignPtr arr8
        let lenElems'   = lenBytes `div` sizeElem SInt32

        if  (startBytes /= 0)
         || (lenBytes   `mod` bytesElem /= 0)
         then return Nothing
         else return $ Just  
                $ AF.unsafeCast
                $ AF.fromForeignPtr lenElems' fptrBuf



---------------------------------------------------------------------------------------------------
-- | Two elements stored consecutively.
instance 
        (   Storable a,   Storable b
        , S.Storable a, S.Storable b)
        => Storable (a, b) where

 data Spec (a, b) = S2 (Spec a) (Spec b)
 type Rep  (a, b) = T2 S S

 sizeElem (S2 sA sB)
  = sizeElem sA + sizeElem sB
 {-# INLINE_FLOW sizeElem #-}

 getArray (S2 sA sB) lenElems bucket
  = do  let bytesA      = sizeElem sA
        let bytesB      = sizeElem sB
        let bytesTuple  = bytesA + bytesB

        -- Read an array containing raw bytes.
        let lenBytes    = lenElems * fromIntegral bytesTuple
        arr8    <- bGetArray bucket lenBytes

        let lenBytes'   = A.length arr8
        let lenElems'   = lenBytes' `div` bytesTuple

        -- Check that we have received a whole number of elements.
        if lenBytes' `mod` bytesTuple /= 0
         then return Nothing
         else do
                let (startBuf, _lenBuf, fptrBuf) 
                         = AF.toForeignPtr arr8

                let arrA = AS.unsafeCast
                         $ AS.fromForeignPtr 
                                startBuf 
                                bytesTuple (fromIntegral lenElems') fptrBuf

                let arrB = AS.unsafeCast
                         $ AS.fromForeignPtr 
                                (startBuf + bytesA)
                                bytesTuple (fromIntegral lenElems') fptrBuf

                return $ Just $ T2Array arrA arrB


