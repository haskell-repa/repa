
module Data.Repa.Flow.IO.Storable 
        ( Storable      (..))
where
import Data.Repa.Array.Material         as A
import Data.Repa.Array.Material.Foreign as A
import Data.Repa.Array                  as A
import Data.Repa.Flow.IO.Bucket         as F
import Data.Repa.Flow                   as F
import qualified Data.Repa.Flow.Generic as G
import qualified Foreign.Storable       as S
import Data.Int


-------------------------------------------------------------------------------
-- | Class of element types that we can load and store to the file system.
--
--   Each element type is associated with a native representation, 
--   which requires 
--
class Bulk (Rep a) a
   => Storable a where

 data Spec a   
 type Rep  a

 -- | Read an array of the given length form a bucket.
 --   If the store does not contain a whole number of elements then `Nothing`.
 sGetArray
        :: Spec  a              -- ^ Element specification.
        -> Integer              -- ^ Number of elements to read.
        -> Bucket               -- ^ Bucket to read from.
        -> IO (Maybe (Array (Rep a) a))

 -- | Write an array to a bucket.
 sPutArray
        :: Spec  a              -- ^ Element specification.
        -> Bucket               -- ^ Bucket to write to.
        -> Array (Rep a) a      -- ^ Array to write.
        -> IO ()

-------------------------------------------------------------------------------
instance Storable Int32  where
 data Spec Int32  = SInt32
 type Rep  Int32  = F

 sGetArray SInt32 lenElems bucket
  = do  let lenElem     = S.sizeOf (undefined :: Int32)
        arr8    <- bGetArray bucket (lenElems * fromIntegral lenElem)

        -- TODO: check result array contains a whole number of elements.

        return  $ Just $ A.unsafeCast arr8

 sPutArray SInt32 bucket arr
  =     bPutArray bucket (A.unsafeCast arr)


-------------------------------------------------------------------------------
instance 
        ( Storable a, Storable b, S.Storable a, S.Storable b
        , Index (Rep a) ~ Index (Rep b))
        => Storable (a, b) where

 data Spec (a, b) = S2 (Spec a) (Spec b)
 type Rep  (a, b) = T2 (Rep  a) (Rep  b)

 sGetArray (S2 sA sB) lenElems bucket
  = do  let lenA = S.sizeOf (undefined :: a)
        let lenB = S.sizeOf (undefined :: b)       
        arr8    <- bGetArray bucket (lenElems * fromIntegral (lenA + lenB))

        return undefined
--        ... need to unpack into separate A and B chunks.


-------------------------------------------------------------------------------
-- Move to Data.Repa.Flow.Binary
sourceBinary 
        :: Storable a
        => Spec a                       -- ^ Specification of elements.
        -> Integer                      -- ^ Number of elements per chunk.
        -> Array B Bucket               -- ^ Buckets of table.
        -> IO (G.Sources Int IO (Array (Rep a) a))

sourceBinary spec lenElems bs
 = return $ G.Sources (A.length bs) pull_sourceBinary
 where
        pull_sourceBinary i eat eject
         = do   let b   = A.index bs i
                op      <- bIsOpen b
                if not op
                 then eject
                 else do
                  eof   <- bAtEnd b
                  if eof
                   then eject
                   else do
                        Just !chunk <- sGetArray spec lenElems b
                        eat chunk
        {-# INLINE pull_sourceBinary #-}
{-# INLINE_FLOW sourceBinary #-}
-- TODO: specialise this rather than inlining it.

