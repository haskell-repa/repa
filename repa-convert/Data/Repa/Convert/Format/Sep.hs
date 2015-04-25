
module Data.Repa.Convert.Format.Sep
        (Sep (..))
where
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Data.Repa.Product
import Data.Word
import Data.Char
import qualified Foreign.Storable               as S
import qualified Foreign.Ptr                    as S


-- | Separate fields with the given character.
data Sep f
        = Sep  Char f
        deriving Show


instance Format (Sep ()) where
 type Value (Sep ())     = ()
 fieldCount (Sep _ _)    = 0
 minSize    (Sep _ _)    = 0
 fixedSize  (Sep _ _)    = return 0
 packedSize (Sep _ _) () = return 0
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance ( Format f1, Format (Sep fs)
         , Value (Sep fs) ~ Value fs)
        => Format (Sep (f1 :*: fs)) where

 type Value (Sep (f1 :*: fs)) 
        = Value f1 :*: Value fs

 fieldCount (Sep c (_  :*: fs))
  = 1 + fieldCount (Sep c fs)

 minSize    (Sep c (f1 :*: fs))
  = let !n      = fieldCount (Sep c fs)
    in  minSize f1
                + (if n == 0 then 0 else 1) 
                + minSize (Sep c fs)

 fixedSize  (Sep c (f1 :*: fs))
  = do  s1       <- fixedSize f1
        ss       <- fixedSize (Sep c fs)
        let sSep =  if fieldCount (Sep c fs) == 0 then 0 else 1
        return  $ s1 + sSep + ss

 packedSize (Sep c (f1 :*: fs)) (x1 :*: xs)
  = do  s1      <-  packedSize f1 x1
        ss      <-  packedSize (Sep c fs) xs
        let sSep =  if fieldCount (Sep c fs) == 0 then 0 else 1
        return  $ s1 + sSep + ss 
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable (Sep ()) where
 pack   _buf _fmt _val k = k 0
 unpack _buf _len _fmt k = k ((), 0)
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


instance ( Packable f1, Packable (Sep fs)
         , Value (Sep fs) ~ Value fs)
       => Packable (Sep (f1 :*: fs)) where

 pack   !buf    (Sep c (f1 :*: fs)) (x1 :*: xs) k
  |  fieldCount (Sep c fs) >= 1
  =  pack  buf                       f1          x1          $ \o1
  -> pack  (S.plusPtr buf  o1)       Word8be    (w8 $ ord c) $ \oc
  -> pack  (S.plusPtr buf (o1 + oc)) (Sep c fs)  xs          $ \os
  -> k (o1 + oc + os)

  | otherwise
  =  pack  buf                       f1          x1          $ \o1
  -> k o1
 {-# INLINE pack #-}


 unpack !buf !len (Sep c (f1 :*: fs)) k
  | n <- fieldCount (Sep c fs) 
  , n >= 1
  = findSep (w8 $ ord c) buf len $ \pos
  -> let
        -- The following size code should be evaluated statically via
        -- inlining and GHC simplifications.
        !s1 = minSize f1
        !ss = minSize (Sep c fs)

     in if   (s1 <= pos)
          && (s1 + 1 + ss <= len)
          then unpack  buf                       pos            f1       $ \(x1, o1)
            -> unpack (S.plusPtr buf (o1 + 1)) (len - o1 - 1) (Sep c fs) $ \(xs, os)
            -> k (x1 :*: xs, o1 + 1 + os)
          else return Nothing

  | otherwise
  =  unpack buf                len        f1         $ \(x1, o1)
  -> unpack (S.plusPtr buf o1) (len - o1) (Sep c fs) $ \(xs, os)
  -> k (x1 :*: xs, o1 + os)
 {-# INLINE unpack #-}


-- | Find the first occurrence of the given separating character in the
--   buffer, or `Nothing` if we don't find it before the buffer ends.
findSep :: Word8                  -- ^ Separating character.
        -> S.Ptr Word8            -- ^ Buffer.
        -> Int                    -- ^ Buffer length
        -> (Int -> IO (Maybe a))  -- ^ Continuation taking separator position.      
        -> IO (Maybe a)

findSep !sep !buf !len k 
 = loop_findSep 0
 where  
        loop_findSep !ix
         | ix >= len    
         = return Nothing

         | otherwise
         = do   x :: Word8  <- S.peekByteOff buf ix
                if x == sep
                 then k ix
                 else loop_findSep (ix + 1)
        {-# INLINE loop_findSep #-}
{-# INLINE findSep #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

