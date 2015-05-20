
module Data.Repa.Convert.Format.Sep
        (Sep (..))
where
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Data.Repa.Scalar.Product
import Data.Monoid
import Data.Word
import Data.Char
import qualified Foreign.Storable               as F
import qualified Foreign.Ptr                    as F
import Prelude hiding (fail)


-- | Separate fields with the given character.
-- 
--   * The separating character is un-escapable. 
--   * The format @(Sep ',')@ does NOT parse a CSV
--     file according to the CSV specification: http://tools.ietf.org/html/rfc4180.
--
data Sep f
        = Sep  Char f
        deriving Show


---------------------------------------------------------------------------------------------------
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


instance Packable (Sep ()) where
 pack   _fmt _val        = mempty
 unpack _fmt             = return ()
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
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


instance ( Packable f1, Packable (Sep fs)
         , Value (Sep fs) ~ Value fs)
       => Packable (Sep (f1 :*: fs)) where

 pack   (Sep c (f1 :*: fs)) (x1 :*: xs) 
  | fieldCount (Sep c fs) >= 1
  = pack f1 x1 <> pack Word8be (w8 $ ord c) <> pack (Sep c fs) xs

  | otherwise
  = pack f1 x1
 {-# INLINE pack #-}


 unpack (Sep c (f1 :*: fs)) 
  | fieldCount (Sep c fs) >= 1
  = Unpacker $ \start end fail eat
  -> let !len = F.minusPtr end start in  
     findSep (w8 $ ord c) start len fail $ \pos
      -> let -- The following size code should be evaluated statically via
             -- inlining and GHC simplifications.
             !s1 = minSize f1
             !ss = minSize (Sep c fs)

         in if  (s1 <= pos) && (s1 + 1 + ss <= len)
             then
                  (fromUnpacker $ unpack f1)             start     end fail $ \start_x1 x1
               -> let start_x1' = F.plusPtr start_x1 1 
                  in  (fromUnpacker $ unpack (Sep c fs)) start_x1' end fail $ \start_xs xs
                    -> eat start_xs (x1 :*: xs)
             else fail

  | otherwise
  =  Unpacker  $ \start end fail eat
  -> (fromUnpacker $ unpack f1)         start   end fail $ \start_x  x
  -> (fromUnpacker $ unpack (Sep c fs)) start_x end fail $ \start_xs xs
  -> eat start_xs (x :*: xs)
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Find the first occurrence of the given separating character in the
--   buffer, or `Nothing` if we don't find it before the buffer ends.
--
findSep :: Word8                  -- ^ Separating character.
        -> F.Ptr Word8            -- ^ Buffer.
        -> Int                    -- ^ Buffer length
        -> IO b                   -- ^ Signal failure
        -> (Int -> IO b)          -- ^ Continuation taking separator position.      
        -> IO b

findSep !sep !buf !len fail eat
 = loop_findSep 0
 where  
        loop_findSep !ix
         | ix >= len    
         = fail

         | otherwise
         = do x :: Word8  <- F.peekByteOff buf ix
              if x == sep
               then eat ix
               else loop_findSep (ix + 1)
        {-# INLINE loop_findSep #-}
{-# INLINE findSep #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

