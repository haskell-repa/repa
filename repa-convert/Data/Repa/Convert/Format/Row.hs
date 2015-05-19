
module Data.Repa.Convert.Format.Row
        (Row (..))
where
import Data.Repa.Convert.Format.Sep
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Data.Repa.Scalar.Product
import Data.Monoid
import Data.Word
import Data.Char


-- | Display fields as a human readable row.
data Row f
        = Row Char f
        deriving Show


---------------------------------------------------------------------------------------------------
instance Format (Row ()) where
 type Value (Row ())     = ()
 fieldCount (Row _ _)    = 0
 minSize    (Row _ _)    = 0
 fixedSize  (Row _ _)    = return 0
 packedSize (Row _ _) () = return 0
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable (Row ()) where
 pack   _fmt _val       = mempty
 unpack _fmt            = return ()
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
instance ( Format f1
         , Format (Row fs)
         , Format (Sep fs)
         , Value  (Sep fs) ~ Value fs)
        => Format (Row (f1 :*: fs)) where

 type Value (Row (f1 :*: fs)) 
        = Value f1 :*: Value fs

 fieldCount (Row c (_  :*: fs))
  = 1 + fieldCount (Row c fs)

 minSize    (Row c (f1 :*: fs))
  = let !n      = fieldCount (Row c fs)
    in  2 + minSize f1
          + (if n == 0 then 0 else 1) 
          + minSize (Sep '\t' fs)

 fixedSize  (Row c (f1 :*: fs))
  = do  s1      <- fixedSize f1
        ss      <- fixedSize (Sep c fs)
        let sSep = if fieldCount (Sep c fs) == 0 then 0 else 1
        return  $ 2 + s1 + sSep + ss

 packedSize (Row c (f1 :*: fs)) (x1 :*: xs)
  = do  s1      <- packedSize f1 x1
        ss      <- packedSize (Sep c fs) xs
        let sSep = if fieldCount (Sep c fs) == 0 then 0 else 1
        return  $ 2 + s1 + sSep + ss 
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance ( Packable f1
         , Packable (Sep fs)
         , Format (Row fs)
         , Value  (Sep fs) ~ Value fs)
       => Packable (Row (f1 :*: fs)) where

 pack   (Row c fs) xs
        =  pack Word8be (cw8 '[')
        <> pack (Sep c fs) xs
        <> pack Word8be (cw8 ']')
 {-# INLINE pack #-}


 unpack = error "repa-convert.row unpack finish me"
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
cw8 :: Char -> Word8
cw8 c = fromIntegral $ ord c
{-# INLINE cw8 #-}

