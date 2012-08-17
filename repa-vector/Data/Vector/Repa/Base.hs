
module Data.Vector.Repa.Base
        ( Vector
        , Map(..)
        , Zip(..)
        , vzip3
        , vzip4
        , vlength
        , vreplicate)
where
import Data.Array.Repa                  as R
import Prelude                          hiding (length)
import qualified Data.Vector.Unboxed    as U


type Vector r e 
        = Array r DIM1 e


-- Map ------------------------------------------------------------------------
class Map r a where
 type TM r
 vmap :: (a -> b) -> Vector r a -> Vector (TM r) b


instance U.Unbox e => Map U e where
 type TM U      = D

 vmap f arr
  = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)
 {-# INLINE [4] vmap #-}


instance Map D e where
 type TM D      = D
 vmap           = R.map
 {-# INLINE [4] vmap #-}


-- Zip2 -----------------------------------------------------------------------
class Zip r1 r2 a b where
 type TZ r1 r2
 vzip   :: Vector r1 a -> Vector r2 b
        -> Vector (TZ r1 r2) (a, b)

instance (U.Unbox a, U.Unbox b) 
       => Zip U U a b where
 type TZ U U            = D
 vzip !arr1 !arr2       = vzip (delay arr1) (delay arr2)
 {-# INLINE [4] vzip #-}


instance U.Unbox a
      => Zip U D a b where
 type TZ U D            = D
 vzip !arr1 !arr2       = vzip (delay arr1) arr2
 {-# INLINE [4] vzip #-}


instance U.Unbox b
      => Zip D U a b where
 type TZ D U    = D
 vzip !arr1 !arr2       = vzip arr1 (delay arr2)
 {-# INLINE [4] vzip #-}


instance Zip D D a b where
 type TZ D D    = D
 vzip !arr1 !arr2
  = fromFunction (extent arr1) get
  where get ix = ( arr1 `unsafeIndex` ix
                 , arr2 `unsafeIndex` ix)
        {-# INLINE get #-}
 {-# INLINE [4] vzip #-}




-- Zip3 -----------------------------------------------------------------------
vzip3   :: ( Zip r1 (TZ r2 r3) a (b, c)
           , Zip r2 r3         b c
           , Map (TZ r1 (TZ r2 r3)) (a, (b, c)))
        => Vector r1 a -> Vector r2 b -> Vector r3 c
        -> Vector (TM (TZ r1 (TZ r2 r3))) (a, b, c)

vzip3 !vec1 !vec2 !vec3
 = vmap merge
 $ vzip vec1 (vzip vec2 vec3)
 where  merge (x, (y, z)) = (x, y, z)
        {-# INLINE merge #-}
{-# INLINE [4] vzip3 #-}


-- Zip4 -----------------------------------------------------------------------
vzip4   :: ( Zip r1 (TZ r2 (TZ r3 r4)) a (b, (c, d))
           , Zip r2 (TZ r3 r4)         b (c, d)
           , Zip r3 r4                 c d
           , Map (TZ r1 (TZ r2 (TZ r3 r4))) 
                 (a, (b, (c, d))))
        => Vector r1 a -> Vector r2 b -> Vector r3 c -> Vector r4 d
        -> Vector (TM (TZ r1 (TZ r2 (TZ r3 r4))))
                  (a, b, c, d)

vzip4 !vec1 !vec2 !vec3 !vec4
 = vmap merge
 $ vzip vec1 (vzip vec2 (vzip vec3 vec4))
 where  merge (x1, (x2, (x3, x4))) = (x1, x2, x3, x4)
        {-# INLINE merge #-}
{-# INLINE [4] vzip4 #-}


-------------------------------------------------------------------------------
-- | Get the length of a vector.
vlength :: Source r e => Vector r e -> Int
vlength !v
 = case extent v of
        Z :. len        -> len
{-# INLINE [4] vlength #-}


vreplicate :: Int -> e -> Vector D e
vreplicate !n !x
 = ADelayed (Z :. n) get
 where  get _ = x
        {-# INLINE get #-}
{-# INLINE [4] vreplicate #-}

