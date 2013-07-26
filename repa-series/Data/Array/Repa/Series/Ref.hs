
module Data.Array.Repa.Series.Ref
        ( Ref (..)
        , new
        , read
        , write)
where
import Data.Vector.Unboxed.Mutable              (IOVector, Unbox)
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude hiding (read)


-- | Mutable references.
data Ref a
        = Ref !(IOVector a)


-- | Create a new unboxed reference.
new :: Unbox a => a -> IO (Ref a)
new x
 = do   vec     <- UM.new 1
        UM.unsafeWrite vec 0 x
        return  (Ref vec)
{-# INLINE new #-}


-- | Read from an unboxed reference.
read :: Unbox a => Ref a -> IO a
read (Ref vec)
 =      UM.unsafeRead vec 0
{-# INLINE read #-}


-- | Write to an unboxed reference.
write :: Unbox a => Ref a -> a -> IO ()
write (Ref vec) x
 = do   UM.unsafeWrite vec 0 x
{-# INLINE write #-}

