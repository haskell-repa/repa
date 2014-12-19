
module Data.Array.Repa.Series.Ref
        ( Ref (..)
        , new
        , read
        , write)
where
import Data.Vector.Primitive.Mutable              (IOVector, Prim)
import qualified Data.Vector.Primitive.Mutable    as PM
import Prelude hiding (read)


-- | Mutable references.
data Ref a
        = Ref !(IOVector a)


-- | Create a new unboxed reference.
new :: Prim a => a -> IO (Ref a)
new x
 = do   vec     <- PM.unsafeNew 1
        PM.unsafeWrite vec 0 x
        return  (Ref vec)
{-# INLINE [1] new #-}


-- | Read from an unboxed reference.
read :: Prim a => Ref a -> IO a
read (Ref vec)
 =      PM.unsafeRead vec 0
{-# INLINE [1] read #-}


-- | Write to an unboxed reference.
write :: Prim a => Ref a -> a -> IO ()
write (Ref vec) x
 = do   PM.unsafeWrite vec 0 x
{-# INLINE [1] write #-}

