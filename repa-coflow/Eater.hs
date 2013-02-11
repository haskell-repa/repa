
{-# LANGUAGE BangPatterns #-}
module Eater where
import Prelude hiding (map, unzip, filter, fold, sum)
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Data.Vector.Unboxed                      (Unbox, Vector)
import Data.IORef

-- Flow:  must produce an element on demand.
-- Eater: must consume an element on demand.

-- Branch *in*  is easy for flow,  but hard for eater.
-- Branch *out* is easy for eater, but hard for flow.

-- eater zip
ezip   :: ((a, b) -> c) -> Eater c -> (Eater a, Eater b)

-- flow unzip
funzip :: (a -> (b, c)) -> Flow a  -> (Flow b,  Flow c)

-- HOWEVER: 
--  In vector programming each parallel operation consumes
--  manifest vectors of a well known size. Input rates are well
--  defined, and we don't combine flows of unrelated rates.

-- TODO: Use this to implement the unflow primitive.
--       Define parallel eaters that take parallel flows.
--

-- | An eater eats a value and says whether it's still hungry.
data Eater a        
        = Eater (a -> IO Bool)

-- TODO: add start method, to tell it how many elements to expect.
--       add eat8 method, to match get8 from flows.

{-
feedFlow :: Flow r a -> Eater a -> IO ()
feedFlow (Flow  fStart fSize fReport fGet1 fGet8) 
         (Eater eEat1)
 = do   
        state   <- fStart
        let spin
             =  fGet1 state >>= \r
             -> case r of
                  Yield1 x _ 
                   -> do eEat1 x
                         return ()

                  Done 
                   -> return ()
        spin 
-}


-- Maps -----------------------------------------------------------------------
-- | Lift a unary function to an eater.
--   Given a function from 'a' to 'b' and an eater of 'b', 
--   we can produce an eater of 'b' by precomposition.
map  :: (a -> b) -> Eater b -> Eater a
map f (Eater eb)
 = Eater $ \xa -> eb (f xa)
{-# INLINE map #-}


-- | Lift a binary function to an eater.
map2 :: (a -> b -> c) -> Eater c -> Eater (a, b)
map2 f (Eater ec)
 = Eater $ \(xa, xb) -> ec (f xa xb)
{-# INLINE map2 #-}


-- Dup ------------------------------------------------------------------------
-- | Given two eaters of 'a', 
--   construct a new eater that passes its value to both.
dup  :: Eater a -> Eater a -> Eater a
dup (Eater ea1) (Eater ea2)
 = Eater $ \x -> ea1 x >> ea2 x
{-# INLINE dup #-}


-- | Build an eater of tuples from an eater of its components.
unzip  :: Eater a -> Eater b -> Eater (a, b)
unzip ea eb
 = dup (map fst ea) (map snd eb)
{-# INLINE unzip #-}


-- Filter ---------------------------------------------------------------------
-- | Filter eater.
filter :: (a -> Bool) -> Eater a  -> Eater a
filter p (Eater ea)
 = Eater $ \xa -> if p xa then ea xa
                          else return True
{-# INLINE filter #-}


-- Fold -----------------------------------------------------------------------
fold    :: (Unbox a, Num a)
        => (a -> a -> a) -> a  
        -> (Eater a -> IO b) -> IO (a, b)

fold f z load
 = do   refAcc  <- UM.unsafeNew 0
        UM.unsafeWrite refAcc 0 z

        let eater
             = Eater $ \x 
             -> do !acc <- UM.unsafeRead refAcc 0
                   UM.unsafeWrite refAcc 0 (acc `f` x)
                   return True

        y       <- load eater
        x       <- UM.unsafeRead refAcc 0
        return  (x, y)
{-# INLINE fold #-}


sum  :: (Unbox a, Num a) => (Eater a -> IO b) -> IO (a, b)
sum  = fold (+) 0
{-# INLINE sum #-}


prod :: (Unbox a, Num a) => (Eater a -> IO b) -> IO (a, b)
prod = fold (*) 1
{-# INLINE prod #-}


-- Eater4 ---------------------------------------------------------------------
-- | An eater eats a value and says whether it's still hungry.
data Eater4 a        
        = Eater4 (a -> IO Bool)
                 (a -> a -> a -> a -> IO Bool)


promote :: Eater a -> Eater4 a
promote (Eater eat1)
 = Eater4 eat1
 $ \x1 x2 x3 x4
 -> do  eat1 x1
        eat1 x2
        eat1 x3
        eat1 x4


map4 :: (a -> b) -> Eater4 b -> Eater4 a
map4 f (Eater4 eat1 eat4)
 = Eater4 eat1' eat4'
 where  eat1'   = \x   
                -> eat1 (f x)

        eat4'   = \x1 x2 x3 x4
                -> eat4 (f x1) (f x2) (f x3) (f x4)
{-# INLINE map4 #-}



-- Combine --------------------------------------------------------------------
{-
combine :: Eater a -> IO (Eater Bool, Eater a, Eater a)
combine (Eater eatResult) 
 = (eatFlags, eatLeft, eatRight)
 where  
-}
{-
esums  :: Num a => E a -> (E Int, E a)
esums (E ea)
 = do   refAcc  <- newIORef 0
        refLen  <- newIORef 0

        eatSegLen
         = ...

        eatElem
         = ...
-}


-- Feeders --------------------------------------------------------------------
feed1 :: [a] -> Eater a -> IO ()
feed1 xx (Eater eat)
 = go xx
 where  go []   = return ()
        go (x : xs)
         = do   eat x
                go xs
{-# INLINE feed1 #-}


ufeed1 :: Unbox a => Vector a -> Eater a -> IO ()
ufeed1 vec (Eater eat)
 = go 0
 where  len     = U.length vec
        go ix
         | ix >= len    = return ()
         | otherwise    
         = do   eat (U.unsafeIndex vec ix)
                go (ix + 1)



feed4 :: [a] -> Eater4 a -> IO ()
feed4 xx (Eater4 eat1 eat4)
 = go xx
 where  go []   
         = return ()

        go (x1 : x2 : x3 : x4 : xs)
         = do   eat4 x1 x2 x3 x4
                go xs

        go (x1 : xs)
         = do   eat1 x1
                go xs
{-# INLINE feed4 #-}


{-} 
feed2 :: [a] -> [b] -> E a -> E b -> IO ()
feed2 xs []
-}


-- Consumers ------------------------------------------------------------------
eatPrint :: Show a => Eater a
eatPrint 
 = Eater $ \xa -> print xa >> return True

