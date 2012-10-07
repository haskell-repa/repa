
module Data.Array.Repa.Chain.Base
        ( -- * Linear chains
          Chain(..)
        , Step (..)
        , chain
        , fold
        , foldM

          -- * Distributed chains
        , DistChain(..)
        , Distro (..)
        , chainD
        , foldD, foldMD)
where
import Data.Array.Repa.Distro
import GHC.Exts


-- Linear ---------------------------------------------------------------------
-- | Pure linear chains.
--
--   Unlike streams, chains have a known length at creation time.
--   This means the consumer always knows how many elements to demand, 
--   and we don't need a `Done` constructor in the `Step` type.
-- 
data Chain a
        = forall s
        . Chain 
        { chainLength   :: Int# 
        , chainStart    :: s 
        , chainNext     :: Int# -> s -> Step s a
        }


-- | A chain command.
data Step s a
        = Yield  s a            -- ^ Yield a new stream state and an element.
        | Update s              -- ^ Just update the stream state.
        deriving Show


-- | Construct a chain from a function that produces each value.
chain   :: Int#                 -- ^ Overall size of the chain.
        -> (Int# -> a)          -- ^ Get the element at this position.
        -> Chain a

chain size get
 = Chain size () mkStep

 where  mkStep ix _
         = Yield () (get ix)
        {-# INLINE [0] mkStep #-}
{-# INLINE [1] chain #-}


-- | Consume a chain.
fold :: (a -> b -> b) -> b -> Chain a -> b
fold f y0 c
        = fold' 0# f y0 c
{-# INLINE [1] fold #-}



-- | Consume a chain in a monad.
foldM :: Monad m => (a -> b -> m b) -> b -> Chain a -> m b
foldM f y0 c
        = foldM' 0# f y0 c
{-# INLINE [1] foldM #-}


-- Distributed Chains ---------------------------------------------------------
-- | Pure distributed chains.
data DistChain a
        = DistChain
        { distChainDistro       :: !Distro
        , distChainFrag         :: Int# -> Chain a }


-- | Construct a distributed chain from a function that gets 
--   an arbitrary element.
chainD  :: Distro               -- ^ How the result should be distributed.
        -> (Int# -> a)          -- ^ Get the element at this position.
        -> DistChain a

chainD distro get
 = DistChain distro getFrag
 where  getFrag i
         = chain (distroFragLength distro i) get
        {-# INLINE [0] getFrag #-}
{-# INLINE [1] chainD #-}


-- | Consume a distributed chain.
foldD :: (a -> b -> b) -> b -> DistChain a -> b
foldD f y0 (DistChain distro getFrag)
 = eatFrags y0 0#
 where eatFrags y i
        | i >=# distroFrags distro   = y
        | otherwise
        = eatFrags (fold' (distroFragStart distro i) f y (getFrag i))
                   (i +# 1#)
       {-# INLINE [0] eatFrags #-}
{-# INLINE [1] foldD #-}


-- Consume a single chain fragment, given the starting index.
--
-- NOTE: We don't export this function because not all chains can be
--       started from an arbitrary index. However, this is possible
--       by construction for the fragments of distributed chains.
--
fold' :: Int# -> (a -> b -> b) -> b -> Chain a -> b
fold' ix0 f y0 (Chain size s0 mkStep)
 = eat ix0 s0 y0
 where eat ix s y
        | ix >=# size   = y
        | otherwise
        = case mkStep ix s of
                Yield  s' x     -> eat (ix +# 1#) s' (f x y)
                Update s'       -> eat ix s' y
       {-# INLINE [0] eat #-}
{-# INLINE [1] fold' #-}



-- | Consume a distributed chain in a monad.
foldMD :: Monad m => (a -> b -> m b) -> b -> DistChain a -> m b
foldMD f y0 (DistChain distro getFrag)
 = eatFrags y0 0#
 where eatFrags y i
        | i >=# distroFrags distro   = return y
        | otherwise
        = do    y'      <- foldM' (distroFragStart distro i) f y (getFrag i)
                eatFrags y' (i +# 1#)
       {-# INLINE [0] eatFrags #-}
{-# INLINE [1] foldMD #-}


-- Consume a single chain fragment in a monad, given the starting index.
--
-- NOTE: We don't export this function because not all chains can be
--       started from an arbitrary index. However, this is possible
--       by construction for the fragments of distributed chains.
--
foldM' :: Monad m => Int# -> (a -> b -> m b) -> b -> Chain a -> m b
foldM' ix0 f y0 (Chain size s0 mkStep)
 = eat ix0 s0 y0
 where eat ix s y
        | ix >=# size   = return y
        | otherwise
        = case mkStep ix s of
                Yield  s' x     
                 -> do  y'      <- f x y
                        eat (ix +# 1#) s' y'
                Update s'
                 -> eat ix s' y
       {-# INLINE [0] eat #-}
{-# INLINE [1] foldM' #-}

