
module Data.Array.Repa.Chain.Base
        ( -- * Linear chains
          Chain(..)
        , Step (..)
        , chain
        , fold,  fold'
        , foldM, foldM'

          -- * Fragmented chains
        , ChainF(..)
        , chainF
        , foldF, foldMF)
where
import GHC.Exts


-- Linear ---------------------------------------------------------------------
-- | Pure chains.
--
--   Unlike streams, chains have a known length at creation time.
--   This means the consumer always knows how many elements to demand, 
--   and we don't need a `Done` constructor in the `Step` type.
-- 
data Chain a
        = forall s
        . Chain 
        { chainLength     :: Int# 
        , chainStateStart :: s 
        , chainMkStep     :: Int# -> s -> Step s a
        }


-- | A chain command.
data Step s a
        = Yield  s a            -- ^ Yield a new stream state and an element.
        | Update s              -- ^ Just update the stream state.
        deriving Show


-- | Construct a chain
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


-- | Consume a single chain fragment, given the starting index.
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
{-# INLINE [0] fold' #-}


-- | Consume a chain in a monad.
foldM :: Monad m => (a -> b -> m b) -> b -> Chain a -> m b
foldM f y0 c
        = foldM' 0# f y0 c
{-# INLINE [1] foldM #-}


-- | Consume a single chain fragment in a monad, given the starting index.
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
{-# INLINE [0] foldM' #-}


-- Fragmented -----------------------------------------------------------------
-- | Pure fragmented chains.
--
data ChainF a
        = ChainF
        { chainLengthF  :: Int# 
        , chainFrags    :: Int#
        , chainGetStart :: Int# -> Int#
        , chainGetFrag  :: Int# -> Chain a }


-- | Construct a fragmented chain.
chainF  :: Int#                 -- ^ Overall size of the chain.
        -> Int#                 -- ^ Number of fragments.
        -> (Int# -> Int#)       -- ^ Get the start position of a fragment.
        -> (Int# -> a)          -- ^ Get the element at this position.
        -> ChainF a

chainF size frags start get
 = ChainF size frags start getFrag
 where  getFrag frag
         = chain (start (frag +# 1#) -# start frag)
                 get
        {-# INLINE [0] getFrag #-}
{-# INLINE [1] chainF #-}


-- | Consume a fragmented chain.
foldF :: (a -> b -> b) -> b -> ChainF a -> b
foldF f y0 (ChainF _ frags start getFrag)
 = eatFrags y0 0#
 where eatFrags y frag
        | frag >=# frags        = y
        | otherwise
        = eatFrags (fold' (start frag) f y (getFrag frag))
                   (frag +# 1#)
       {-# INLINE [0] eatFrags #-}
{-# INLINE [1] foldF #-}


-- | Consume a fragmented chain in a monad.
foldMF :: Monad m => (a -> b -> m b) -> b -> ChainF a -> m b
foldMF f y0 (ChainF _ frags start getFrag)
 = eatFrags y0 0#
 where eatFrags y frag
        | frag >=# frags        = return y
        | otherwise
        = do    y'      <- foldM' (start frag) f y (getFrag frag)
                eatFrags y'
                        (frag +# 1#)
       {-# INLINE [0] eatFrags #-}
{-# INLINE [1] foldMF #-}
