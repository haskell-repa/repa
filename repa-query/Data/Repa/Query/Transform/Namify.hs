
module Data.Repa.Query.Transform.Namify
        ( Namifier (..)
        , mkNamifierStrings)
where
import Control.Monad
import Data.Repa.Query.Exp              as Q
import Data.Map                         (Map)
import qualified Data.Map               as Map


-- | Holds a function to rename anonymous binders, 
--   and the state of the renamer as we decend into the tree.
--
--   @s@ is the state type used to generate fresh names.
--
--   @b@ is the type of bindind occurrences of variables.
--
--   @u@ is the type of bound occurrences of variables.
--
data Namifier b u
        = Namifier
        { -- | Create a new name for this binder that is not in the given
          --   environment.
          namifierNew   :: Map b u -> (b, u)

          -- | Holds the in-scope named binders during namification
        , namifierEnv   :: Map b u

          -- | Stack of renamed binders along this path.
        , namifierStack :: [(b, u)] }


-------------------------------------------------------------------------------
class Namify (c :: * -> * -> *) where
 -- | Rewrite anonymous binders to named binders in a thing.
 namify :: Ord b
        => Namifier b u
        -> c () Int
        -> Maybe (c b u)


instance Namify (Exp a) where
 namify nam xx
  = case xx of
        XVal a v        
         -> liftM  (XVal a) (namify  nam v)

        XVar a u
         -> liftM  (XVar a) (rewrite nam u)

        XApp a x1 x2
         -> liftM2 (XApp a) (namify nam x1) (namify nam x2)

        XOp a sop xs
         -> liftM  (XOp a sop) (mapM (namify nam) xs)


instance Namify (Val a) where
 namify nam vv
  = case vv of
        VLit a l
         ->     return $ VLit a l

        VLam a _ xx
         -> do  let (nam', (b', _)) = push nam
                liftM (VLam a b') (namify nam' xx)


-------------------------------------------------------------------------------
-- | Assign a name to an anonymous binder and push it on the stack.
push    :: Ord b
        => Namifier b u
        -> (Namifier b u, (b, u))

push nam
 = let  (b, u) = namifierNew nam (namifierEnv nam)
   in   (  nam {  namifierEnv   = Map.insert b u (namifierEnv nam)
               ,  namifierStack = [(b, u)] }
        , (b, u))


-- | Rewrite a debruijn index to its named version,
--   or `Nothing` if it's not in scope.
rewrite :: Namifier b u
        -> Int
        -> Maybe u

rewrite nam ix
 = case lookup ix $ zip [0..] (namifierStack nam) of
        Just (_, u)     -> return u
        Nothing         -> mzero


-------------------------------------------------------------------------------
mkNamifierStrings :: Namifier String String
mkNamifierStrings 
 = Namifier
        { namifierNew   = \has -> new has (0 :: Int)
        , namifierEnv   = Map.empty
        , namifierStack = [] }
 where
        new !has !i
         = let candidate = "x" ++ show i
           in  case Map.lookup candidate has of
                Nothing     -> (candidate, candidate)
                Just _clash -> new has (i + 1)


