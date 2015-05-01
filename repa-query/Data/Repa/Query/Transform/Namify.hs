
module Data.Repa.Query.Transform.Namify
        ( Namifier (..)
        , Namify   (..)
        , mkNamifierStrings)
where
import Control.Monad
import Data.Repa.Query.Graph            as Q
import Data.Repa.Query.Job.Spec         as Q
import Data.Map                         (Map)
import qualified Data.Map               as Map


-- | Holds a function to rename anonymous binders, 
--   and the state of the renamer as we decend into the tree.
--
--   @b@ and @u@ are the types of bindinging and bound occurrences of
--   variables respectively.
--
data Namifier b u
        = Namifier
        { -- | Create a new name for this binder that is not in the given
          --   environment.
          namifierNew   :: Map b u -> (b, u)

          -- | Holds the in-scope named binders during namification.
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


instance Namify (Query a nF) where
 namify nam (Query fOut graph)
        = liftM (Query fOut) (namify nam graph)


instance Namify (Graph a nF) where
 namify nam (Graph ns) 
        = liftM Graph (mapM (namify nam) ns)


instance Namify (Node a nF) where
 namify nam nn
  = case nn of
        NodeSource s    
         -> return $ NodeSource s

        NodeOp fop
         -> liftM NodeOp (namify nam fop)


instance Namify (FlowOp a nF) where
 namify nam fop
  = case fop of
        FopMapI    fins fout xx
         -> liftM  (FopMapI fins fout)   (namify nam xx)

        FopFilterI fin fout xx
         -> liftM  (FopFilterI fin fout) (namify nam xx)

        FopFoldI   fin fout xf xn
         -> liftM2 (FopFoldI fin fout)   (namify nam xf) (namify nam xn)

        FopFoldsI  flen felem fout xf xn
         -> liftM2 (FopFoldsI flen felem fout) (namify nam xf) (namify nam xn)

        FopGroupsI flen felem xx
         -> liftM  (FopGroupsI flen felem) (namify nam xx)


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
               ,  namifierStack = (b, u) : namifierStack nam }
        , (b, u))


-- | Rewrite a debruijn index to its named version,
--   or `Nothing` if it's not in scope.
rewrite :: Namifier b u
        -> Int
        -> Maybe u

rewrite nam ix
 = case lookup ix $ zip [0..] (namifierStack nam) of
        Just (_, u)     -> return u
        Nothing         -> Nothing


-------------------------------------------------------------------------------
-- | Namifier that uses strings for binding and bound occurrences
--   of variables.
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

