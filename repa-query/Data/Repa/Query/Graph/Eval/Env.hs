
-- | Generic environment type used when directly evaluating queries.
module Data.Repa.Query.Graph.Eval.Env
        (Env (..))
where
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- Generic environment type.
class Env env a where
 type Bind  env  
 type Bound env

 -- | Look up a value.
 lookup         :: Bound env -> env a -> Maybe a

 -- | Update a value.
 update         :: Bound env -> a -> env a -> env a

 -- | Insert a new value.
 insert         :: Bind  env -> a -> env a -> env a

 -- | Get the binder corresponding to this bound occurrence from the 
 bindOfBound    :: Bound env -> env a -> Maybe (Bind env)

 -- | Yield an environment from a list of binders and values.
 fromList       :: [(Bind env, a)] -> env a

 -- | Yield a list of binders and values from an environment.
 toList         :: env a -> [(Bind env, a)]


instance Ord n => Env (Map n) a where
 type Bind  (Map n) = n
 type Bound (Map n) = n
 lookup          = Map.lookup
 update          = Map.insert
 insert          = Map.insert
 bindOfBound u _ = Just u
 fromList        = Map.fromList
 toList          = Map.toList
