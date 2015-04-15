
module Data.Repa.Query.Eval.Env
        (Env (..))
where
import Data.Map                 (Map)
import qualified Data.Map       as Map

class Env env a where
 type Bind  env  
 type Bound env
 lookup         :: Bound env -> env a -> Maybe a
 update         :: Bound env -> a -> env a -> env a
 insert         :: Bind  env -> a -> env a -> env a
 bindOfBound    :: Bound env -> env a -> Maybe (Bind env)

 fromList       :: [(Bind env, a)] -> env a
 toList         :: env a -> [(Bind env, a)]


instance Env (Map String) a where
 type Bind  (Map String) = String
 type Bound (Map String) = String
 lookup          = Map.lookup
 update          = Map.insert
 insert          = Map.insert
 bindOfBound u _ = Just u
 fromList        = Map.fromList
 toList          = Map.toList
