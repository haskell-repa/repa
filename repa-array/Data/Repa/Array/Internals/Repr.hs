

-- | Class of array representations.
class Repr r where
 -- | Proxy for an array representation. The representations are singletons, 
 --   so there is only one value of a given type. 
 --   Use with an explicit type signature, like @(repr :: B)@.
 repr    :: r


