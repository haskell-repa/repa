


-- Three cases
-- 1) Bulk arrays. 1:1 mapping.
-- 2) Chains.      Streams without skip.
-- 3) Streams.     With skip.
-- Result of a zipwith is the most general of all arguments.
-- Compute that type with instances.
-- End up with complicated constraints, need to hide them behind uniform API.
--
-- Use rewrite rules to break the abstraction during compilation.
-- Breaking the abstraction locally the types.
-- Can rewrite to a different type provided the context can cope with it.
-- Type change can ripple up through enclosing functions.
--

-- Wrap operators in data constructors, to produce AST during inlining.
-- Use a plugin to rewrite the AST to actual code code.
-- Only have to come up with the dictionaries specific to this problem,
-- don't need to handle the general case.
--

{-
data AST
instance Source AST a e where
 data Array 

data Vec e
        = VecCompute  (Vector D a)
        | VecUnStream (Vector S a)
        | VecUnChain  (Vector N a)
        | forall a b.   VecZipWith (a -> b -> e) (Vec a) (Vec b)


-- Implement real evaluator as rewrite rules,
-- so we don't need to inline case expressions to handle
-- every possible AST node.
veval :: Vector AST e -> Vector U e
veval arr
 = case arr of
        VecCompute  dvec        -> compute dvec
        VecUnstream svec        -> unstream svec
        VecUnChain  cvec        -> unchain cvec
        ...
{-# NOINLINE veval #-}

{-# RULES ...
    veval (VecCompute dvec)     = compute dvec 
    ...
  #-}



acompute :: Vector D a -> Vector AST e
acompute vec    = VecCompute vec (vcompute_susp vec)

vcompute_susp = vcompute
{-# NOINLINE vcompute_susp #-}


-- To do it like this only need to produce the local Zip dictionaries
-- for the ZipWith instance. These are all very regular.
--
-- Don't need to handle the general case, where we would need the 
-- constraint solver to perform long-distance unification.
-}


-- ZipWiths -------------------------------------------------------------------
uzipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
          => (a -> b -> c -> d -> e)
          -> Vector U a -> Vector U b -> Vector U c -> Vector U d
          -> Vector U e
uzipWith4 f vec1 vec2 vec3 vec4
        = vcompute
        $ vzipWith4 f (veat vec1) (veat vec2) (veat vec3) (veat vec4)
{-# INLINE [6] uzipWith4 #-}

{-
{-# RULES "vzipwith4/eat1"
    forall 
        (f    :: a -> b -> c -> d -> e)
        (vec1 :: Vector r1 a) 
        (vec2 :: Vector r2 b)
        (vec3 :: Vector r3 c)
        (vec4 :: Vector r4 d)
    .  vcompute (vzipWith4 f (veat vec1) vec2 vec3 vec4)
    =  vcompute (vzipWith4 f vec1        vec2 vec3 vec4)
   #-}

{-# RULES "veat/vcompute"
    forall vec. veat (vcompute vec) = vec
 #-}
-}
-- Want to allow a rule to fire that changes the type of the expression, 
-- provided the expression is the argument of a type class method,
-- and we can choose a new instance for that function for the result expression.


-- Eat/Compute ----------------------------------------------------------------
veat    = delay
{-# INLINE [4] veat #-}


vcompute = suspendedComputeP
{-# INLINE [4] vcompute #-}

{-# RULES "eat/compute"
    forall x. veat (vcompute x) = x #-}
