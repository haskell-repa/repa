
-- | Expressions that can be used to produce scalar values in Repa queries.
module Data.Repa.Query.Graph.Exp
        ( -- * Expressions
          Exp           (..)
        , annotOfExp

          -- * Values
        , Val           (..)
        , xLit,         xLam
        , takeVLit,     takeVLam

          -- * Literals
        , Lit           (..)
        , xBool
        , xWord,        xInt
        , xFloat,       xDouble
        , xString

          -- * Scalar operators.
        , ScalarOp      (..))
where


-------------------------------------------------------------------------------
-- | Scalar expressions.
data Exp a bV uV
        -- | Value.
        = XVal  !a !(Val a bV uV)

        -- | Variable.
        | XVar  !a !uV

        -- | Function application.
        | XApp  !a !(Exp a bV uV) !(Exp a bV uV)

        -- | Apply a primitive operator.
        --   The application must be fully saturated.
        | XOp   !a !ScalarOp ![Exp a bV uV]
        deriving (Eq, Show)


-- | Take the annotation from an expression.
annotOfExp :: Exp a bV uV -> a
annotOfExp xx
 = case xx of
        XVal a _        -> a
        XVar a _        -> a
        XApp a _ _      -> a
        XOp  a _ _      -> a


-------------------------------------------------------------------------------
-- | A value.
data Val a bV uV
        = VLit  !a !Lit
        | VLam  !a !bV !(Exp a bV uV)
        deriving (Eq, Show)


-- | Wrap a literal into an expression.
xLit   :: a -> Lit -> Exp a bV uV
xLit a lit = XVal a (VLit a lit)


-- | Wrap a lambda into an expression.
xLam   :: a -> bV -> Exp a bV uV -> Exp a bV uV
xLam a b x = XVal a (VLam a b x)


-- | Take a `Lit` from a `Val`, if it is one.
takeVLit :: Val a bV uV -> Maybe (a, Lit)
takeVLit vv
 = case vv of
        VLit a lit      -> Just (a, lit)
        _               -> Nothing


-- | Take `Lam` from `Val`, if it is one.
takeVLam :: Val a bV uV -> Maybe (a, bV, Exp a bV uV)
takeVLam vv
 = case vv of
        VLam a b x      -> Just (a, b, x)
        _               -> Nothing


-------------------------------------------------------------------------------
-- | Literal values.
data Lit
        = LBool       !Bool            -- ^ Literal Boolean
        | LWord       !Integer         -- ^ Literal word.
        | LInt        !Integer         -- ^ Literal integer.
        | LFloat      !Float           -- ^ Literal float.
        | LDouble     !Double          -- ^ Literal double.
        | LString     !String          -- ^ Literal string.
        deriving (Eq, Show)


-- | Wrap a boolean into an expression.
xBool   :: a -> Bool    -> Exp a bV uV
xBool a b   = XVal a $ VLit a $ LBool b


-- | Wrap a word into an expression.
xWord  :: a -> Integer  -> Exp a bV uV
xWord a i   = XVal a $ VLit a $ LWord (fromIntegral i)


-- | Wrap an integer into an expression.
xInt    :: a -> Integer -> Exp a bV uV
xInt a i    = XVal a $ VLit a $ LInt (fromIntegral i)


-- | Wrap a float into an expression.
xFloat  :: a -> Float  -> Exp a bV uV
xFloat a f  = XVal a $ VLit a $ LFloat f


-- | Wrap a float into an expression.
xDouble  :: a -> Double  -> Exp a bV uV
xDouble a f = XVal a $ VLit a $ LDouble f


-- | Wrap a string into an expression.
xString :: a -> String  -> Exp a bV uV
xString a s = XVal a $ VLit a $ LString s


-------------------------------------------------------------------------------
-- | Scalar operators.
data ScalarOp
        -- Arithmetic
        = SopNeg                -- ^ Negation.
        | SopAbs                -- ^ Absolute value.
        | SopSignum             -- ^ Sign of number.
        | SopAdd                -- ^ Addition.
        | SopSub                -- ^ Subtraction.
        | SopMul                -- ^ Multiplication.
        | SopDiv                -- ^ Division.
        | SopEq                 -- ^ Equality.
        | SopNeq                -- ^ Negated equality.
        | SopGt                 -- ^ Greater-than.
        | SopGe                 -- ^ Greater-than or equal.
        | SopLt                 -- ^ Less-than.
        | SopLe                 -- ^ Less-than or equal.

        -- Tupling
        | SopRow  Int           -- ^ Construction        
        | SopGet Int Int        -- ^ Projection.

        -- Dates
        | SopStringOfDate       -- ^ Convert a date to a string.
        | SopYearOfDate         -- ^ Take the year number of a date.
        | SopMonthOfDate        -- ^ Take the month number of a date.
        | SopDayOfDate          -- ^ Take the day number of a date.
        deriving (Eq, Show)


