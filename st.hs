{-

-- State Monad (is it?)

-- given a state, a transformer returns a pair with the new state
newtype ST a = S (State -> (a, State))

-- function that applies a state transformer to a state
st'apply :: ST a -> State -> (a, State)
st'apply (S st) x = st x

-- ST is a Functor
instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x, s') = st'apply st s in (g x, s'))

-- ST is an Applicative Functor
instance Applicative ST where
    -- pure = a -> ST a
    pure x = S (\s -> (x, s))
    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f, s' ) = st'apply stf s
            (x, s'') = st'apply stx s' in
                (f x, s''))

-- ST is a Monad
instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = st'apply st s in st'apply (f x) s')

-}

