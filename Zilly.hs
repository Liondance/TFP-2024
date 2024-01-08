--
-- Zilly
--
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Zilly where

import Control.ZM
import Control.IE
import ADT.ADT
import ADT.Map qualified as M
import Data.Functor
import Data.Foldable (traverse_)
import Control.Monad
import Data.Dynamic
import Control.Concurrent.MVar
--
-- Runtime
--


rvalue :: E Env -> ZME (E Env) 
rvalue (Val v) = pure . Val $ v
rvalue (Minus ma mb) = do
  a <- rvalue ma 
  b <- rvalue mb 
  case (a,b) of
    (Val a',Val b') -> pure . Val $ a' - b'
    (Val _, x) -> do
      s <- showE x
      throwM . BT $ "Error on minus, expected a value as its second argument, but got: " <> s
    (x, Val _) -> do 
      s <- showE x
      throwM . BT $ "Error on minus, expected a value as its first argument, but got: " <> s
    (x,x') -> do
      s  <- showE x
      s' <- showE x'
      throwM . BT 
        $ "Error on minus, expected a value in both arguments, but got: " 
        <> s 
        <> ", as its first argument and"
        <> s' 
        <> " as its second"
rvalue c@(ClosureV {}) = pure c
rvalue (Sym s) = getVar s >>= rvalue
rvalue (Lambda t v b) = do
  env <- ask
  pure $ ClosureV env v b
rvalue (Apply f x) = rvalue f >>= \f -> case f of
  (ClosureV env v b) -> do  -- ClosureV env "x" (Sym "x") == \x -> x
    s0 <- showE x
    x' <- rvalue x
    s  <- showE x'
    value <- liftIO $ newMVar (toDyn x') -- Minus (Sym "x") (Minus (Val 0) (Sym "y")) 
    let env' = M.insert v value env -- insert "x" (Minus (Sym "x") (Minus (Val 0) (Sym "y")))
    local (const env') (rvalue b)
  e -> do 
    s <- showE e
    throwM . BT $ "Can only apply functions, but instead got: " <> s
rvalue (If mb ma mc) = do
  b <- rvalue mb 
  case b of
    Val b -> rvalue (if b > 0 then ma else mc)
    x     -> do 
      s <- showE x
      throwM . BT $ "Ifs first argument must be a value" <> s
rvalue (Defer ma) = pure ma
rvalue (Less ma mb) = do
  a <- rvalue ma 
  b <- rvalue mb
  case (a,b) of
    (Val a',Val b') -> pure . Val $ if a' < b' then 1 else -1
    (Val _, x) -> do
      s <- showE x
      throwM . BT $ "Error on minus, expected a value as its second argument, but got: " <> s
    (x, Val _) -> do 
      s <- showE x
      throwM . BT $ "Error on minus, expected a value as its first argument, but got: " <> s
    (x,x') -> do
      s  <- showE x
      s' <- showE x'
      throwM . BT 
        $ "Error on minus, expected a value in both arguments, but got: " 
        <> s 
        <> ", as its first argument and"
        <> s' 
        <> " as its second"
rvalue (Formula ma) = case ma of
  Sym varName -> do
    env <- ask
    v <- getVar varName
    pure $ ClosureF env v

  _           -> do 
    env <- ask
    pure $ ClosureF env ma 
rvalue (ClosureF env e) = local (const env) (rvalue e)

{-
<Z> x := 5;
<Z -> Z> f := <Z> y -> x + y;
x := 6;
<Z> z := f(9); -- returns 6 + 9
-}


run' :: Statement Env -> ZME Env
run' (Define t a b)= case a of
  Sym varName -> do
    b' <- rvalue b
    defineVar t varName b'
  _ -> do
    s <- showE a
    throwM . BT $ "Bad l-value: " <> s
run' (Assign a b)= case a of
  Sym varName -> do
    b' <- rvalue b
    assignVar varName b'
    ask
  _ -> do
    s <- showE a
    throwM . BT $ "Bad l-value: " <> s
run' (Show s e) = do
  e' <- showE =<< rvalue e
  liftIO . putStrLn $ s <> e'
  ask
run' (Branch cond as bs) = rvalue cond >>= \c -> case c of
  Val n -> do
    oldEnv <- ask
    oldEnv <$ foldM (\e a -> local (const e)(run' a)) oldEnv
      (if n >= 0 then as else bs)
  c     -> do 
    s <- showE c
    throwM . BT $ "Branch conditions must be integer valued. But instead got: " <> s
run' (While cond as) = rvalue cond >>= \c -> case c of
  Val n -> do
    oldEnv <- ask
    if n >= 0 
      then do 
        foldM_ (\e a -> local (const e) (run' a)) oldEnv as
        local (const oldEnv) (run' $ While cond as)
      else ask 
  c     -> do 
    s <- showE c
    throwM . BT $ "While conditions must be integer valued. But instead got: " <> s


run :: Program Env -> IO ()
run = void . foldM (\e a -> runZillyM (run' a) e) M.empty

p1 :: IO ()
p1 = run 
  [ Define Z (Sym "x") $ Val 9
  , Define Z (Sym "y") $ Val 9
  , Define Z (Sym "z") $ Minus (Sym "x") (Sym "y")
  , Show "" (Sym "x")
  , Show "" (Sym "y")
  , Show "" (Sym "z")
  ]

-- fibonacci
p2 :: IO ()
p2 = run 
  [ Define Z (Sym "plus") 
    $ Lambda Z "x" -- \x ->
    $ Lambda Z "y" -- \y ->
    $ Minus (Sym "x") -- x -
    $ Minus (Val 0) (Sym "y") -- 0 - y
  , Define Z (Sym "z") $ Val 20 
  , Define Z (Sym "y") $ Apply (Apply (Sym "plus") $ Val 7) (Val 5)
  , Show "" (Sym "y")
  ]


p3 :: IO ()
p3 = run 
  [ Define Z (Sym "x") $ Val 100
  , Define Z (Sym "plus") 
    $ Lambda Z "y" -- \y ->
    $ Minus (Sym "x") -- x -
    $ Minus (Val 0) (Sym "y") -- 0 - y
  , Define Z (Sym "z") $ Val 20 
  , Define Z (Sym "y") $ Apply (Sym "plus") $ Val 7
  , Show "" (Sym "y")
  ]

p4 :: IO ()
p4 = run 
  [ Define Z (Sym "x") $ Val 100
  , Define Z (Sym "plus") 
    $ Lambda Z "y" -- \y ->
    $ Minus (Sym "x") -- x -
    $ Minus (Val 0) (Sym "y") -- 0 - y
  , Define Z (Sym "z") $ Val 20 
  , Assign (Sym "x") $ Val 10
  , Define Z (Sym "y") $ Apply (Sym "plus") $ Val 7
  , Show "" (Sym "y")
  ]

p5 :: IO ()
p5 = run 
  [ Define Z (Sym "plus") 
    $ Lambda Z "x" -- \x ->
    $ Lambda Z "y" -- \y ->
    $ Minus (Sym "x") -- x -
    $ Minus (Val 0) (Sym "y") -- 0 - y
  , Define Z (Sym "f") 
    $ Lambda Z "x"
    $ If (Sym "x" `Less` Val 1) (Val 1) 
      (Apply 
        (Apply (Sym "plus") (Sym "x"))
        (Apply (Sym "f") (Sym "x" `Minus` Val 1))
      )
  , Define Z (Sym "z1") (Apply (Sym "f") (Val 1))
  , Show "" (Sym "z1")
  , Define Z (Sym "z10") (Apply (Sym "f") (Val 10))
  , Show "" (Sym "z10")
  ]

p6 :: IO ()
p6 = run 
  [ Define Z (Sym "plus") 
    $ Lambda Z "x" -- \x ->
    $ Lambda Z "y" -- \y ->
    $ Minus (Sym "x") -- x -
    $ Minus (Val 0) (Sym "y") -- 0 - y
  , Define Z (Sym "f") 
    $ Lambda Z "x"
    $ If (Sym "x" `Less` Val 2) (Sym "x") 
      (Apply 
        (Apply (Sym "plus") (Apply (Sym "f") (Sym "x" `Minus` Val 1)))
        (Apply (Sym "f") (Sym "x" `Minus` Val 2))
      )
  , Define Z (Sym "z0") (Apply (Sym "f") (Val 0))
  , Show "" (Sym "z0")
  , Define Z (Sym "z1") (Apply (Sym "f") (Val 1))
  , Show "" (Sym "z1")
  , Define Z (Sym "z2") (Apply (Sym "f") (Val 2))
  , Show "" (Sym "z2")
  , Define Z (Sym "z3") (Apply (Sym "f") (Val 3))
  , Show "" (Sym "z3")
  , Define Z (Sym "z4") (Apply (Sym "f") (Val 4))
  , Show "" (Sym "z4")
  , Define Z (Sym "z5") (Apply (Sym "f") (Val 5))
  , Show "" (Sym "z5")
  , Define Z (Sym "z6") (Apply (Sym "f") (Val 6))
  , Show "" (Sym "z6")
  , Define Z (Sym "z7") (Apply (Sym "f") (Val 7))
  , Show "" (Sym "z7")
  , Define Z (Sym "z8") (Apply (Sym "f") (Val 8))
  , Show "" (Sym "z8")
  , Define Z (Sym "z9") (Apply (Sym "f") (Val 9))
  , Show "" (Sym "z9")
  , Define Z (Sym "z10") (Apply (Sym "f") (Val 10))
  , Show "" (Sym "z10")
  ]