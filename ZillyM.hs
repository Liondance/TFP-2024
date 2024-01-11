--
-- Zilly
--
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TypeApplications #-}
module ZillyM where

import Control.ZM hiding (handle)
import Control.IE
import ADT.ADTM
import ADT.Map qualified as M
import ADT.Map (Map)
import Data.Functor
import Data.Foldable (traverse_)
import Control.Monad
import Data.Dynamic
import Control.Concurrent.MVar
import Control.Exception



--
-- Runtime
--

cvalue :: E Env -> ZME (E Env)
cvalue (Sym s) = getVar s
{-
whats the cvalue of a closure?
cvalue (ClosureF _ env e@(Sym s)) = local (const env) $ cvalue e
cvalue (ClosureF _ _ c@(ClosureF {})) = cvalue c
-}
cvalue a       = pure a


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
  (ClosureV env v b) -> do 
    s0 <- showE x
    x' <- rvalue x
    value <- liftIO $ newMVar (toDyn x') 
    let env' = insert v value env
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
rvalue (Defer ma) = flip ClosureF ma <$> ask
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
rvalue (Formula ma) = do 
  env <- ask
  ma' <- cvalue ma
  pure $ ClosureF env ma'
rvalue (ClosureF env e) = local (const env) (rvalue e)


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
    oldEnv <$ foldM (\e a -> local (const e) (run' a)) oldEnv
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
run' Halt = throwM HaltE

run :: Program Env -> IO ()
run = handle @SomeException print . void . foldM (\e a -> runZillyM (run' a) e) M.empty
