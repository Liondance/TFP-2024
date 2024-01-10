{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Control.IE where


import ADT.Map
import Control.ZM
import ADT.ADT

import Prelude hiding (lookup)

-- Stack
import ADT.Stack as Stack hiding (empty)
-- Map
import ADT.Map (Map, insert, lookup, update)
import qualified ADT.Map as Map
-- 
import Data.Dynamic
import Control.Concurrent.MVar
import Control.Exception
import Data.Functor
---------------------
-- Type Definitions
---------------------

type Env = Map String (MVar Dynamic)
type ZME = ZM Env


---------------------
-- Utilities
---------------------

defineVar :: T -> String -> E Env -> ZME Env
defineVar _ varName varBody = 
  asks (lookup varName) >>= \a -> case a of
  Just untypedVar -> throwM $ VAD varName
  Nothing         -> do
    case varBody of
      ClosureV e a b -> do
        value <- liftIO newEmptyMVar
        let c = ClosureV (insert varName value e) a b
        liftIO . putMVar value . toDyn $ c
        v <- liftIO $ newMVar (toDyn c)
        asks (insert varName v)
      _ -> do 
        value <- liftIO $ newMVar (toDyn varBody)
        asks (insert varName value)

assignVar :: String -> E Env -> ZME ()
assignVar varName varBody = asks (lookup varName) >>= \a -> case a of
  Just untypedVar -> case varBody of 
    ClosureV e a b ->
      let c = ClosureV (insert varName untypedVar e) a b
      in void . liftIO $ swapMVar untypedVar (toDyn c)
    _ -> void . liftIO $ swapMVar untypedVar (toDyn varBody)
  Nothing         -> throwM $ VND varName


getVar :: String -> ZME (E Env)
getVar varName = asks (lookup varName) >>= \a -> case a of
  Just untypedVar -> do 
    dynValue <- liftIO $ readMVar untypedVar
    case fromDynamic dynValue of
      Just value  -> pure value
      Nothing     -> throwM . BT . concat $ ["Variable: ", show varName, ", Has an incompatible type."]
  Nothing         -> throwM $ VND varName

showE :: E Env -> ZME String
showE (Val n) = pure . concat $ ["Val ", show n]
showE (Sym s) = pure . concat $ ["Sym ", show s]
showE (Lambda t s e) = (\e' -> concat ["Lambda (", show t, ") ", show s, " (",  e', ")"]) <$> showE e
showE (Apply f x)    = (\a b -> concat ["Apply (", a, ") (", b, ")"]) <$> showE f  <*> showE x
showE (If a b c)     = (\a b c -> concat ["If (", a, ") (", b, ") (", c,")"]) 
  <$> showE a 
  <*> showE b
  <*> showE c
showE (Defer a)  = mappend "Defer " <$> showE a
showE (Less a b) = (\a b -> concat ["Less (", a, ") (", b, ")"]) 
  <$> showE a 
  <*> showE b
showE (Minus a b) = (\a b -> concat ["Minus (", a, ") (", b, ")"]) 
  <$> showE a 
  <*> showE b
showE (Formula a)  = mappend "Formula " <$> showE a
showE (ClosureV _ s e) = do
  s' <- showE e
  pure $ concat ["ClosureV env ", show s, " (", s', ")"]
showE (ClosureF _ _ e) = do
  s' <- showE e
  pure $ concat ["ClosureF env ", " (", s', ")"]


----------------------------
-- Env related Exceptions
----------------------------

newtype VariableAlreadyDefinedException = VAD String

instance Show VariableAlreadyDefinedException where
  show (VAD varName) = concat 
    ["Variable: ", show varName, ", cannot be redifined."]

instance Exception VariableAlreadyDefinedException

newtype VariableNotDefinedException = VND String

instance Show VariableNotDefinedException where
  show (VND varName) = concat 
    ["Variable: ", show varName, ", is not defined in the environment."]

instance Exception VariableNotDefinedException

newtype BadType = BT String


instance Show BadType where
  show (BT msg) = msg

instance Exception BadType