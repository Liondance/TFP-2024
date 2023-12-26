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

defineVar :: T -> String -> E ZME -> ZME ()
defineVar _ varName varBody = asks (lookup varName) >>= \a -> case a of
  Just untypedVar -> throwM $ VAD varName
  Nothing         -> do
    value <- liftIO $ newMVar (toDyn varBody)
    modify (insert varName value)

assignVar :: T -> String -> E ZME -> ZME ()
assignVar _ varName varBody = asks (lookup varName) >>= \a -> case a of
  Just untypedVar -> void . liftIO $ swapMVar untypedVar (toDyn varBody)
  Nothing         -> throwM $ VND varName


getVar :: String -> ZME (E ZME)
getVar varName = asks (lookup varName) >>= \a -> case a of
  Just untypedVar -> do 
    dynValue <- liftIO $ readMVar untypedVar
    case fromDynamic dynValue of
      Just value  -> pure value
      Nothing     -> throwM . BT . concat $ ["Variable: ", show varName, ", Has an incompatible type."]
  Nothing         -> throwM $ VND varName

showE :: E ZME -> ZME String
showE (Val n) = pure . concat $ ["Val ", show n]
showE (Sym s) = pure . concat $ ["Sym ", show s]
showE (Lambda t s e) = (\e' -> concat ["Lambda (", show t, ") ", show s, e']) <$> (e >>= showE)
showE (Apply f x)    = (\a b -> concat ["Apply (", a, ") (", b, ")"]) <$> (f >>= showE) <*> (x >>= showE)
showE (If a b c)     = (\a b c -> concat ["If (", a, ") (", b, ") (", c,")"]) 
  <$> (a >>= showE) 
  <*> (b >>= showE)
  <*> (c >>= showE)
showE (Defer a)  = mappend "Defer " <$> (a >>= showE)
showE (Less a b) = (\a b -> concat ["Less (", a, ") (", b, ")"]) 
  <$> (a >>= showE) 
  <*> (b >>= showE)
showE (Minus a b) = (\a b -> concat ["Minus (", a, ") (", b, ")"]) 
  <$> (a >>= showE) 
  <*> (b >>= showE)
showE (Formula a)  = mappend "Formula " <$> (a >>= showE)



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