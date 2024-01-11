{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TypeApplications #-}
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
import ADT.Types 
import Data.Dynamic
import Control.Concurrent.MVar
import Control.Exception
import Data.Functor
import Data.Traversable
import Data.Tree
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

showE' :: Bool -> E Env -> ZME String
showE' showEnv = fmap (drawTree . fmap getUnquotedText) . mapClosureEnv
  where
    mapClosureEnv :: E Env -> ZME (Tree UnquotedText)
    mapClosureEnv (ClosureV e v b) = do 
      e' <- for (Map.delete v e) $ \mvar -> do 
        var <- (liftIO . readMVar) mvar
        case fromDynamic var of
          Just (ClosureV {}) -> pure $ UT "function"
          Just a  -> UT <$> showE' showEnv a
          Nothing -> error "impossible case"
      b' <- mapClosureEnv b
      pure $ Node 
        { rootLabel = UT "ClosureV "
        , subForest = 
            [ Node (UT "env") $ if showEnv 
              then 
                Map.toList e' >>= \(key,item) ->
                  pure $ Node (UT $ show key <> ":- " <> show item) []
              else []
            , Node (UT $ "arg: " <> v) []
            , Node (UT "body") [b']
            ]
        }
    mapClosureEnv (ClosureF env e) = do
      env' <- for env $ \mvar -> do 
        var <- (liftIO . readMVar) mvar
        case fromDynamic var of 
          Just a  -> UT <$> showE' showEnv a
          Nothing -> error "impossible case"
      e' <- mapClosureEnv e
      pure $ Node 
        { rootLabel = UT "ClosureF "
        , subForest = 
            [ Node (UT "env") $ if showEnv 
                then Map.toList env' >>= \(key,item) ->
                  pure $ Node (UT $ show key <> ":- " <> show item) []
                else []
            , Node (UT "body") [e']
            ]
        }
    mapClosureEnv (Val x) = pure $ Node (UT . show $ Val @() x) []
    mapClosureEnv (Sym x) = pure $ Node (UT . show $ Sym @() x) []
    mapClosureEnv (Lambda t v b) = do
      b' <- mapClosureEnv b
      pure $ Node (UT "Lambda") 
        [ Node (UT $ "Type: " <> show t) []
        , Node (UT $ "Arg: " <> v) []
        , Node (UT "body:") [b']
        ]
    mapClosureEnv (Apply f x) = do 
      f' <- mapClosureEnv f
      x' <- mapClosureEnv x
      pure $ Node (UT "Apply") [Node (UT "function") [f'], Node (UT "arg") [x']]

    mapClosureEnv (If b t f)  = do 
      b' <- mapClosureEnv b
      t' <- mapClosureEnv t
      f' <- mapClosureEnv f
      pure $ Node (UT "If") 
        [ Node (UT "cond") [b']
        , Node (UT "true branch") [t']
        , Node (UT "false branch") [f']
        ]
    mapClosureEnv (Defer x)   = do 
      x' <- mapClosureEnv x
      pure $ Node (UT "Defer") [x']
    mapClosureEnv (Less a b)  = do
      a' <- mapClosureEnv a
      b' <- mapClosureEnv b
      pure $ Node (UT "Less") [a',b']
    mapClosureEnv (Minus a b) = do
      a' <- mapClosureEnv a
      b' <- mapClosureEnv b
      pure $ Node (UT "Minus") [a',b']
    mapClosureEnv (Formula x) = do 
      x' <- mapClosureEnv x
      pure $ Node (UT "Formula") [x']

showE :: E Env -> ZME String
showE = showE' False

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