{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use lambda-case" #-}
--
-- Zilly
--

module ZillyT where

import Prelude hiding (lookup)

-- Stack
import ADT.Stack as Stack hiding (empty)
-- Map
import ADT.Map (Map, insert, lookup, update)
import qualified ADT.Map as Map

-- Functor/Applicative/Monad utilities
import Data.Functor
import Control.Applicative
import Control.Monad

-- Zilly

----------------------------
-- Zilly Types
----------------------------

type Z = Integer

type Symbol = String

newtype Defered a = Defered a
newtype Var a     = Var a
-- reserved words: Z, lazy, if, lt, minus, show
data T a where
  Z :: T Int
  Lazy :: T a -> T (Defered  a)
  Fun  :: T a -> T b -> T (a -> b)

-- Expressions
data E m a where
  Val    :: Z        -> E m Z
  Sym    :: Symbol   -> T a -> E m (Var a)  
  Lambda :: E m (Var a)  -> E m b -> E m (a -> b)
  Apply  :: E m (a -> b) -> E m a -> E m b
  If     :: E m Z -> E m a -> E m a -> E m a
  Defer  :: E m a -> E m (Defered a)
  Less   :: E m Z -> E m Z -> E m Z
  Minus  :: E m Z -> E m Z -> E m Z




----------------------------
-- Runtime meta
----------------------------

type Binding m a = E m a

type K = Symbol
type V = Binding
type Env = Map K V
type Err = String

fmap2 :: (Functor f1, Functor f2) 
  => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap

-- | Reader (transformer) datatype. Allows for variable (static) binding.
newtype RT s m a = R {runReaderT :: s -> m a}

type R m a = RT Env m a

instance Functor m => Functor (RT s m) where
    fmap g = R . fmap2 g . runReaderT


instance Applicative m => Applicative (RT s m) where
    pure  = R . const . pure
    stf <*> stx = R $ (<*>) <$> runReaderT stf <*> runReaderT stx

instance Monad m => Monad (RT s m) where
  return = pure
  mx >>= f  = R $ \s -> do
    x <- runReaderT mx s 
    runReaderT (f x) s



withReaderT :: forall r' r m a. (r' -> r) -> RT r m a -> RT r' m a
withReaderT f x = R $ runReaderT x . f

ask :: Monad m => RT r m r
ask = R pure

local :: (r -> r) -> RT r m a -> RT r m a
local = withReaderT

asks :: Monad m => (r -> a) -> RT r m a
asks f = f <$> ask

mapReaderT :: (m a -> n b) -> RT r m a -> RT r n b
mapReaderT f x = R $ f . runReaderT x

class Monad m => MonadError  e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError () Maybe where
  throwError = const Nothing
  catchError ma f = maybe (f ()) pure ma

instance MonadError e (Either e) where
  throwError = Left
  catchError ma f = either f pure ma

instance (MonadError e m) => MonadError e (RT r m) where
  throwError  = R . const . throwError 
  catchError :: MonadError e m => RT r m a -> (e -> RT r m a) -> RT r m a
  catchError ma f = R $ \s -> catchError 
    (runReaderT ma s) (\e ->  runReaderT (f e) s)


----------------------------
-- Runtime Functions
----------------------------

getBinding :: Monad m => K -> R m (Maybe Binding)
getBinding = asks . flip lookup

getType :: Monad m => K -> R m (Maybe T)
getType = fmap2 (\(t,_,_) -> t) . getBinding

getSymbol :: Monad m => K -> R m (Maybe Symbol)
getSymbol = fmap2 (\(_,s,_) -> s) . getBinding

getValue :: Monad m => K -> R m (Maybe E)
getValue = fmap2 (\(_,_,e) -> e) . getBinding

setVar :: T -> K -> E -> Env -> Env
setVar t n e m = insert m n (t,n,e) 

------------------------------
-- Evaluators and interpreters
------------------------------

-- | typechecks and evals. Not a good idea in general, but better approaches require GADTs
-- or other constructs
{- rvalue :: MonadError Err m  => R m E -> R m E
rvalue me = me >>= \e -> case e of
  Val z -> pure . Val $ z
  Sym s -> getValue s >>= maybe (throwError . variableNotBoundError $ s) (rvalue . (me $>))
  l@(Lambda {}) -> me $> l 
  Apply l e -> do
    reducedE <- rvalue $ me $> e
    case l of
      Sym mf -> do
        f <- rvalue mf
        rvalue (me $> Apply f reducedE) `catchError` \e -> e <> ". On binding: " <> show f
      Lambda t (Sym s) body -> local (setVar t s reducedE) $ me $> body
      l@(Lambda {}) -> throwError  
        $ "Lambdas can only bind variables. But instead got: " 
        <> show (prettyExp l)
      _ -> throwError
        $ "Function application only possible over lambda expressions and functions, but instead got:"
        <> show (prettyExp l)
  If a b c -> (\x y z -> if x then y else z) 
    <$> rvalue (me $> a) 
    <*> rvalue (me $> b) 
    <*> rvalue (me $> c) 
  Defer x  -> me $> x
  Less a b -> liftA2 (,) (rvalue $ me $> a) (rvalue $ me $> b) >>= \x -> case x of
    (Val a, Val b) -> me $> if (a < b) then 1 else 0
    
  _ -> undefined
  where
    variableNotBoundError s = concat ["Symbol: ", show s, " not Bound to any value"] -}

