{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--
-- Zilly
--

module MZilly where

import Prelude hiding (lookup)

-- Stack
import ADT.Stack as Stack hiding (empty)
-- Map
import ADT.Map (Map, insert, lookup, update)
import qualified ADT.Map as M

-- Functor/Applicative/Monad utilities
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Kind (Type)
import Data.Dynamic
import Data.Typeable

-- Zilly

----------------------------
-- Zilly Types
----------------------------

type Z = Integer
type Symbol = String
type Error = String

newtype Defered a = Defered a
newtype Var a     = Var a


data MetaType = MVal | MSym | MLambda | MApply | MIf | MDefer | MLess | MMinus
  deriving Typeable

data Meta (mtype :: MetaType)
  deriving Typeable

-- reserved words: Z, lazy, if, lt, minus, show
data T (a :: Type) where
  Z :: T Z
  Lazy :: T a -> T (Defered  a)
  Fun  :: T a -> T b -> T (a -> b)
  deriving Typeable
-- Expressions
data E meta a  where
  Val    :: Z        -> E MVal (T Z)
  Sym    :: (Typeable a) => Symbol   -> T a -> E MSym (T a)
  Lambda :: (Typeable a, Typeable b) => E MSym (T a) -> E meta (T b) -> E MLambda (T (a -> b))
  Apply  :: (Typeable a, Typeable b) => E x (T (a -> b)) -> E y (T a) -> E MApply (T b)
  If     :: E x0 (T Z) -> E x1 (T a) -> E x2 (T a) -> E MIf (T a)
  Defer  :: E x (T a) -> E MDefer (T (Defered a))
  Less   :: E x0 (T Z) -> E x1 (T Z) -> E MLess (T Z)
  Minus  :: E x0 (T Z) -> E x1 (T Z) -> E MMinus (T Z)
  deriving Typeable


-- rvalue(expr) = rvalue(cvalue(expr))



----------------------------
-- Runtime meta
----------------------------


type K   = Symbol
type Env = Map K Dynamic
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


getBinding :: forall a m meta.
  (Typeable a, Typeable m, Typeable meta, Monad m ) => K -> R m (Maybe (T a, E meta a))
getBinding = fmap join . fmap2 fromDynamic  . asks . flip lookup

getType :: forall (meta :: MetaType) m a.
  (Typeable a, Typeable m, Typeable meta, Monad m ) => K -> R m (Maybe (T a))
getType =  fmap2 fst . getBinding @a @m @meta


getValue :: forall (meta :: MetaType) m a.
  (Typeable a, Typeable m, Typeable meta, Monad m ) => K -> R m (Maybe (E meta a))
getValue =  fmap2 snd . getBinding @a @m @meta

setVar :: (Typeable a, Typeable meta) => T a -> K -> E meta (T a) -> Env -> Env
setVar t n e m = insert m n $ toDyn (t,e) 


{- setVar' :: (Typeable m, Typeable a, Typeable meta) => K -> E meta (T a) -> Env -> Env
setVar' n e m = let x = case typeOf @a of
   insert m n $ toDyn (t,e)  -}

------------------------------
-- Evaluators and interpreters
------------------------------

rvalue :: (Typeable m, Typeable a, MonadError Err m)  => R m (E meta a) -> R m (E meta a)
rvalue me = me >>= \e -> case e of
  Val x   -> pure . Val $ x
  Sym s _ -> getValue @MSym s >>= maybe (throwError . variableNotBoundError $ s) (rvalue . pure) 
  l@Lambda {} -> pure l
  Apply (Lambda (Sym v (t :: T t0)) body) (mArg :: E (m1 :: MetaType) (T t2)) -> do
    arg <- rvalue $ pure mArg
    let x = setVar t v arg
    undefined
  _ -> undefined
  where
    variableNotBoundError s = concat ["Symbol: ", show s, " not Bound to any value"]

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

