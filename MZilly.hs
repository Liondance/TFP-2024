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
import Data.Kind (Type, Constraint)
import Data.Dynamic
import Data.Typeable
import Data.Functor.Identity

-- Zilly

----------------------------
-- Zilly Types
----------------------------

type Z = Integer
type Symbol = String
type Error = String

newtype Defered a = Defered a

instance Show a => Show (Defered a) where
  show (Defered x) = "'" <> show x <> "'"

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


class Evaluable a b where
  reduce :: a -> b

instance Evaluable a a where
  reduce = id
instance Evaluable (Defered a) a where
  reduce (Defered x) = x


class ZillySym m where
  val    :: Z -> m Z
  sym    :: Typeable a => Symbol -> T a -> m a
  lambda :: (Typeable a, Typeable b, Typeable c, Evaluable c a) => Symbol -> T a -> m b -> (m c -> m b)
  ifX    :: Typeable a => m Z -> m a -> m a -> m a
  defer  :: m a -> m (m (Defered a))
  less   :: m Z -> m Z -> m Z
  formula :: m a -> m a


-- rvalue(expr) = rvalue(cvalue(expr))


----------------------------
-- Runtime meta
----------------------------

data Binding = Binding 
  { bFormula :: Dynamic
  , bValue   :: Dynamic
  }
type K   = Symbol
type Env = Map K Binding
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


newtype ShowInterpreter a = ShowInterpreter {getString :: String}
  deriving (Semigroup,Monoid)
----------------------------
-- Runtime Functions
----------------------------

setBinding :: (Typeable a,Typeable m) => Symbol -> a -> m a  -> Env -> Env
setBinding name value formula  env = insert env name . Binding (toDyn formula) . toDyn $ value

readFromEnv :: forall a m. (Typeable a, MonadError Error m)
  => Symbol -> RT Env m a
readFromEnv k = ask >>= \e -> case lookup e k of
  Nothing -> throwError $ "Variable: " <> show k <> " not defined"
  Just (Binding _ v)  -> case fromDynamic @a v of
    Nothing -> throwError
      $ "Wrong type for variable: "
      <> show k
      <> ". Expected: " <> (show . typeRep $ Proxy @a )
      <> ", but instead got: " <> "???"
      <> ". Actual value: " <> show v
    Just v -> pure v


------------------------------
-- Evaluators and interpreters
------------------------------

instance (MonadError String m, Typeable m) => ZillySym (RT Env m) where
  val = pure
  sym var (t :: T a) = readFromEnv @a var
  
  lambda :: forall a b c m
    . (MonadError String m, Typeable a, Typeable b, Typeable c, Evaluable c a, Typeable m) 
    => Symbol -> T a -> RT Env m b -> RT Env m c -> RT Env m b
  lambda varName t body mvalue = do 
    let mvalueReduced = reduce @c @a <$> mvalue
    value <- mvalueReduced
    local (setBinding varName value mvalueReduced) body

  ifX b x y = b >>= \b -> if b > 0 then x else y
  defer = pure . fmap Defered
  less a b = (\x y -> if x < y then 1 else -1) <$> a <*> b
  formula = id


interpretZSym :: Show a => RT Env (Either Error) a -> IO ()
interpretZSym =  either putStrLn print
  . flip runReaderT M.empty 


test :: IO ()
test = interpretZSym f6
  where
    five = val 5
    f    = lambda "X" Z $ sym "X" Z `less` five
    f6   = f $ val 6


------------------------------
-- pretty printers           |
------------------------------


mkParens :: Bool -> String -> String
mkParens b s = if b then "(" <> s <> ")" else s

prettyType :: T a -> String
prettyType = prettyType' ((-1) :: Float)
  where 
    prettyType' :: Float -> T a -> String
    prettyType' _ Z = "Z"
    prettyType' pPrec (Lazy y) = mkParens (pPrec > 3) $ "lazy " <> prettyType' 3 y 
    prettyType' pPrec (Fun left@(Fun _ _) right) 
      = mkParens (pPrec > 2) $ prettyType' 2.1 left <> " -> " <> prettyType' 2 right
    prettyType' pPrec (Fun left right) 
      = mkParens (pPrec > 2) $ prettyType' 2 left <> " -> " <> prettyType' 2 right

