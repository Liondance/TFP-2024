module Control.ZM where

import GHC.Stack
import Control.Exception hiding (catchJust)
import Data.Dynamic 
newtype ZM s a = ZM { runZillyM :: s -> IO (s,a) } deriving (Typeable)

instance Monad (ZM s) where
  return = pure
  ma >>= f = ZM $ \s -> do
    (s',a) <- runZillyM ma s
    runZillyM (f a) s'

instance Applicative (ZM s) where
  pure x = ZM $ \s -> pure (s,x)
  mf <*> mx  = do
    f <- mf
    x <- mx
    pure $ f x

instance Functor (ZM s) where
  fmap f mx = do
    x <- mx
    pure $ f x
 
-------------
-- MonadIO 
-------------

liftIO :: IO a -> ZM s a
liftIO ma = ZM $ \s -> (\a -> (s,a)) <$> ma

----------------
-- Monad Reader
----------------

ask :: ZM s s
ask = ZM $ \s -> pure (s,s)

local :: (s -> s) -> ZM s a -> ZM s a
local f ma = ZM $ runZillyM ma . f

reader :: (r -> a) -> ZM r a
reader f = ZM $ \s -> pure (s, f s)

asks :: (r -> a) -> ZM r a
asks = reader

----------------
-- Monad State
----------------

get :: ZM s s
get = ask

put :: s -> ZM s ()
put s = ZM $ \_ -> pure (s,())

state :: (s -> (s, a)) -> ZM s a
state f = ZM $ pure . f

modify :: (s -> s) -> ZM s ()
modify f = ZM $ \s -> pure (f s, ())

gets :: (s -> a) -> ZM s a
gets  = asks

----------------
-- Monad Throw
----------------

throwM :: HasCallStack => Exception e => e -> ZM s a
throwM e = ZM $ \_ -> throwIO e

----------------
-- Monad Catch
----------------

catchM :: HasCallStack => Exception e => ZM s a -> (e -> ZM s a) -> ZM s a
catchM ma handler = ZM $ \s -> runZillyM ma s `catch` (flip runZillyM s  . handler)

catchJust :: HasCallStack => Exception e => (e -> Maybe b) -> ZM s a -> (b -> ZM s a) -> ZM s a
catchJust f ma h = withFrozenCallStack catchM ma $ \e -> case f e of
  Just b  -> h b
  Nothing -> throwM e

catchIf :: HasCallStack => Exception e => (e -> Bool) -> ZM s a -> (e -> ZM s a) -> ZM s a
catchIf f ma h = withFrozenCallStack catchM ma $ \e -> if f e then h e else throwM e

handle :: HasCallStack => Exception e => (e -> ZM s a) -> ZM s a -> ZM s a
handle = flip (withFrozenCallStack catchM)

handleJust :: HasCallStack => Exception e => (e -> Maybe b) -> (b -> ZM s a) -> ZM s a -> ZM s a
handleJust = fmap flip (withFrozenCallStack catchJust)

handleIf :: HasCallStack => Exception e => (e -> Bool) -> (e -> ZM s a) -> ZM s a -> ZM s a
handleIf = fmap flip (withFrozenCallStack catchIf)

try :: HasCallStack => Exception e => ZM s a -> ZM s (Either e a)
try ma = withFrozenCallStack catchM (fmap Right ma) (pure . Left)

tryJust :: HasCallStack => Exception e => (e -> Maybe b) -> ZM s a -> ZM s (Either b a)
tryJust f ma = withFrozenCallStack catchM (fmap Right ma) $ \e -> case f e of
  Just e  -> pure . Left $ e
  Nothing -> throwM e

