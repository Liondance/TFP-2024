module Control.ZM where

import GHC.Stack
import Control.Exception hiding (catchJust)
import Data.Dynamic 

-- un monad App

newtype ZM s a = ZM { runZillyM :: s -> IO a } deriving (Typeable)

instance Monad (ZM s) where
  return = pure
  ma >>= f = ZM $ \s -> do
    a <- runZillyM ma s
    runZillyM (f a) s

instance Applicative (ZM s) where
  pure x = ZM $ \_ -> pure x
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
liftIO ma = ZM $ const  ma

----------------
-- Monad Reader
----------------

ask :: ZM s s
ask = ZM $ \s -> pure s

local :: (s -> s) -> ZM s a -> ZM s a
local f ma = ZM $ runZillyM ma . f

reader :: (r -> a) -> ZM r a
reader f = ZM $ \s -> pure $ f s

asks :: (r -> a) -> ZM r a
asks = reader


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

