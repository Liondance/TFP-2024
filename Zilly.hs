--
-- Zilly
--

module Zilly where

import Control.ZM
import Control.IE
import ADT.ADT

--
-- Runtime
--


rvalue :: E ZME -> ZME (E ZME)
-- example
rvalue (Minus ma mb) = do
  a <- ma >>= rvalue 
  b <- mb >>= rvalue
  case (a,b) of
    (Val a',Val b') -> pure . Val $ a' - b'
    (Val _, e) -> do 
      s <- showE e
      throwM . BT $ "Error on minus, expected a value as its second argument, but got: " <> s
    (e, Val _) -> do 
      s <- showE e
      throwM . BT $ "Error on minus, expected a value as its first argument, but got: " <> s
-- Another example
rvalue (Sym s) = getVar s

run' :: Statement ZME -> ZME ()
run' = undefined