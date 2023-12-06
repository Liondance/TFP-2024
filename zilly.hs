--
-- Zilly
--

module Zilly where

import Prelude hiding (lookup)

-- Stack
import ADT.Stack as Stack hiding (empty)

-- Map
import Data.Map (Map)
import qualified Data.Map as Map

lookup :: Ord k => Map k v -> k -> Maybe v
lookup map k = Map.lookup k map

insert :: Ord k => Map k v -> k -> v -> Map k v
insert map k v = Map.insert k v map

delete :: Ord k => Map k v -> k -> Map k v
delete map k = Map.delete k map

update :: Ord k => Map k v -> k -> v -> Map k v
update map k v = let m = delete map k in insert m k v

-- Zilly

type Z = Integer

type Symbol = String

-- reserved words: Z, lazy, if, lt, minus, show

-- Types
data T =
      Z                 -- integer type: Z
    | Fun T T           -- function type: <type> -> <type>
    | Lazy T            -- lazy type: lazy <type>
    deriving Show

-- Expressions
data E =
      Val Z             -- integer values: 0, 1, .., 10, 11, .. 42 ..
    | Sym Symbol        -- var names, i.e. user defined symbols: a, b, .. x, y, .. zarzuela, ..
    | Lambda T E E      -- <type> <symbol> -> <exp>: (Z -> Z) x -> minus(x)(minus(0)(1))
    | Apply E E         -- function application <exp>(<exp>): ((Z -> Z) x -> minus(x)(minus(0)(1)))(41)
    | If (E, E, E)      -- conditional expression: if(<exp>,<exp>,<exp>): if(lt(x)(y), x, y)
    | Defer E           -- deferred expression '<exp>': 'uniform(0)(1)'
    | Less E E          -- less than (lt predefined function): lt(x)(y) =<>=> x < y
    | Minus E E         -- subtraction (minus predefined function): minus(x)(y) =<>=> x - y
    deriving Show

-- Statements
data Statement = 
      Define T E E      -- <type> <symbol> := <expression>;
    | Assign E E        -- <symbol> := <expression>;
    | Show (String, E)  -- magic form: show(<string>, <exp>) prints <string> ==> rvalue(<exp>)
    | Halt
    deriving Show

-- Program
type Program = [Statement]

--
-- Runtime
--

type Binding = (T, Symbol, E)
type Global  = Map Symbol Binding
type Local   = Stack Binding
type State   = (Char, Local, Global)

status :: State -> Char
status (c, _, _) = c

stack :: State -> Local
stack (_, s, _) = s

global :: State -> Global
global (_, _, g) = g


{-

-- State Monad (is it?)

-- given a state, a transformer returns a pair with the new state
newtype ST a = S (State -> (a, State))

-- function that applies a state transformer to a state
st'apply :: ST a -> State -> (a, State)
st'apply (S st) x = st x

-- ST is a Functor
instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x, s') = st'apply st s in (g x, s'))

-- ST is an Applicative Functor
instance Applicative ST where
    -- pure = a -> ST a
    pure x = S (\s -> (x, s))
    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f, s' ) = st'apply stf s
            (x, s'') = st'apply stx s' in
                (f x, s''))

-- ST is a Monad
instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = st'apply st s in st'apply (f x) s')

-}

-- context lookup

global'lookup :: Global -> Symbol -> Maybe Binding
global'lookup global symbol =
    let binding = lookup global symbol in
        case binding of
            Nothing -> Nothing
            Just _ -> binding

stack'lookup :: Local -> Symbol -> Maybe Binding
stack'lookup Empty _ = Nothing
stack'lookup (Top binding under) symbol =
    let (lty, sym, val) = binding in
    if sym == symbol then stack'lookup under symbol else Just binding

state'lookup :: State -> Symbol -> Maybe Binding
state'lookup (status, stack, global) symbol =
    let binding = stack'lookup stack symbol in
        case binding of
            Nothing -> global'lookup global symbol
            Just _ -> binding

-- ltype
ltype :: State -> E -> Maybe T
ltype gamma (Sym symbol) =
    case state'lookup gamma symbol of
        Nothing -> Nothing
        Just (lty, _, _) -> Just lty

-- cvalue
cvalue :: State -> E -> Maybe E
cvalue gamma (Sym symbol) =
    case state'lookup gamma symbol of
        Nothing -> Nothing
        Just (_, _, exp) -> Just exp

-- substitute
substitute :: State -> E -> E -> E -> E

substitute gamma _ _ (Val i) = (Val i)

substitute gamma (Sym r) arg (Sym s)
    | r == s = arg
    | otherwise = (Sym s)

substitute gamma (Sym r) arg (Lambda lty (Sym s) exp)
    | r == s = (Lambda lty (Sym s) exp)
    | otherwise = (Lambda lty (Sym s) exp') where
        exp' = substitute gamma (Sym r) arg exp


substitute gamma (Sym r) arg (Apply fun art) =
    let fun' = substitute gamma (Sym r) arg fun in
    let arg' = substitute gamma (Sym r) arg art in
    (Apply fun' arg')

substitute gamma (Sym r) arg (If (p, x, y)) =
    let p' = substitute gamma (Sym r) arg p in
    let x' = substitute gamma (Sym r) arg x in
    let y' = substitute gamma (Sym r) arg y in
        If (p', x', y')

substitute gamma (Sym r) arg (Defer exp) =
    let exp' = substitute gamma (Sym r) arg exp in
        Defer exp'

substitute gamma (Sym r) arg (Less lhs rhs) =
    let lhs' = substitute gamma (Sym r) arg lhs in
    let rhs' = substitute gamma (Sym r) arg rhs in
        Less lhs' rhs'

substitute gamma (Sym r) arg (Minus lhs rhs) =
    let lhs' = substitute gamma (Sym r) arg lhs in
    let rhs' = substitute gamma (Sym r) arg rhs in
        Minus lhs' rhs'

-- apply
apply :: State -> E -> E -> Maybe E
apply (status, stack, global) (Lambda lty (Sym symbol) exp) arg = do
    let stack' = push stack (lty, symbol, arg)
    let gamma' = (status, stack', global)
    rvalue gamma' exp

-- rvalue
rvalue :: State -> E -> Maybe E

rvalue gamma (Val i) = Just (Val i)

rvalue gamma (Sym s) = let cv = cvalue gamma (Sym s) in
    case cv of
        Nothing -> Nothing
        Just exp -> rvalue gamma exp

rvalue gamma (Lambda lty x exp) = Just (Lambda lty x exp)

rvalue gamma (Apply fun arg) = do
    arg' <- rvalue gamma arg
    fun' <- rvalue gamma fun
    case fun' of
        (Lambda lty par exp) -> do
            let exp' = substitute gamma par arg' exp
            rvalue gamma exp'
        otherwise -> Just (Apply fun' arg')
        -- otherwise IS relevent for (future) lazy-ness propagation:
        --   'f'(a) =bs=> f(rv(a)) =bs=> rv(f)(rv(rv(a)))
        --   'inc'(6 * 7 - 1) =rv=> inc(41) =rv=> 42
        -- the paper will mention lazy-ness propagation ...
        -- ... but most likely we won't have time to formalize the semantic 
        -- we should remove the otherwise clause, unless we formalize it
        -- FYI, besides LP there is another eval generalization we are exploring:
        -- alternating RV and XV (leaving eXp intact) for controlled partial evaluation.
        -- We don't know yet if these ideas are "good"

rvalue gamma (If (predicate, x, y)) =
    let p = rvalue gamma predicate in
        case p of
            Nothing -> Nothing
            Just (Val z) -> if z /= 0 then rvalue gamma x else rvalue gamma y

rvalue gamma (Defer exp) = Just exp

rvalue gamma (Less lhs rhs) = do
    l <- rvalue gamma lhs
    r <- rvalue gamma rhs
    Just (less l r)

rvalue gamma (Minus lhs rhs) = do
    l <- rvalue gamma lhs
    r <- rvalue gamma rhs
    Just (minus l r)

less :: E -> E -> E
less (Val x) (Val y) = if x < y then Val 1 else Val 0

minus :: E -> E -> E
minus (Val x) (Val y) = Val (x - y)

-- exec
exec :: State -> Statement -> State
exec (status, stack, global) (Define lty (Sym symbol) exp) =
    let exp' = rvalue (status, stack, global) exp in
    case exp' of
        Nothing -> (status, stack, global)
        Just exp'' -> (status, stack, global')
            where global' = insert global symbol (lty, symbol, exp'')

exec (status, stack, global) (Assign (Sym symbol) exp) =
    let def = global'lookup global symbol in
    case def of
        Nothing -> (status, stack, global)
        Just (lty, _, _) ->
            let exp' = rvalue (status, stack, global) exp in
            case exp' of
                Nothing -> (status, stack, global)
                Just exp'' -> (status, stack, global')
                    where global' = update global symbol (lty, symbol, exp'')

exec (status, stack, global) (Halt) = ('H', stack, global)

-- initial store
empty :: State
empty = ('R', Stack.Empty, Map.empty)

-- run program
run' store [] = store
run' state (action:actions) =
    if status state == 'H' then state else
        let store' = exec state action in
        run' store' actions

run prog = run' empty prog
