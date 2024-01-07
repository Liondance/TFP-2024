module ADT.ADT where

import Data.Dynamic

-- Zilly

type Z = Integer

type Symbol = String

-- reserved words: Z, lazy, if, lt, minus, show

-- Types
data T =
      Z                 -- integer type: Z
    | Fun T T           -- function type: <type> -> <type>
    | Lazy T            -- lazy type: lazy <type>
    deriving (Eq,Show, Typeable)

-- Expressions
data E e =
      Val Z                                    -- ^ integer values: 0, 1, .., 10, 11, .. 42 ..
    | Sym Symbol                               -- ^ var names, i.e. user defined symbols: a, b, .. x, y, .. zarzuela, ..
    | Lambda T String (E e)                -- ^ <type> <symbol> -> <exp>: (Z -> Z) x -> minus(x)(minus(0)(1))
    | Apply   (E e) (E e)              -- ^ function application <exp>(<exp>): ((Z -> Z) x -> minus(x)(minus(0)(1)))(41)
    | If      (E e) (E e) (E e)    -- ^ conditional expression: if(<exp>,<exp>,<exp>): if(lt(x)(y), x, y)
    | Defer   (E e)                        -- ^ deferred expression '<exp>': 'uniform(0)(1)'
    | Less    (E e) (E e)              -- ^ less than (lt predefined function): lt(x)(y) =<>=> x < y
    | Minus   (E e) (E e)              -- ^ subtraction (minus predefined function): minus(x)(y) =<>=> x - y
    | Formula (E e)
    | ClosureV e String (E e) 
    | ClosureF e (E e)
    deriving Typeable


-- Statements
data Statement e = 
      Define T (E e) (E e)      -- ^ <type> <symbol> := <expression>;
    | Assign (E e) (E e)        -- ^ <symbol> := <expression>;
    | Show String (E e)         -- ^ magic form: show(<string>, <exp>) prints <string> ==> rvalue(<exp>)
    | Branch (E e) [Statement e] [Statement e]
    | While (E e) [Statement e]
    deriving Typeable
-- Program
type Program e = [Statement e]


