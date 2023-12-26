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
data E m =
      Val Z                                    -- ^ integer values: 0, 1, .., 10, 11, .. 42 ..
    | Sym Symbol                               -- ^ var names, i.e. user defined symbols: a, b, .. x, y, .. zarzuela, ..
    | Lambda T String (m (E m))                -- ^ <type> <symbol> -> <exp>: (Z -> Z) x -> minus(x)(minus(0)(1))
    | Apply   (m (E m)) (m (E m))              -- ^ function application <exp>(<exp>): ((Z -> Z) x -> minus(x)(minus(0)(1)))(41)
    | If      (m (E m)) (m (E m)) (m (E m))    -- ^ conditional expression: if(<exp>,<exp>,<exp>): if(lt(x)(y), x, y)
    | Defer   (m (E m))                        -- ^ deferred expression '<exp>': 'uniform(0)(1)'
    | Less    (m (E m)) (m (E m))              -- ^ less than (lt predefined function): lt(x)(y) =<>=> x < y
    | Minus   (m (E m)) (m (E m))              -- ^ subtraction (minus predefined function): minus(x)(y) =<>=> x - y
    | Formula (m (E m))
    deriving Typeable

-- Statements
data Statement m = 
      Define T (E m) (E m)      -- ^ <type> <symbol> := <expression>;
    | Assign (E m) (E m)        -- ^ <symbol> := <expression>;
    | Show String (E m)         -- ^ magic form: show(<string>, <exp>) prints <string> ==> rvalue(<exp>)
    deriving Typeable
-- Program
type Program m = [Statement m]
