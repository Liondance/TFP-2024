module Zilly'Test where

import Zilly

-- convenience functions for interactive testing

cv :: State -> String -> E
cv state symbol =
    let cvs = cvalue state (Sym symbol) in
        case cvs of
            Nothing -> (Sym "Nothing")
            Just ex -> ex


rv :: State -> String -> E
rv state symbol =
    let rvs = rvalue state (Sym symbol) in
        case rvs of
            Nothing -> (Sym "Nothing")
            Just ex -> ex

sbx :: State -> String -> E -> E -> E
sbx state symbol arg exp = substitute state (Sym symbol) arg exp

sbv :: State -> String -> E -> String -> E
sbv state symbol arg var = substitute state (Sym symbol) arg (cv state var)

-- Test programs

prog0 :: Program
prog0 = []

prog1 =
    [
        Define    Z      (Sym "x") (Val 67),
        Define    Z      (Sym "y") (Val 25),
        Define (Lazy Z)  (Sym "z") (Defer (Minus (Sym "x") (Sym "y"))),
        Define (Lazy Z)  (Sym "l") (Defer (If (Less (Sym "x") (Sym "y"), (Sym "x"), (Sym "y"))))
    ]

prog2 =
    [
        Define (Fun (Lazy Z) (Lazy Z)) (Sym "id") (
            Lambda (Lazy Z) (Sym "x") (Formula (Sym "x"))
        ),

        Define (Fun Z Z) (Sym "chs") (
            Lambda Z (Sym "x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z (Lazy Z))) (Sym "plus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Defer (Minus (Sym "x") (Apply (Sym "chs") (Sym "y")))
                )
            )
        )
    ]

{-

prog3 =
    [
        Define    Z      (Sym "answer42") (Lambda Z (Sym "n") (Val 42)),
        Define    Z      (Sym "identity") (Lambda Z (Sym "x") (Sym "x")),
        Define    Z      (Sym "aa")       (Apply (Sym "answer42") (Apply (Sym "answer42") (Val 67))),
        Define    Z      (Sym "ai")       (Apply (Sym "answer42") (Apply (Sym "identity") (Val 67))),
        Define    Z      (Sym "ia")       (Apply (Sym "identity") (Apply (Sym "answer42") (Val 67))),
        Define    Z      (Sym "ii")       (Apply (Sym "identity") (Apply (Sym "identity") (Val 67)))
    ]

prog4 =
    [
        Define    Z      (Sym "x")   (Apply (Lambda Z (Sym "n") (Minus (Sym "n") (Val 25))) (Val 67))
    ]

prog5 =
    [
        Define (Fun Z Z) (Sym "dec") (Lambda Z (Sym "n") (Minus (Sym "n") (Val 1))),
        Define    Z      (Sym "x")   (Val 43),
        Define    Z      (Sym "y")          (Apply (Sym "dec") (Sym "x")),
        Define (Lazy Z)  (Sym "z")   (Defer (Apply (Sym "dec") (Sym "x")))
    ]

prog6 =
    [
        Define (Fun Z Z) (Sym "dec") (Lambda Z (Sym "n") (Minus (Sym "n") (Val 1))),
        Define    Z      (Sym "x")   (Val 68),
        Define    Z      (Sym "y")          (Apply (Sym "dec") (Sym "x")),
        Define (Lazy Z)  (Sym "z")   (Defer (Apply (Sym "dec") (Sym "x"))),
        Assign           (Sym "x")   (Minus (Val 43) (Minus (Sym "x") (Sym "z")))
    ]

prog7 =
    [
        Define (Fun Z (Fun Z Z)) (Sym "sub") (
            Lambda Z (Sym "s") (
                Lambda Z (Sym "n") (
                    Minus (Sym "n") (Sym "s")
                )
            )
        ),

        Define (Fun Z Z) (Sym "dec") (Apply (Sym "sub") (Val 1)),
        Define    Z      (Sym "x67") (Apply (Sym "dec") (Val 68)),
        Define    Z      (Sym "x42") (Apply (Apply (Sym "sub") (Val 25)) (Sym "x67")),

        Halt,
        Define    Z      (Sym "x00") (Val 0),
        Halt
    ]


prog8 =
    [
        Define (Fun Z (Fun Z Z)) (Sym "plus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Minus (Sym "x") (Minus (Val 0) (Sym "y"))
                )
            )
        ),

        Define       Z  (Sym "x") (Val 19),
        Define       Z  (Sym "y") (Val 23),
        Define (Lazy Z) (Sym "z") (Defer ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y")))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 25)),
        Define       Z  (Sym "b") (Sym "z")
    ]

prog9 =
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z (Sym "x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z Z)) (Sym "plus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Minus (Sym "x") (Apply (Sym "chs") (Sym "y"))
                )
            )
        ),

        Define       Z  (Sym "x") (Val 19),
        Define       Z  (Sym "y") (Val 23),
        Define (Lazy Z) (Sym "z") (Defer ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y")))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 25)),
        Define       Z  (Sym "b") (Sym "z")
    ]

progA =
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z (Sym "x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z (Lazy Z))) (Sym "plus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Defer (Minus (Sym "x") (Apply (Sym "chs") (Sym "y")))
                )
            )
        ),

        Define       Z  (Sym "x") (Val 19),
        Define       Z  (Sym "y") (Val 23),
        Define (Lazy Z) (Sym "z") ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y"))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 25)),
        Define       Z  (Sym "b") (Sym "z")
    ]

progB =
    [
    ]

progC =
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z (Sym "x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z (Lazy Z))) (Sym "plus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Apply (
                        Lambda (Lazy Z) (Sym "x") (Formula (Sym "x"))
                    ) (
                        Defer (Minus (Sym "x") (Apply (Sym "chs") (Sym "y")))
                    )
                )
            )
        ),

        Define       Z  (Sym "x") (Val 19),
        Define       Z  (Sym "y") (Val 23),
        Define (Lazy Z) (Sym "z") ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y"))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 25)),
        Define       Z  (Sym "b") (Sym "z")
    ]

progD =
    [
    ]

progE =
    [
    ]

progF =
    [
        Define (Fun Z Z) (Sym "square") (Lambda Z (Sym "a") (Minus (Sym "n") (Val 1))),
        Define (Fun Z (Fun Z Z)) (Sym "escape") (
            Lambda Z (Sym "a") ((Lambda Z (Sym "b") (
                Minus (Sym "a") (Sym "b")
            )))
        )
    ]
-}
