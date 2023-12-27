module Zilly'Test where

import Zilly

-- convenience functions for interactive testing

cv :: State -> String -> E
cv state symbol =
    let cvs = cvalue state (Sym symbol) in
        case cvs of
            Error x -> Error x
            cve -> cve

rv :: State -> String -> E
rv state symbol =
    let rvs = rvalue state (Sym symbol) in
        case rvs of
            Error x -> Error x
            rve -> rve

sbx :: State -> String -> E -> E -> E
sbx state symbol arg exp = substitute state (Sym symbol) arg exp

sbv :: State -> String -> E -> String -> E
sbv state symbol arg var = substitute state (Sym symbol) arg (cv state var)

-- Test programs

prog0 :: Program
prog0 = []

prog1 = -- cvalue, rvalue, and identity
    [
        Define (Fun (Lazy Z) Z) (Sym "rv") (
            Lambda (Lazy Z) (Sym "x") (Sym "x")
        ),

        Define (Fun (Lazy Z) (Lazy Z)) (Sym "id") (
            Lambda (Lazy Z) (Sym "x") (Formula (Sym "x"))
        ),

        Define (Z) (Sym "x") (Val 7),

        Define (Z) (Sym "y") (Val 8),

        Define (Lazy Z) (Sym "z") (Defer (Minus (Sym "x") (Minus (Val 0) (Sym "y")))),

        Define (Lazy Z) (Sym "t") (Minus (Sym "x") (Minus (Val 0) (Sym "y"))),

        Show "cv(z)" (Formula (Sym "z")),

        Show "rv(z)" (Apply (Sym "rv") (Sym "z")),

        Show "id(z)" (Apply (Sym "id") (Sym "z")),

        Show "id(cv(z))" (Apply (Sym "id") (Formula (Sym "z"))),

        Show "rv(cv(z))" (Apply (Sym "rv") (Formula (Sym "z"))),

        Halt
    ]

prog2 = -- deferred evaluation and mutation
    [
        Define    Z      (Sym "x") (Val 67),
        Define    Z      (Sym "y") (Val 72),

        Define (Lazy Z)  (Sym "min") (Defer (If (Less (Sym "x") (Sym "y"), (Sym "x"), (Sym "y")))),
        Define    Z      (Sym "t00") (Sym "min"),

        Assign (Sym "y") (Minus (Sym "y") (Val 30)),

        Define    Z      (Sym "t01") (Sym "min"),

        Halt
    ]

prog3 = -- Curried function application
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

        Define (Fun Z (Fun Z Z)) (Sym "minus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Minus (Sym "x") (Sym "y")
                )
            )
        ),

        Define Z (Sym "x") (Apply (Apply (Sym "plus") (Val 42)) (Val 25)),
        Define Z (Sym "y") (Apply (Apply (Sym "minus") (Sym "x")) (Val 25)),

        Halt
    ]

prog4 = -- partial evaluation
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z (Sym "x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z Z)) (Sym "add") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Minus (Sym "y") (Apply (Sym "chs") (Sym "x"))
                )
            )
        ),

        Define (Fun Z (Fun Z Z)) (Sym "sub") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Minus (Sym "y") (Sym "x")
                )
            )
        ),

        Define Z (Sym "add25") (Apply (Sym "add") (Val 25)),
        Define Z (Sym "sub25") (Apply (Sym "sub") (Val 25)),

        Define Z (Sym "x") (Apply (Sym "add25") (Val 42)),
        Define Z (Sym "y") (Apply (Sym "sub25") (Sym "x")),

        Halt
    ]

prog5 = -- scope + partial application with non-local context
    [
        Define Z (Sym "x") (Val 42),

        Define (Fun Z (Fun Z Z)) (Sym "nesty") (
            Lambda Z (Sym "a") (
                Lambda Z (Sym "b") (
                    Minus (Sym "x") (Minus (Sym "a") (Sym "b"))
                )
            )
        ),

        Define Z (Sym "z67") (Apply (Apply (Sym "nesty") (Val 7)) (Val 32)),
        Assign (Sym "x") (Sym "z67"),
        Define Z (Sym "z42") (Apply (Apply (Sym "nesty") (Val 32)) (Val 7)),
        Assign (Sym "x") (Sym "z42"),
        Define (Fun Z Z) (Sym "m7") (Apply (Sym "nesty") (Val 7)),
        Assign (Sym "x") (Apply (Sym "m7") (Val 32)),

        Halt
    ]

prog6 = -- scope + partial application with non-local context and lazy expressions
    [
        Define (Lazy Z) (Sym "x") (Val 42),

        Define (Fun Z (Fun Z (Lazy Z))) (Sym "nesty") (
            Lambda Z (Sym "a") (
                Lambda Z (Sym "b") (
                    Defer (Minus (Sym "x") (Minus (Sym "a") (Sym "b")))
                )
            )
        ),
{-
        Define (Lazy Z) (Sym "z67") (Apply (Apply (Sym "nesty") (Val 7)) (Val 32)),
        Assign (Sym "x") (Sym "z67"),
        Define (Lazy Z) (Sym "z42") (Apply (Apply (Sym "nesty") (Val 32)) (Val 7)),
        Assign (Sym "x") (Sym "z42"),
-}
        Define (Fun Z (Lazy Z)) (Sym "m7") (Apply (Sym "nesty") (Val 7)),
        Assign (Sym "x") (Apply (Sym "m7") (Val 32)),
        Halt
    ]

prog7 =
    [
        Define (Fun Z Z) (Sym "dec") (Lambda Z (Sym "n") (Minus (Sym "n") (Val 1))),
        Define    Z      (Sym "x")   (Val 43),
        Define    Z      (Sym "y")          (Apply (Sym "dec") (Sym "x")),
        Define (Lazy Z)  (Sym "z")   (Defer (Apply (Sym "dec") (Sym "x"))),

        Halt,

        Define (Fun Z Z) (Sym "dec") (Lambda Z (Sym "n") (Minus (Sym "n") (Val 1))),
        Define    Z      (Sym "x")   (Val 68),
        Define    Z      (Sym "y")          (Apply (Sym "dec") (Sym "x")),
        Define (Lazy Z)  (Sym "z")   (Defer (Apply (Sym "dec") (Sym "x"))),
        Assign           (Sym "x")   (Minus (Val 43) (Minus (Sym "x") (Sym "z"))),

        Halt
    ]

prog8 =
    [
        Define (Fun Z Z) (Sym "dec") (Apply (Sym "sub") (Val 1)),
        Define    Z      (Sym "x67") (Apply (Sym "dec") (Val 68)),
        Define    Z      (Sym "x42") (Apply (Apply (Sym "sub") (Val 25)) (Sym "x67")),
        Define    Z      (Sym "x00") (Val 0),

        Halt
    ]


prog9 =
    [
        Define (Fun Z (Fun Z (Lazy Z))) (Sym "plus") (
            Lambda Z (Sym "x") (
                Lambda Z (Sym "y") (
                    Defer (Minus (Sym "x") (Apply (Sym "chs") (Sym "y")))
                )
            )
        ),

        Define       Z  (Sym "x") (Val 19),
        Define       Z  (Sym "y") (Val 23),
        Define (Lazy Z) (Sym "z") (Defer ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y")))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 25)),
        Define       Z  (Sym "b") (Sym "z"),

        Halt
    ]

progA =
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
        Define       Z  (Sym "b") (Sym "z"),

        Halt
    ]

progB =
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
        Define       Z  (Sym "b") (Sym "z"),

        Halt
    ]

progC =
    [
    ]

progD =
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
        Define       Z  (Sym "b") (Sym "z"),

        Halt
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
        ),

        Halt
    ]

fibo =
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

        Define (Fun Z Z) (Sym "fibo") (
            Lambda Z (Sym "n") (
                If (Less (Sym "n") (Val 2), (Sym "n"),
                    Apply (Apply (Sym "plus")
                        (Apply (Sym "fibo") (Minus (Sym "n") (Val 1))))
                        (Apply (Sym "fibo") (Minus (Sym "n") (Val 2)))
                )
            )
        ),

        Halt
    ]

{-

    Lazy - Defer
    Formula
    Curry
    Partial evaluation
    Escapes

-}
