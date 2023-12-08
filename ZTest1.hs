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

{-
    Sample Program

    [
        Z -> Z chs := Z x -> minus(0)(x);
        Z -> (Z -> Z) plus := (Z x -> (Z y -> minus(x)(chs(y))));
        Z x := 7;
        Z y := 8;
        Z q := plus(x)(y);
        show("q ==> ", q);              -- imprime: q ==> 15
        lazy Z z := 'plus(x)(y)'
        show("z ==> ", z);              -- imprime: z ==> 15
        x := plus(x)(1);
        show("q ==> ", q);              -- imprime: q ==> 15
        show("z ==> ", z);              -- imprime: z ==> 16
    ]
-}

prog0 :: Program
prog0 = []

prog1 =
    [
        Define    Z      (Sym "x") (Val 67),
        Define    Z      (Sym "y") (Val 25),
        Define (Lazy Z)  (Sym "z") (Defer (Minus (Sym "x") (Sym "y")))
    ]

prog2 =
    [
        Define    Z      (Sym "x") (Val 42),
        Define    Z      (Sym "y") (Val 67),
        Define (Lazy Z)  (Sym "z") (Defer (If (Less (Sym "x") (Sym "y"), (Sym "x"), (Sym "y"))))
    ]

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
        Define (Fun Z Z) (Sym "square") (Lambda Z (Sym "a") (Minus (Sym "n") (Val 1))),
        Define (Fun Z (Fun Z Z)) (Sym "escape") (
            Lambda Z (Sym "a") ((Lambda Z (Sym "b") (
                Minus (Sym "a") (Sym "b")
            )))
        )
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

        Define       Z  (Sym "x") (Val 21),
        Define       Z  (Sym "y") (Val 21),
        Define (Lazy Z) (Sym "z") (Defer ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y")))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 25)),
        Define       Z  (Sym "b") (Sym "z")

    ]
