module ZTest1 where

import Zilly
import ADT.ADT

-- convenience functions for interactive testing

{- cv :: State -> String -> E
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
sbv state symbol arg var = substitute state (Sym symbol) arg (cv state var) -}

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

prog1 = -- cvalue, rvalue, and identity
    [
        Define (Fun (Lazy Z) Z) (Sym "rv") (
            Lambda (Lazy Z) ("x") (Sym "x")
        ),

        Define (Fun (Lazy Z) (Lazy Z)) (Sym "id") (
            Lambda (Lazy Z) ("x") (Formula (Sym "x"))
        ),

        Define (Z) (Sym "x") (Val 7),

        Define (Z) (Sym "y") (Val 8),

        Define (Lazy Z) (Sym "z") (Defer (Minus (Sym "x") (Minus (Val 0) (Sym "y")))),

        Define (Lazy Z) (Sym "t") (Minus (Sym "x") (Minus (Val 0) (Sym "y"))),
{- 
        Show "cv(z) ==> " (Formula (Sym "z")),

        Show "rv(z) ==> " (Apply (Sym "rv") (Sym "z")),

        Show "id(z) ==> " (Apply (Sym "id") (Sym "z")),

        Show "id(cv(z)) ==> " (Apply (Sym "id") (Formula (Sym "z"))), -}
        -- ACA
        Show "rv(cv(z)) ==> " (Apply (Sym "rv") (Formula (Sym "z"))),

        Show "z ==> " (Sym "z"),

        Show "t ==> " (Sym "t")
        
    ]

prog2 = -- deferred evaluation and mutation
    [
        Define    Z      (Sym "x") (Val 67),
        Define    Z      (Sym "y") (Val 72),

        Define (Lazy Z)  (Sym "min") (Defer (If (Less (Sym "x") (Sym "y")) (Sym "x") (Sym "y"))),
        Define    Z      (Sym "t00") (Sym "min"),
        Show "t00 ==> " (Sym "t00"),
        Assign (Sym "y") (Minus (Sym "y") (Val 30)),

        Define    Z      (Sym "t01") (Sym "min"),
        Show "t00 ==> " (Sym "t00"),
        Show "t01 ==> " (Sym "t01")
        
    ]

prog3 = -- Curried function application
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z ("x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z Z)) (Sym "plus") (
            Lambda Z ("x") (
                Lambda Z ("y") (
                    Minus (Sym "x") (Apply (Sym "chs") (Sym "y"))
                )
            )
        ),

        Define (Fun Z (Fun Z Z)) (Sym "minus") (
            Lambda Z ("x") (
                Lambda Z ("y") (
                    Minus (Sym "x") (Sym "y")
                )
            )
        ),

        Define Z (Sym "x") (Apply (Apply (Sym "plus") (Val 42)) (Val 25)),
        Define Z (Sym "y") (Apply (Apply (Sym "minus") (Sym "x")) (Val 25)),
        Show "" (Sym "x"),
        Show "" (Sym "y")  
    ]

prog4 = -- partial evaluation
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z ("x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z Z)) (Sym "add") (
            Lambda Z ("x") (
                Lambda Z ("y") (
                    Minus (Sym "y") (Apply (Sym "chs") (Sym "x"))
                )
            )
        ),

        Define (Fun Z (Fun Z Z)) (Sym "sub") (
            Lambda Z ("x") (
                Lambda Z ("y") (
                    Minus (Sym "y") (Sym "x")
                )
            )
        ),

        Define Z (Sym "add25") (Apply (Sym "add") (Val 25)),
        Define Z (Sym "sub25") (Apply (Sym "sub") (Val 25)),

        Define Z (Sym "x") (Apply (Sym "add25") (Val 42)),
        Define Z (Sym "y") (Apply (Sym "sub25") (Sym "x")),
        Show "" (Sym "x"),
        Show "" (Sym "y")

        
    ]

prog5 = -- scope + partial application with non-local context
    [
        Define Z (Sym "x") (Val 42),

        Define (Fun Z (Fun Z Z)) (Sym "nesty") (
            Lambda Z ("a") (
                Lambda Z ("b") (
                    Minus (Sym "x") (Minus (Sym "a") (Sym "b"))
                )
            )
        ),

        Assign (Sym "x") (Apply (Apply (Sym "nesty") (Val 7)) (Val 32)),
        Define Z (Sym "x67") (Sym "x"),
        Show "x67 ==> " (Sym "x67"),
        Assign (Sym "x") (Apply (Apply (Sym "nesty") (Val 32)) (Val 7)),
        Define Z (Sym "x42") (Sym "x"),
        Show "x42 ==> " (Sym "x42"),
        Define (Fun Z Z) (Sym "m7") (Apply (Sym "nesty") (Val 7)),
        Assign (Sym "x") (Apply (Sym "m7") (Val 32)),
        Show "x ==> " (Sym "x")

        
    ]

prog6 = -- like prog5 with lazy expressions
    [
        Define Z (Sym "x") (Val 42),

        Define (Fun Z (Fun Z (Lazy Z))) (Sym "nesty") (
            Lambda Z ("a") (
                Lambda Z ("b") (
                    Defer (Minus (Sym "x") (Minus (Sym "a") (Sym "b")))
                )
            )
        ),

        Define (Lazy Z) (Sym "z67") (Apply (Apply (Sym "nesty") (Val 7)) (Val 32)),
        Assign (Sym "x") (Sym "z67"),

        Define (Lazy Z) (Sym "z42") (Apply (Apply (Sym "nesty") (Val 32)) (Val 7)),
        Assign (Sym "x") (Sym "z42"),

        Define (Fun Z (Lazy Z)) (Sym "n7") (Apply (Sym "nesty") (Val 7)),
        Assign (Sym "z67") (Apply (Sym "n7") (Val 32)),
        Assign (Sym "x") (Sym "z67")

        
    ]

prog7 =
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z ("x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z Z)) (Sym "plus") (
            Lambda Z ("x") (
                Lambda Z ("y") (
                    Minus (Sym "x") (Apply (Sym "chs") (Sym "y"))
                )
            )
        ),

        Define       Z  (Sym "x") (Val 19),
        Define       Z  (Sym "y") (Val 23),
        Define (Lazy Z) (Sym "z") (Defer ((Apply (Apply (Sym "plus") (Sym "x")) (Sym "y")))),
        Define       Z  (Sym "a") (Sym "z"),
        Assign          (Sym "x") (Apply (Apply (Sym "plus") (Sym "x")) (Val 11)),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 14)),
        Define       Z  (Sym "b") (Sym "z")

        
    ]

prog8 =
    [
        Define (Fun Z Z) (Sym "dec") (Lambda Z ("n") (Minus (Sym "n") (Val 1))),

        Define    Z      (Sym "x")   (Val 43),
        Define    Z      (Sym "y")          (Apply (Sym "dec") (Sym "x")),
        Define (Lazy Z)  (Sym "z")   (Defer (Apply (Sym "dec") (Sym "x"))),

        Define    Z      (Sym "ty0") (Sym "y"),
        Define    Z      (Sym "tz0") (Sym "z"),
        Assign           (Sym "x")   (Val 68),
        Define    Z      (Sym "ty1") (Sym "y"),
        Define    Z      (Sym "tz1") (Sym "z")

        
    ]

prog9 =
    [
        Define Z (Sym "x") (Val 67),
        Define Z (Sym "y") (Val 42),

        Define (Fun (Lazy Z) (Fun (Lazy Z) Z)) (Sym "minus") (
            Lambda (Lazy Z) ("x") (
                Lambda (Lazy Z) ("y") (
                    Minus (Sym "x") (Sym "y")
                )
            )
        ),

        Define Z (Sym "z") (
            Apply (Apply (Sym "minus") (
                Minus (Sym "x") (Sym "y")
            )) (
                Minus (Sym "y") (Sym "x")
            )
        ),
        Show "" (Sym "z")        
    ]

progA =
    [
    ]

progB =
    [
    ]

progC =
    [
    ]

progD =
    [
    ]

progE =
    [
    ]

progF =
    [
    ]

fibo =
    [
        Define (Fun Z Z) (Sym "chs") (
            Lambda Z ("x") (Minus (Val 0) (Sym "x"))
        ),

        Define (Fun Z (Fun Z Z)) (Sym "plus") (
            Lambda Z ("x") (
                Lambda Z ("y") (
                    Minus (Sym "x") (Apply (Sym "chs") (Sym "y"))
                )
            )
        ),

        Define (Fun Z Z) (Sym "fibo") (
            Lambda Z "n" (
                If (Less (Sym "n") (Val 2)) (Sym "n")
                    (Apply (Apply (Sym "plus")
                        (Apply (Sym "fibo") (Minus (Sym "n") (Val 1))))
                        (Apply (Sym "fibo") (Minus (Sym "n") (Val 2)))
                )
            )
        ),
        Define Z (Sym "z") $ Apply (Sym "fibo") (Val 3), 
        Show ""  $ Sym "z"
        
    ]
