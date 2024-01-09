module ZTest3 where

import ZillyM
import ADT.ADT


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

        Show "cv(z) ==> " (Formula (Sym "z")),

        Show "rv(z) ==> " (Apply (Sym "rv") (Sym "z")),

        Show "id(z) ==> " (Apply (Sym "id") (Sym "z")),

        Show "id(cv(z)) ==> " (Apply (Sym "id") (Formula (Sym "z"))), 

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
        
        Show "x ==> " (Sym "x"),
        Show "y ==> " (Sym "y"),
        Show "z ==> " (Sym "z"),
        Show "a ==> " (Sym "a"),

        Assign          (Sym "x") (Apply (Apply (Sym "plus") (Sym "x")) (Val 11)),
        Assign          (Sym "y") (Apply (Apply (Sym "plus") (Sym "y")) (Val 14)),
        Define       Z  (Sym "b") (Sym "z"),
        
        Show "x ==> " (Sym "x"),
        Show "y ==> " (Sym "y"),
        Show "z ==> " (Sym "z"),
        Show "a ==> " (Sym "a"),
        Show "b ==> " (Sym "b")
        

        
    ]

prog8 =
    [
        Define (Fun Z Z) (Sym "dec") (Lambda Z ("n") (Minus (Sym "n") (Val 1))),

        Define    Z      (Sym "x")   (Val 43),
        Define    Z      (Sym "y")          (Apply (Sym "dec") (Sym "x")),
        Define (Lazy Z)  (Sym "z")   (Defer (Apply (Sym "dec") (Sym "x"))),

        Show "x ==> " (Sym "x"),
        Show "y ==> " (Sym "y"),
        Show "z ==> " (Sym "z"),

        Define    Z      (Sym "ty0") (Sym "y"),
        Define    Z      (Sym "tz0") (Sym "z"),
        Assign           (Sym "x")   (Val 68),
        Define    Z      (Sym "ty1") (Sym "y"),
        Define    Z      (Sym "tz1") (Sym "z"),

        Show "ty0 ==> " (Sym "ty0"),
        Show "tz0 ==> " (Sym "tz0"),
        Show "x ==> " (Sym "x"),
        Show "ty1 ==> " (Sym "ty1"),
        Show "tz1 ==> " (Sym "tz1")
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

prog10 = 
  [ Define Z (Sym "x") (Val 42)
  , Define (Fun Z Z) (Sym "chs") 
    ( Lambda Z ("x") (Minus (Val 0) (Sym "x")))
  , Define (Fun Z (Fun Z Z)) (Sym "plus") (
    Lambda Z ("x") (
      Lambda Z ("y") (
        Minus (Sym "x") (Apply (Sym "chs") (Sym "y"))
        )
      )
    )
  , Define (Lazy . Lazy $ Z) (Sym "z") $ Defer (Defer (Formula $ Sym "x"))
  , Define (Z `Fun` Z) (Sym "rv")
    $ Lambda Z "x" $ Sym "x"
  , Define (Z `Fun` Z) (Sym "f") 
    $ Lambda Z "x" $ Lambda (Lazy Z) "y" $ Sym "rv" `Apply` Sym "y"
  , Show "f(99)(z) ==> " $ Sym "f" `Apply` Val 99 `Apply` Sym "z"
  ]


prog11 = 
  [ Define Z (Sym "x") (Val 1)
  , Define Z (Sym "y") (Val 43)
  , Define (Z `Fun` Z) (Sym "f") $ 
    Lambda Z "x" $ Lambda Z "y" $ Formula $ Sym "x" `Minus` Sym "y"
  , Define (Z `Fun` Z) (Sym "rv")
    $ Lambda Z "x" $ Sym "x"
  , Show "f(99)(100) ==> " (Sym "f" `Apply` Val 99 `Apply` Val 100)
  , Show "rv(f(99)(100)) ==> " $ Sym "rv" `Apply` (Sym "f" `Apply` Val 99 `Apply` Val 100)
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

fiboIter =
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

        Define Z (Sym "n2") (Val 0),
        Define Z (Sym "n1") (Val 1),
        Define Z (Sym "cont") (Val 10),

        Branch (Val 2 `Less` Sym "cont")
          [
            While (Sym "cont")
              [ Show "n1 ==> " (Sym "n2"),
                Define Z (Sym "aux") (Sym "n2"),
                Assign (Sym "n2") (Sym "n1"),
                Assign (Sym "n1") (Sym "plus" `Apply` Sym "n1" `Apply` Sym "aux"),
                Assign (Sym "cont") (Sym "cont" `Minus` Val 1)
              ]
          ]
          [
            Show "cont ==> " (Sym "cont")
          ]
        ,

        Show "n1 ==> " (Sym "n1")
        
    ]
