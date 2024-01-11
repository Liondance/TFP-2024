# TFP-2024

Repository for the TFP-2024 conference artifact.

This artifact embodies, in Haskell, our formalization of the Zilly language. Zilly is a small 
subset of Lilly - a language that generalizes the core spreadsheet semantics, as described in 
the ICICT 2021 paper titled "Towards Wide-Spectrum Spreadsheet Computing".

The ICICT 2021 paper was published  in the IEEE Xplore (https://ieeexplore.ieee.org/document/9476942)
and is reproduced here for convenience. The TFP 2024 paper is meant to be a step towards the formalization of Lilly.

# List of Content

- `README.md`: (this file) instructions and examples for how to run the artifacts
- `ZTest1.hs`: basic test driver - contains tests programs in `ADT` notation
- `ZTest2.hs`: advanced test driver - includes a parser for running test files
- `ZTest3.hs`: Monadic Interpreter test driver - contains tests programs in `ADTM` notation.
- `Zilly.hs`: AST definitions and interpreter for Zilly.
- `ZillyM.hs`: Monadic untyped interpreter for Zilly.
- `Pretty.hs`: A pretty printer for the pure interpreter.
- `ICICT-2021`: paper that informally describes how Lilly covers the core spreadsheet semantics
- `ADT`: directory with ADT libraries for the pure interpreter
- `ADT/Map`: a basic `Map` ADT (key-value dictionary) derived from Haskell's `Data.Map`
- `ADT/Stack`: a basic implementation of a `Stack` ADT
- `ADT/ADTM`: The `ADT` for the monadic untyped interpreter.
- `ADT/Types`: Miscelaneous types mainly used for pretty printing.
- `Control/ZM`: The definition of the Monadic Interpreter Main monad: `ZM`.
- `Control/IE`: Utilities and Exceptions for the Monadic Interpreter
- `Test`: directory with test files (Zilly programs in a sugared syntax) ran by `ZTest2`


# Installation Instructions

The project here was coded using GHC version `9.4.7`. For better reproducibility assurance, you may want 
to install the same version, using [GHCup](https://www.haskell.org/ghcup/install/) or a similar utility.

To run the advanced test driver `ZTest2.hs`, the Haskell libraries `parsec` and `HUnit` must also be installed:

```
$ cabal install --lib containers
$ cabal install --lib parsec
$ cabal install --lib HUnit
```

# Basic test driver: ZTest1.hs

Here is a sample session showing how to run test programs 6 and 7 residing in the basic test driver.
The same technique applies, mutatis mutandis, for running the other programs in `ZTest1.hs`.

``` haskell
ghci ZTest1.hs

ZTest1> state = run prog6
ZTest1> state
ZTest1> cv state "x" -- abbreviated form for `cvalue state (Sym "x')` - computes the c-value of "x"
ZTest1> cv state "y"
ZTest1> cv state "z"
ZTest1> rv state "x" -- abbreviated form for `rvalue state (Sym "x')` - computes the r-value of "x" 
ZTest1> rv state "y"
ZTest1> rv state "z"

ZTest1> state = run prog7
ZTest1> state
ZTest1> rv state "sub"
ZTest1> rv state "dec"
ZTest1> rv state "x67"
ZTest1> rv state "x42"
ZTest1> rvalue state (Sym "sub")
ZTest1> rvalue state (Apply (Sym "sub") (Val 25))
ZTest1> rvalue state (Apply (Apply (Sym "sub") (Val 25)) (Val 67))
```
# Advanced test driver: ZTest2.hs

The advanced test driver is able to parse and run Zilly programs residing in text files. The notation
used in those text files is a sugared and more friendly version of the ADT construction notation used 
in `ZTest1`. The correspondence between the ADT notation and the sugared one is pretty obvious. The 
latter is much closer to the actual syntax currently used in the Lilly.

A sample session running programs 6 and 7 is shown below.

```haskell
ghci ZTest2.hs
ZTest2> state6 <- run "Test/t6.zilly"
ZTest2> state7 <- run "Test/t7.zilly"
```

The file [Test/tAll.zilly](./Test/tAll.zilly) contains the full suite of test programs. It can be
used to get a handle of the syntax and define your own test programs, if so desired.

# Monadic Interpreter: ZTest3.hs

We also provide a monadic untyped interpreter for Zilly, that almost mimics our previous pure interpreter, with 2 addons: a `While` block, and a `Branch` block. A usage example of these two new instructions can be seen in `ZTest3.fiboIter`

A sample session running progams 1 and 10 is shown below:

```haskell
ZTest3> state1  <- run prog1
ZTest3> state10 <- run prog10
```

## Advantages of the monadic Interpreter

If we take a look at the definition of our main monad:

```haskell
newtype ZM s a = ZM { runZillyM :: s -> IO a }
type Env = Map String (MVar Dynamic)
type ZME = ZM Env -- Zilly Monad Environment
```

We can see that `ZM s a ~ ReaderT s IO a`, and thus `ZM s` is:

- A Functor (which yields 6 combinators from `Data.Functor`)
- An Applicative (8 combinators from `Control.Applicative`)
- A Monad (28 combinators from `Control.Monad`)
- An instance of MonadReader (4 combinators from the transformers library `Control.Monad.Trans.Reader` )
- An instance of MonadIO (1 combinator from `Control.Monad.IO.Class`)
- An instance of Monad Throw (1 combinator from the exceptions library `Control.Monad.Throw`)
- An instance of Monad Catch (8 combinators from the exceptions library `Control.Monad.Catch`)

Which yields a total of 56 combinators! That's 56 different functions that we are able to freely use out of the box. Giving us an ample instruction set to start developing.

But more importantly, looking at our main monad, we can instantly see some of the effects that we are going to use throughout our interpreter:

- The Monad instance for the ReaderT datatype, not only hints that we are going to be reading from an environment (from variables to values), but also that we are going to use its `local` capabilities to implement static bindings.
- The MonadThrow/Catch instance for `IO`, allows us to halt the program and throw (fast) custom exceptions while type checking.
- `IO` + having mutable variables inside the environment, provides a clean way of modeling reactivity, without having to disrupt any static bindings (closures) already defined.
- `IO` also allows us to output results to the stdout, providing a way of `Show`ing intermediate results.

Thus, by selecting the right building blocks (monad transformer), we got half of our work cut out for us, since we can simply rely on the semantics of each transformer layer to directly model our logic.

## Improvements to be made

- **Typing**: currently, types are only checked at runtime. Moreover, type information is never saved. Meaning that we are inspecting the type of a term as many times as it appears in the code, which is wasteful. Currently, we are still figuring out how to migrate from the unitype ADT defined in `ADT.ADTM`, to a more sleek/restrained one, by using `GADTs` or a Tagless Final Encoding approach while also keeping the type applications to a minimum.
- `IO` **is way too general**: For the time being, we are using 3 "subsets" of it: mutable variable handling, (interpreter) exception handling, and logging. Thus, it would be desireable to have these smaller effects newtyped for two reasons: It would make our intentions clearer, and the code a bit cleaner (by removing the `liftIO` that are all over the place)
- **Expressions cannot mutate state nor display text**: this means that the expression interpreter should not have access to the `mutate` variable effect, nor the `logger` effect, unlike the statement interpreter which needs both of these effects. This implies that in the future, we are going to need a monad for each interpreter, and a  way to `hoist`, `embed` or `lift` these constructs.
- `Dynamic` **values are not currently used**: We decided to model variables as `Dynamic` for future proofing, since we had the suspicion that if we were to use `GADTs`, we might encounter a problem while inserting them into a container (i.e: we cannot have `E (a -> b)` and `E Int` living in the same container).

