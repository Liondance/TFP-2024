# TFP-2024

Repository for the TFP-2024 conference, which holds detailed instrucions and examples on how to run each artifact.

# Installation instructions

The following project was coded using GHC version `9.4.7`. For identical results, please install this version of the compiler using [GHCup](https://www.haskell.org/ghcup/install/) or a similar utility.

In order to run the complementary artifact `ZTest2.hs`, please install the haskell libraries `parsec` and `HUnit` the following way:

```
$ cabal install --lib parsec
$ cabal install --lib HUnit
```

# Main artifact: ZTest1.hs

In order to test the main artifact, please follow the following example session:

``` haskell
ghci ZTest1.hs

ZTest1> state = run prog6
ZTest1> state
ZTest1> cv state "x" 
ZTest1> cv state "y"
ZTest1> cv state "z"
ZTest1> rv state "x" 
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
# Complementary artifact: ZTest2.hs

The complementary artifact uses plain text files in order to run. Please, see the [Test/t7.zilly](./Test/t7.zilly) file in order to get a handle on the grammar that Zilly employs.

Once the file is created, please follow the following example session:

```haskell
ghci ZTest2.hs
ZTest2> state6 <- run "Test/t6.zilly"
ZTest2> state7 <- run "Test/t7.zilly"

```

# Repository structure

Currently, the repository is structured as follows:

- `Zilly.hs`: Holds the interpreter and definition of Zilly. Plus a small deserializer for pretty printing which is just complementary.
- `ADT/Map`: Holds a re-export of the mapping operations, with some minor modifications.
- `ADT/Stack`: Holds an implementation of the `Stack` data structure.
- `ZTest1.hs`: Main testing artifact, which holds some tests programs in `ADT` notation.
- `ZTest2.hs`: Complementary artifact. Holds a parser for the `Zilly` language, plus a lot of unit tests!.






