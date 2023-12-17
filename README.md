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
- `Zilly.hs`: AST definitions and interpreter for Zilly
- `ICICT-2021`: paper that informally describes how Lilly covers the core spreadsheet semantics
- `ADT`: directory with ADT libraries
- `ADT/Map`: a basic `Map` ADT (key-value dictionary) derived from Haskell's `Data.Map`
- `ADT/Stack`: a basic implementation of a `Stack` ADT
- `Test`: directory with test files (Zilly programs in a sugared syntax) ran by `ZTest2`
 
# Installation Instructions

The project here was coded using GHC version `9.4.7`. For better reproducibility assurance, you may want 
to install the same version, using [GHCup](https://www.haskell.org/ghcup/install/) or a similar utility.

To run the advanced test driver `ZTest2.hs`, the Haskell libraries `parsec` and `HUnit` must also be installed:

```
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
