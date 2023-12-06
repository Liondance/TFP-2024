# TFP-2024

## Artifact

### Instructions
load ztest1 in the ghc REPL.
Here is a sample session running test programs 6 and 7.

    ghci ztest1

    Zilly'Test> state = run prog6
    Zilly'Test> state
    Zilly'Test> cv state "x" 
    Zilly'Test> cv state "y"
    Zilly'Test> cv state "z"
    Zilly'Test> rv state "x" 
    Zilly'Test> rv state "y"
    Zilly'Test> rv state "z"

    Zilly'Test> state = run prog7
    Zilly'Test> state
    Zilly'Test> rv state "sub"
    Zilly'Test> rv state "dec"
    Zilly'Test> rv state "x67"
    Zilly'Test> rv state "x42"
    Zilly'Test> rvalue state (Sym "sub")
    Zilly'Test> rvalue state (Apply (Sym "sub") (Val 25))
    Zilly'Test> rvalue state (Apply (Apply (Sym "sub") (Val 25)) (Val 67))

### Daniel

    1. Make your code compile (it does not for me) with no unknown dependencies **PENDING FOR TESTING**
    2. Your test program (ztest2) should import the interpreter (zilly) **DONE**
    3. Eliminate/rename zdani1/zdani2: there should be only one ztest2  **DONE**
    4. Judges will test running *ghci ztest1* or *ghci ztest2*, at their choice **DONE**
    5. After you make this work cleanly, we will look into sample tests
    6. Please factor out Map code (zilly.hs: lines 12-27) as an ADT.Map module **DONE**
    7. ¡Animo! If I don't mess up we will qualify for post-symposium review

### Cody's suggestions

    YES: use a state transformer!! (without do notation)
    create a monad type
    exec state action ==> State -> Maybe a
    ExecEnv ()
    ST: ExecEnv a = State -> Maybe (a, State)
    run' : [Act] -> ExecEnv a

    ExecEnv a = State -> Maybe a
    ()
    ExecEnv ()

    ExecEnv a = State -> Maybe (a, State)
    run' : [Act] -> ExecEnv a
    rvalue : Exp -> ExecEnv Value
