# TFP-2024

## Artifact

### Daniel

    1. Build a second Zilly interpreter ... one that works well! ;-)
    2. Borrador de por que el monadico es buena opcion
    3. actualizar el repo
    4. actualizar los casos de prueba de ZTest2 en main
    5. Update Zilly and ZillyM syntax so that both can match again.
    6. Add a new type `DiagramText a ~ Tree a` to Types and make `mapClosureEnv` a top declaration.

### Enzo

    1. Make the Zilly interpreter work well! ;-)

    2. Test variations of

        f := a ->
                b ->
                    a - b

        g := f(67);

        x := g(25);

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


