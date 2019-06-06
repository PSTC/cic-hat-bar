# CIC^_

A Haskell implementation of a sized type inference algorithm for CIC^_ [\[1\]](#f1), which is CIC^ [\[2\]](#f2) with simple types, which is CIC [\[3\]](#f3) with sized types, which is the dependent type theory for Coq. It isn't specified how CIC^_ should be pronounced (or written in ASCII), so I'm calling it CIC-hat-bar, because CIC-caret-underscore is far too long a name.

The algorithm itself is based directly on the one specified in CIC^, which in turn references an algorithm in F^ [\[4\]](#f4). Changes will need to be made to the algorithm to restrict the language to simple types.

This project uses Stack; run `stack install` to install and `stack build` to build an executable that doesn't do anything. Run `stack test` to print out some example inferences. The terms will print out as fancy Unicode characters; if you're on Windows, you'll need to run `chcp.com 65001` first (see [here](https://stackoverflow.com/q/25373116/9270195)).

## Structure
```
src/
    Grammar/
        Terms.hs: AST for the terms of CIC^_
        Stages.hs: All the different annotations a type could have
        Contexts.hs: Local and global contexts for bindings and inductive definitions
    Inference/
        Infer.hs: The main inference algorithms `infer`, `check`, `recCheck`
        Defs.hs: Definitions over inductive constructions
        Auxil.hs: Auxiliary functions for the algorithms
test/
    Spec.hs: Tests
app/
    Main.hs: nothing
```

## TODOs
* Contexts:
    - `isValid`: To check the validity of an inductive definition
    - `getFreeVariable`: To produce a free variable given a context
* Infer:
    - `infer`: Inference for `Case` and `Fix` terms
    - `recCheck`: To ensure soundness and completeness of stage constraints
* Defs:
    - `typeBranch`: Returns the type of a branch in a `Case` expression
* Auxil:
    - `whnf`: Computes the weak head normal form of an expression
    - `(⪯)`: Computes stage constraints given a subtyping relation
    - `shift`: Shifts up stage annotations of types in recursive positions
* Spec:
    - Use Hspec!

## References
<span id="f1">[1]</span> [On type-based termination and dependent pattern matching in the calculus of inductive constructions](https://pastel.archives-ouvertes.fr/pastel-00622429) by Jorge Luis Sacchini.

<span id="f2">[2]</span> [CIC^: Type-based termination of recursive definitions in the calculus of inductive constructions](https://link.springer.com/chapter/10.1007/11916277_18) by Gilles Barthe, Benjamin Grégoire, and Fernando Pastawski.

<span id="f3">[3]</span> [Calculus of Inductive Constructions](https://coq.inria.fr/distrib/current/refman/language/cic.html), from the Coq Reference Manual.

<span id="f4">[4]</span> [Practical inference for type-based termination in a polymorphic setting](https://link.springer.com/chapter/10.1007/11417170_7) by Gilles Barthe, Benjamin Grégoire, and Fernando Pastawski.
