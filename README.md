# CIC^_

A partial Haskell implementation of a sized type inference algorithm for CIC^_ [\[1\]](#f1), which is CIC^ [\[2\]](#f2) with simple types, which is CIC [\[3\]](#f3) with sized types, which is the dependent type theory for Coq. It isn't specified how CIC^_ should be pronounced (or written in ASCII), so I'm calling it CIC-hat-bar.

The algorithm itself is based directly on the one specified in CIC^, which in turn references an algorithm in F^ [\[4\]](#f4). Changes will need to be made to the algorithm to restrict the language to simple types.

This project uses Stack; run `stack install` to install and `stack build` to build an executable that doesn't do anything. Run `stack test` to run some tests (incomplete); use `mainOld` in `Spec.hs` to print out some example inferences. The terms will print out as fancy Unicode characters; if you're on Windows, you'll need to run `chcp.com 65001` first (see [here](https://stackoverflow.com/q/25373116/9270195)).

## File structure
```
src/
    Grammar/
        All.hs: Convenience module for importing/exporting all Grammar modules
        Terms.hs: AST for the terms of CIC^_
        Stages.hs: All the different annotations a type could have
        Contexts.hs: Local and global contexts for bindings and inductive definitions
    Inference/
        Infer.hs: The main inference algorithms `infer`, `check`, `recCheck` (see CIC^, Fig. 3 and F^, Sec. 3.4)
        Defs.hs: Definitions over inductive constructions (see CIC^, Fig. 2)
        Auxil.hs: Auxiliary functions for the algorithms (see CIC^, Sec. 4 Specifications and Def. 12)
test/
    Common.hs: Terms and contexts used throughout tests
    Spec.hs: Entry point for running tests
    Inference/: Contains tests for modules in Inference
app/
    Main.hs: nothing
```

## Module dependency structure
```
Grammar.Terms       -> Grammar.Stages
Grammar.Contexts    -> Grammar.{Terms, Stages}
Grammar.All         -> Grammar.*

Inference.Defs      -> Grammar.All
Inference.Auxil     -> Grammar.All
Inference.Infer     -> Grammar.All, Inference.{Defs, Auxil}

Common              -> Grammar.All
Inference.<Mod>Test -> Common, Inference.<Mod>, same dependencies as Inference.<Mod>
Spec.hs             -> Grammar.All, Inference.*Test, and Common, Inference.Infer (for now)
```

## TODOs
* Contexts:
    - `isValid`: To check the validity of an inductive definition
    - `getFreeVariable`: To produce a free variable given a context and terms
* Infer:
    - `infer`: Inference for `Fix` terms
    - `recCheck`: To ensure soundness and completeness of stage constraints
* Auxil:
    - `whnf`: Computes the weak head normal form of an expression
    - `(⪯)`: Computes stage constraints given a subtyping relation
    - `shift`: Shifts up stage annotations of types in recursive positions
* Spec:
    - Write tests inference functions and helpers

## References
<span id="f1">[1]</span> [On type-based termination and dependent pattern matching in the calculus of inductive constructions](https://pastel.archives-ouvertes.fr/pastel-00622429) by Jorge Luis Sacchini. Referred to as CIC^_.

<span id="f2">[2]</span> [CIC^: Type-based termination of recursive definitions in the calculus of inductive constructions](https://link.springer.com/chapter/10.1007/11916277_18) by Gilles Barthe, Benjamin Grégoire, and Fernando Pastawski. Referred to as CIC^.

<span id="f3">[3]</span> [Calculus of Inductive Constructions](https://coq.inria.fr/distrib/current/refman/language/cic.html), from the Coq Reference Manual. Referred to as the Manual.

<span id="f4">[4]</span> [Practical inference for type-based termination in a polymorphic setting](https://link.springer.com/chapter/10.1007/11417170_7) by Gilles Barthe, Benjamin Grégoire, and Fernando Pastawski. Referred to as F^.
