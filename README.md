# CIC^_

A Haskell implementation of a sized type inference algorithm for CIC^_ [1], which is CIC^ [2] with simple types, which is CIC [3] with sized types, which is the dependent type theory for Coq. It isn't specified how CIC^_ should be pronounced (or written in ASCII), so I'm calling it CIC-hat-bar, because CIC-caret-underscore is far too long a name.

The algorithm itself is based directly on the one specified in CIC^, which in turn references an algorithm in F^ [4]. Changes will need to be made to the algorithm to restrict the language to simple types.

This project uses Stack; run `stack install` to install and `stack build` to build an executable that doesn't do anything. Run `stack test` to print out some example inferences. The terms will print out as fancy Unicode characters; if you're on Windows, you'll need to run `chcp.com 65001` first (see [here](https://stackoverflow.com/q/25373116/9270195)).

## TODOs
* Contexts:
    - isValid: To check the validity of an inductive definition
    - getFreeVariable: To produce a free variable given a context
* Infer:
    - infer: Inference for `Case` and `Fix` terms
    - recCheck: To ensure soundness and completeness of stage constraints
* Defs:
    - typePred: Returns the type of a `Case` expression
    - typeBranch: Returns the type of a branch in a `Case` expression
* Auxil:
    - whnf: Computes the weak head normal form of an expression
    - (⪯): Computes a stage constraint given a subtyping relation
    - shift: Shifts up stage annotations of types in recursive positions
* Spec:
    - Use Hspec!

## References
[1] [On type-based termination and dependent pattern matching in the calculus of inductive constructions](https://pastel.archives-ouvertes.fr/pastel-00622429) by Jorge Luis Sacchini.
[2] [CIC^: Type-based termination of recursive definitions in the calculus of inductive constructions](https://link.springer.com/chapter/10.1007/11916277_18) by Gilles Barthe, Benjamin Grégoire, and Fernando Pastawski.
[3] [Calculus of Inductive Constructions](https://coq.inria.fr/distrib/current/refman/language/cic.html), from the Coq Reference Manual.
[4] [Practical inference for type-based termination in a polymorphic setting](https://link.springer.com/chapter/10.1007/11417170_7) by Gilles Barthe, Benjamin Grégoire, and Fernando Pastawski.
