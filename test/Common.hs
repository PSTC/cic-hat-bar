module Common where

import Grammar.All
import Prelude hiding (succ)

falseType    = Ind "False" Infty [] []
trueType     = Ind "True"  Infty [] []
trueTypeBare = Ind "True"  Bare  [] []
natType      = Ind "Nat"   Infty [] []
natTypeBare  = Ind "Nat"   Bare  [] []
natTypePos   = Ind "Nat"   Position [] []
natTypeEps   = Ind "Nat"   Epsilon  [] []
listType a   = Ind "List"  Infty [Var a] []
finType k    = Ind "Fin"   Infty []      [k]
vecType a n  = Ind "Vec"   Infty [Var a] [n]

zero           = Constr "O"     []  []
succ  n        = Constr "S"     []  [n]
nil   a        = Constr "Nil"   [a] []
cons  a l ls   = Constr "Cons"  [a] [l, ls]
vnil  a        = Constr "VNil"  [a] []
vcons a n v vs = Constr "VCons" [a] [n, v, vs]

indTypes :: IndTypes
indTypes = [
    -- False[0]: Prop := {}
    I "False" 0 Prop [],
    -- True[0]: Prop := {I: True}
    I "True" 0 Prop
        [ Beta "I" trueType ],
    -- Nat[0]: Set := {O: Nat, S: Nat -> Nat}
    I "Nat" 0 Set 
        [ Beta "O" natType
        , Beta "S" (Prod "n" natType natType)
        ],
    -- List[1]: Set -> Set := {Nil: (A: Set) -> List A, Cons: (A: Set) -> A -> List A -> List A}
    I "List" 1 (Prod "A" Set Set)
        [ Beta "Nil"  (Prod "A" Set (listType "A"))
        , Beta "Cons" (Prod "A" Set (Prod "a" (Var "A") (Prod "l" (listType "A") (listType "A"))))
        ],
    -- Fin[0]: Nat -> Set := {FZ: (k: Nat) -> Fin (S k), FS: (k: Nat) -> Fin k -> Fin (S k)}
    I "Fin" 0 (Prod "k" natType Set)
        [ Beta "FZ" (Prod "k" natType (finType (succ (Var "k"))))
        , Beta "FS" (Prod "k" natType (Prod "f" (finType (Var "k")) (finType (succ (Var "k")))))
        ],
    -- Vec[1]: Set -> Nat -> Set := {VNil: (A: Set) -> (n: Nat) -> Vec A n, VCons (A: Set) -> (n: Nat) -> A -> Vec A n -> Vec A (S n)}
    I "Vec" 1 (Prod "A" Set (Prod "n" natType Set))
        [ Beta "VNil"  (Prod "A" Set (vecType "A" zero))
        , Beta "VCons" (Prod "A" Set (Prod "n" natType (Prod "a" (Var "A") (Prod "v" (vecType "A" (Var "n")) (vecType "A" (succ (Var "n")))))))
        ]
    ]

plus :: Term Bare
plus =
    Fix 1 "plus" (Prod "n" natTypePos (Prod "m" natTypeEps natTypeEps))
        (Abs "n" natTypeBare
            (Abs "m" natTypeBare
                (Case (Var "n") natTypeBare
                    [ (Var "m")
                    , (Abs "x" natTypeBare
                        (succ (App (App (Var "plus") (Var "x")) (Var "m"))))
                    ])))
