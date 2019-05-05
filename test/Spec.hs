import Grammar.All
import Inference.Infer
import Prelude hiding (succ)

main :: IO ()
main = mapM_ print
    [ inferProp
    , inferSet
    , inferType
    , inferVar
    , inferProdProp1
    , inferProdProd2
    , inferProdProp3
    , inferProdSet1
    , inferProdSet2
    , inferProdType1
    , inferProdType2
    , inferAbs
    , inferApp
    , inferLet
    , inferZero
    , inferOne
    , inferTwo
    , inferNat
    , inferTrue
    ]

falseType    = Ind "False" Infty [] []
trueType     = Ind "True"  Infty [] []
trueTypeBare = Ind "True"  Bare [] []
natType      = Ind "Nat"   Infty [] []
natTypeBare  = Ind "Nat"   Bare [] []
listType a   = Ind "List"  Infty [Var a] []
vecType a n  = Ind "Vec"   Infty [Var a] [n]

zero   = Constr "O" [] []
succ n = Constr "S" [] [n]

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
    I "List" 0 (Prod "A" Set Set)
        [ Beta "Nil"  (Prod "A" Set (listType "A"))
        , Beta "Cons" (Prod "A" Set (Prod "a" (Var "A") (Prod "l" (listType "A") (listType "A"))))
        ],
    -- List[1]: Set -> Nat -> Set := {VNil: (A: Set) -> (n: Nat) -> Vec A n, VCons (A: Set) -> (n: Nat) -> A -> Vec A n -> Vec A (S n)}
    I "Vec" 0 (Prod "A" Set (Prod "n" natType Set))
        [ Beta "VNil"  (Prod "A" Set (vecType "A" zero))
        , Beta "VCons" (Prod "A" Set (Prod "n" natType (Prod "a" (Var "A") (Prod "v" (vecType "A" (Var "n")) (vecType "A" (succ (Var "n")))))))
        ]
    ]

emptyContext = []
natContext = [Beta "n" natType]

inferProp = infer initStageVars [] [] Prop
inferSet  = infer initStageVars [] [] Set
inferType = infer initStageVars [] [] (Type 2)
inferVar  = infer initStageVars indTypes natContext (Var "n")
inferProdProp1 = infer initStageVars indTypes [] (Prod "p" trueTypeBare trueTypeBare)   -- Prop, Prop
inferProdProd2 = infer initStageVars indTypes [] (Prod "p" natTypeBare  trueTypeBare)   -- Set,  Prop
inferProdProp3 = infer initStageVars indTypes [] (Prod "p" Set          trueTypeBare)   -- Type, Prop
inferProdSet1  = infer initStageVars indTypes [] (Prod "s" trueTypeBare natTypeBare)    -- Prop, Set
inferProdSet2  = infer initStageVars indTypes [] (Prod "s" natTypeBare  natTypeBare)    -- Set,  Set
inferProdType1 = infer initStageVars indTypes [] (Prod "t" Set          natTypeBare)    -- Type, Set
inferProdType2 = infer initStageVars indTypes [] (Prod "t" (Type 7)     (Type 3))       -- Type, Type
inferAbs  = infer initStageVars indTypes [] (Abs "x" natTypeBare (succ (Var "x")))
inferApp  = infer initStageVars indTypes [] (App (Abs "x" natTypeBare (succ (Var "x"))) zero)
inferLet  = infer initStageVars indTypes [] (Let "n" zero natTypeBare (succ (Var "n")))
inferZero = infer initStageVars indTypes [] zero
inferOne  = infer initStageVars indTypes [] (succ zero)
inferTwo  = infer initStageVars indTypes [] (succ (succ zero))
inferNat  = infer initStageVars indTypes [] natTypeBare
inferTrue = infer initStageVars indTypes [] trueTypeBare
