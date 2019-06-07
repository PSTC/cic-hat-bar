import Grammar.All
import Inference.Infer

import Common
import Grammar.ContextsTest as Contexts
import Grammar.StagesTest   as Stages
import Grammar.TermsTest    as Terms
import Inference.AuxilTest  as Auxil
import Inference.DefsTest   as Defs
import Inference.InferTest  as Infer

import Prelude hiding (succ)
import Test.Hspec

main :: IO ()
main = hspec $ do
    Stages.test
    Terms.test
    Contexts.test
    Defs.test
    Auxil.test
    Infer.test

mainOld = mapM_ print
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
