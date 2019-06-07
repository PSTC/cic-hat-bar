{-#LANGUAGE RankNTypes #-}

module Grammar.TermsTest where

import Grammar.Terms
import Grammar.Stages
import Common
import Test.Hspec

test = do
    describe "Terms" $ do
        describe "isSort" $ do
            it "returns true for sorts" $ do
                (Prop    :: Term Bare) `shouldSatisfy` isSort
                (Set     :: Term Bare) `shouldSatisfy` isSort
                (Type 1  :: Term Bare) `shouldSatisfy` isSort
                (Type 2  :: Term Bare) `shouldSatisfy` isSort
                (Type 42 :: Term Bare) `shouldSatisfy` isSort
            it "returns false for non-sort terms" $ do
                (Var "x"              :: Term Bare) `shouldNotSatisfy` isSort
                (Abs "x" Set Set      :: Term Bare) `shouldNotSatisfy` isSort
                (App Set Set          :: Term Bare) `shouldNotSatisfy` isSort
                (Let "x" Set Set Set  :: Term Bare) `shouldNotSatisfy` isSort
                (Prod "x" Set Set     :: Term Bare) `shouldNotSatisfy` isSort
                (Ind "Nat" Bare [] [] :: Term Bare) `shouldNotSatisfy` isSort
                (Constr "O" [] []     :: Term Bare) `shouldNotSatisfy` isSort
                (Case Set (Type 1) [] :: Term Bare) `shouldNotSatisfy` isSort
                (Fix 1 "f" Set Set    :: Term Bare) `shouldNotSatisfy` isSort
                plus `shouldNotSatisfy` isSort
        describe "bind" $ do
            it "binds free variables" $ do
                let ex = Var "x" :: forall a. Term a
                    ey = Var "y" :: forall a. Term a
                bind ex "x" ey `shouldBe` ey
                bind ex "z" ey `shouldBe` ex
                bind (App ex ex)        "x" ey `shouldBe` App ey ey
                bind (Abs  "x" ex ex)   "x" ey `shouldBe` Abs  "x" ey ex
                bind (Abs  "z" ex ex)   "x" ey `shouldBe` Abs  "z" ey ey
                bind (Prod "x" ex ex)   "x" ey `shouldBe` Prod "x" ey ex
                bind (Prod "z" ex ex)   "x" ey `shouldBe` Prod "z" ey ey
                bind (Let "x" ex ex ex) "x" ey `shouldBe` Let "x" ey ey ex
                bind (Let "z" ex ex ex) "x" ey `shouldBe` Let "z" ey ey ey
                bind (Ind "Nat" Infty [ex] [ex]) "x" ey `shouldBe` Ind "Nat" Infty [ey] [ey]
                bind (Constr "O" [ex] [ex])      "x" ey `shouldBe` Constr "O" [ey] [ey]
                bind (Case ex ex [ex])           "x" ey `shouldBe` Case ey ey [ey]
                bind (Fix 1 "x" (Prod "y" ex ex) ex)  "x" ey `shouldBe` Fix 1 "x" (Prod "y" ey ey) ex
                bind (Fix 1 "f" (Prod "y" ex ex) ex)  "x" ey `shouldBe` Fix 1 "f" (Prod "y" ey ey) ey
        describe "apply" $ do
            let f = (Var "f") :: Term Bare
                x = (Var "x") :: Term Bare
                y = (Var "y") :: Term Bare
            it "applies terms" $ do
                apply f [] []     `shouldBe` f
                apply f [x] []    `shouldBe` (App f x)
                apply f [] [y]    `shouldBe` (App f y)
                apply f [x] [y]   `shouldBe` (App (App f x) y)
                apply f [x, y] [] `shouldBe` (App (App f x) y)
                apply f [] [x, y] `shouldBe` (App (App f x) y)
            it "undoes effects of unapply" $ do
                let (f', xs, ys) = unapply (App (App f x) y) 1
                apply f' xs ys `shouldBe` (App (App f x) y)
        describe "unapply" $ do
            let f = (Var "f") :: Term Bare
                x = (Var "x") :: Term Bare
                y = (Var "y") :: Term Bare
                e = (App (App f x) y)
            it "unapplies an application" $ do
                unapply e 0 `shouldBe` (f, [x, y], [])
                unapply e 1 `shouldBe` (f, [x], [y])
                unapply e 2 `shouldBe` (f, [], [x, y])
            it "undoes effects of apply" $ do
                let e' = apply f [x] [y]
                unapply e' 1 `shouldBe` (f, [x], [y])
