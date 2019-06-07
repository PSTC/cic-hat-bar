module Grammar.StagesTest where

import Grammar.Stages
import Test.Hspec

test = do
    describe "Stages" $ do
        describe "nextStage" $ do
            it "returns the next available stage variable" $ do
                nextStage initStageVars `shouldBe` S "0"
        describe "incStageVars" $ do
            it "increments the stage variable" $ do
                let s1 = incStageVars initStageVars
                nextStage s1 `shouldBe` S "1"