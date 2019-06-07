module Inference.InferTest where

import Inference.Infer
import Test.Hspec

test = do
    it "" $ do True `shouldBe` True