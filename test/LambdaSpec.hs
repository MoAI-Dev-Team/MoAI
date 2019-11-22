module LambdaSpec (spec) where

import Test.Hspec
import Lambda

spec :: Spec
spec = describe "Lambda.reduction" $ do
  context "reduct (λx.x) y" $ do
    it "should return y" $ do
      (reduction $ Abstract "x" (Variable "x") `Apply` Variable "y") `shouldBe` Variable "y"
