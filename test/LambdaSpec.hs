module LambdaSpec (spec) where

import Test.Hspec
import Lambda

spec :: Spec
spec = describe "Lambda.reduction" $ do
  let x = Variable "x"
      y = Variable "y"
      xxy = Abstract "x" (Variable "x") `Apply` Variable "y"
  
  context "reduct (λx.x) y" $ do
    it "should return y" $ do
      reduction xxy `shouldBe` y
  
  context "reduct x ((λx.x) y)" $ do
    it "should return x y" $ do
      reduction (x `Apply` xxy) `shouldBe` (x `Apply` y)
  
  context "reduct (λx.x) y ((λx.x) y)" $ do
    it "should return y ((λx.x) y)" $ do
      reduction (xxy `Apply` xxy) `shouldBe` (y `Apply` xxy)
