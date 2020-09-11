module LambdaIndexSpec (spec) where

import Test.Hspec
import LambdaIndex

spec :: Spec
spec = describe "Lambda.reduce" $ do
    let x = Variable 0
        y = Variable 1
        xxy = Abstract 0 (Variable 0) `Apply` Variable 1
  
    context "reduce (λx.x) y once" $ do
        it "should return y" $ do
            reduce xxy `shouldBe` y
  
    context "reduce x ((λx.x) y) once" $ do
        it "should return x y" $ do
            reduce (x `Apply` xxy) `shouldBe` (x `Apply` y)
  
    context "reduce (λx.x) y ((λx.x) y) once" $ do
        it "should return y ((λx.x) y)" $ do
            reduce (xxy `Apply` xxy) `shouldBe` (y `Apply` xxy)
      
    context "reduce (λx.x) y ((λx.x) y) to normal form" $ do
        it "should return y y" $ do
            reduceAll (xxy `Apply` xxy) `shouldBe` (y `Apply` y)
