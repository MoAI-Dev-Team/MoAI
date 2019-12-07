module SKISpec (spec) where

import Test.Hspec
import SKI

spec :: Spec
spec = describe "SKI.reduction" $ do
  context "reduct I I" $ do
    it "sould return I" $ do
      reduction (I `Apply` I) `shouldBe` I
      
  context "reduct K S I" $ do
    it "should return S" $ do
      reduction (K `Apply` S `Apply` I) `shouldBe` S
      
  context "reduct S S K I" $ do 
    it "should return S I (K I)" $ do
      reduction (S `Apply` S `Apply` K `Apply` I) `shouldBe` (S `Apply` I `Apply` (K `Apply` I))
  
  context "reduct S (I K)" $ do
    it "should return S K" $ do
      reduction (S `Apply` (I `Apply` K)) `shouldBe` (S `Apply` K)
  
  context "reduct I S (I K)" $ do
    it "should return S (I K)" $ do
      reduction (I `Apply` S `Apply` (I `Apply` K)) `shouldBe` (S `Apply` (I `Apply` K))
