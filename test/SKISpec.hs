module SKISpec (spec) where

import Test.Hspec
import SKI

spec :: Spec
spec = describe "SKI.reduction" $ do
  context "reduct I I" $ do
    it "sould return Just I" $ do
      reduction (I `Apply` I) `shouldBe` I
      
  context "reduct K S I" $ do
    it "should return Just S" $ do
      reduction (K `Apply` S `Apply` I) `shouldBe` S
      
  context "reduct S S K I" $ do 
    it "should return Just S I (K I)" $ do
      reduction (S `Apply` S `Apply` K `Apply` I) `shouldBe` (S `Apply` I `Apply` (K `Apply` I))
