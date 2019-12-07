module SKISpec (spec) where

import Test.Hspec
import SKI

spec :: Spec
spec = describe "SKI.reduction" $ do
  context "reduct S" $ do
    it "should return Nothing" $ do
      reduction S `shouldBe` Nothing

  context "reduct I I" $ do
    it "sould return Just I" $ do
      reduction (I `Apply` I) `shouldBe` Just I
      
  context "reduct K S I" $ do
    it "should return Just S" $ do
      reduction (K `Apply` S `Apply` I) `shouldBe` Just S
      
  context "reduct S S K I" $ do 
    it "should return Just S I (K I)" $ do
      reduction (S `Apply` S `Apply` K `Apply` I) `shouldBe` Just (S `Apply` I `Apply` (K `Apply` I))
