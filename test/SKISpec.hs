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
