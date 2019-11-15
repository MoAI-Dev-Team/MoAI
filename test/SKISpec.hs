module SKISpec (spec) where

import Test.Hspec
import SKI

spec :: Spec
spec = describe "SKI.reduction" $ do
  context "reduct (I I)" $ do
    it "sould return I" $ do
      reduction (I :$ I) `shouldBe` I
