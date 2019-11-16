module SKISpec (spec) where

import Test.Hspec
import SKI

test :: Int
test = 1

spec :: Spec
spec = describe "SKI.reduction" $ do
  context "reduct I I" $ do
    it "sould return I" $ do
      reduction (I $: I) `shouldBe` I
  context "reduct K S I" $ do
    it "should return S" $ do
      reduction ((K $: S) $: I) `shouldBe` S
  context "reduct 
