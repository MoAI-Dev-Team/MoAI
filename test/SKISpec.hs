module SKISpec (spec) where

import Test.Hspec
import SKI

spec :: Spec
spec = describe "SKI.reduce" $ do
    context "reduce I I once" $ do
        it "sould return I" $ do
            reduce (I `Apply` I) `shouldBe` I
      
    context "reduce K S I once" $ do
        it "should return S" $ do
            reduce (K `Apply` S `Apply` I) `shouldBe` S
      
    context "reduce S S K I once" $ do 
        it "should return S I (K I)" $ do
            reduce (S `Apply` S `Apply` K `Apply` I) `shouldBe` (S `Apply` I `Apply` (K `Apply` I))
  
    context "reduce S (I K) once" $ do
        it "should return S K" $ do
            reduce (S `Apply` (I `Apply` K)) `shouldBe` (S `Apply` K)
  
    context "reduce I S (I K) once" $ do
        it "should return S (I K)" $ do
            reduce (I `Apply` S `Apply` (I `Apply` K)) `shouldBe` (S `Apply` (I `Apply` K))
  
    context "reduce I S (I K) to normal form" $ do
        it "should return S K" $ do
            reduceAll (I `Apply` S `Apply` (I `Apply` K)) `shouldBe` (S `Apply` K)
