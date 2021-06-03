module SpecTP3 where

import TP3
import Cafe
import Test.Hspec

spec = do
  describe "Cafetería" $ do
    it "armarCafe de 400 ml con 100 gramos de café sale con intensidad baja y caliente" $ do
      armarCafe 400 100 `shouldBe` Cafe {intensidad = 1, temperatura = 60, ml = 390}

    it "armarCafe de 1400 ml con 500 gramos de café sale intenso y caliente" $ do
      armarCafe 1400 500 `shouldBe` Cafe {intensidad = 9, temperatura = 60, ml = 990}

    it "armarFrapu con 20 gramos de café sale con intensidad baja y frío" $ do
      armarFrapu 20 `shouldBe` Cafe {intensidad = 2, temperatura = 12, ml = 190}

    it "armarFrapu con 50 gramos de café sale intenso y frío" $ do
      armarFrapu 50 `shouldBe` Cafe {intensidad = 9, temperatura = 12, ml = 190}