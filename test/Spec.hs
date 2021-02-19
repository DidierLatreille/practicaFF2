import Test.Hspec
import Lib

main :: IO()
main = hspec $ do
   describe "costoTotal" $ do
      it "costoTotal con 'costoComida 500' y propinaRecomendada = 550" $ do
         costoTotal propinaRecomendada 500 `shouldBe` 550

      it "costoTotal con 'costoComida 500' y propinaMrPink = 500" $ do
         costoTotal propinaMrPink 500 `shouldBe` 500

      it "costoTotal con 'costoComida 500' y propinaConservadora = 520" $ do
         costoTotal propinaConservadora 500 `shouldBe` 520

      it "costoTotal con 'costoComida 501' y propinaConservadora = 526" $ do
         costoTotal propinaConservadora 501 `shouldBe` 526

      it "costoTotal con 'costoComida 500', propinaTioCarlos y el mozo se llama carlos= 600" $ do
         costoTotal (propinaTioCarlos "carlos") 500 `shouldBe` 600

      it "costoTotal con 'costoComida 500', propinaTioCarlos y el mozo se llama Jojo = 525" $ do
         costoTotal (propinaTioCarlos "Jojo") 500 `shouldBe` 525

      it "costoTotal con 'costoComida 500' y propinaPayasos = 550" $ do
         costoTotal (propinaPayasos "azul") 500 `shouldBe` 550

      it "costoTotal con 'costoComida 500' y propinaPayasos = 600" $ do
         costoTotal (propinaPayasos "rojo") 500 `shouldBe` 600

      it "costoTotal con 'costoComida 500' y propinaPayasos = 500" $ do
         costoTotal (propinaPayasos "negro") 500 `shouldBe` 500

      it "costoTotal con 'costoComida 400' y propinaSegunCosto = 520" $ do
         costoTotal propinaSegunCosto 400 `shouldBe` 520

      it "costoTotal con 'costoComida 600' y propinaSegunCosto = 620" $ do
         costoTotal propinaSegunCosto 600 `shouldBe` 620
      
      it "costoTotal con 'costoComida 601' y propinaSegunCosto = 626" $ do
         costoTotal propinaSegunCosto 601 `shouldBe` 626
      
      it "costoTotal con 'costoComida 1200' y propinaSegunCosto = 1200" $ do
         costoTotal propinaSegunCosto 1200 `shouldBe` 1200
      
      -- ¿Qué concepto aparece en la consulta del punto b. que no aparece en el punto a?
      -- NOTA: las consultas del punto a y b son las dos que se encuentran a continuacion
      -- en la consulta del punto b (la del tio carlos) aparece el concepto de pattern matching

      it "costoTotal con 'costoComida 100' y propinaRecomendada = 110" $ do
         costoTotal propinaRecomendada 100 `shouldBe` 110

      it "costoTotal con 'costoComida 200', propinaTioCarlos y el mozo se llama juan = 210 " $ do
         costoTotal (propinaTioCarlos "juan") 200 `shouldBe` 210
   
   describe "mozoSatisfecho" $ do

      it "Mozo satisfecho con propinaSegunCosto y 'costoComida 400'" $ do
         mozoSatisfecho propinaSegunCosto 400 `shouldBe` True

      it "Mozo NO satisfecho con propinaSegunCosto y 'costoComida 900'" $ do
         mozoSatisfecho propinaSegunCosto 900 `shouldBe` False

      it "Mozo NO satisfecho con propinaTioCarlos con mozo juan y 'costoComida 200'" $ do
         mozoSatisfecho (propinaTioCarlos "juan") 200 `shouldBe` False

      it "Mozo satisfecho con propinaTioCarlos con Mozo Juan y 'costoComida 200'" $ do
         mozoSatisfecho (propinaTioCarlos "carlos") 200 `shouldBe` True

