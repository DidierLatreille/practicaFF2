module Lib where

laVerdad = True

type ColorCorbata = String
type NombreMozo = String
type CriterioPropina = Double -> Double

propinaRecomendada :: CriterioPropina
propinaRecomendada costoComida = costoComida * 0.1

propinaMrPink :: CriterioPropina
propinaMrPink _ = 0

propinaConservadora :: CriterioPropina
propinaConservadora costoComida
  | (even . floor) costoComida = 20
  | otherwise                  = 20 + 5

propinaTioCarlos :: NombreMozo -> CriterioPropina
propinaTioCarlos "carlos"  costoComida = ((* 2) . propinaRecomendada) costoComida
propinaTioCarlos nombreMozo costoComida = ((/ 2) . propinaRecomendada) costoComida

costoTotal :: CriterioPropina -> Double -> Double
costoTotal propina costoComida = costoComida + propina costoComida

propinaPayasos :: ColorCorbata -> CriterioPropina
propinaPayasos "azul" costoComida = propinaRecomendada costoComida
propinaPayasos "rojo" _ = 100
propinaPayasos "negro" _ = 0

propinaSegunCosto :: CriterioPropina
propinaSegunCosto costoComida | costoComida <= 500 = ((3*).propinaRecomendada) costoComida 
                              | costoComida < 1000 = propinaConservadora costoComida
                              | otherwise = 0

mozoSatisfecho :: CriterioPropina -> Double -> Bool
mozoSatisfecho propina costoComida = propina costoComida >= costoComida * 0.15
