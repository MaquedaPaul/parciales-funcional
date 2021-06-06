module Piratas where

import Text.Show.Functions

--type Habilidad = String

data Tripulante = UnTripulante {
    nombre :: String,
    energia :: Int
} deriving (Show,Eq)

type Actividad = Tripulante -> Tripulante


data Barco = UnBarco {
    tripulacion :: [Tripulante],
    oro :: Int,
    balas :: Int,
    dimension :: Int
} deriving (Show,Eq)


jeff = UnTripulante {
    nombre = "Jeff McGregory",
    energia = 60
}

carlos = UnTripulante {
    nombre = "Carlos Malasangre",
    energia = 40
} 

modificarEnergia ::  Int -> Tripulante -> Tripulante
modificarEnergia delta tripulante = tripulante {energia= energia tripulante + delta}

estaCasiMuerto :: Tripulante -> Bool
estaCasiMuerto =  (<50) . energia

enfrentarEsqueleto :: Actividad
enfrentarEsqueleto tripulante 
 |estaCasiMuerto tripulante = modificarEnergia (-20) tripulante
 |otherwise = modificarEnergia (-10) tripulante

transportarUnaCarga :: Int -> Actividad
transportarUnaCarga pesoCarga = modificarEnergia (-pesoCarga)

beberGrog :: Actividad
beberGrog = modificarEnergia (20)

estaMuerto :: Tripulante -> Bool
estaMuerto = (==0).energia


----------------------------------------

galeon = UnBarco {
    tripulacion = [],
    oro = 0,
    balas = 0,
    dimension = 150
}
bergantin = UnBarco {
    tripulacion = [],
    oro = 0,
    balas = 0,
    dimension = 100
}
balandro = UnBarco {
    tripulacion = [],
    oro = 0,
    balas = 0,
    dimension = 50
}

galeonDeJeff = UnBarco {
    tripulacion = [jeff,carlos],
    oro = 0,
    balas = 0,
    dimension = 150
}

esBarcoFantasma :: Barco -> Bool
esBarcoFantasma barco = all (estaMuerto) (tripulacion barco) || null(tripulacion barco)

cantidadTripulantes :: Barco -> Int
cantidadTripulantes= length.tripulacion

llenarBarcoMax :: Barco -> Barco
llenarBarcoMax barco = barco 
 {oro = 7* dimension barco, balas= cantidadTripulantes barco*3} 


-- data Barco = UnBarco {
--     tripulacion :: [Tripulante],
--     oro :: Int,
--     balas :: Int,
--     dimension :: Int
-- } deriving (Show,Eq)