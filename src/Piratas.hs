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
    dimension :: Int,
    madera :: Int
} deriving (Show,Eq)


jeff = UnTripulante {
    nombre = "Jeff McGregory",
    energia = 60
}

carlos = UnTripulante {
    nombre = "Carlos Malasangre",
    energia = 40
} 

jorge = UnTripulante {
    nombre = "Jorge el Malhabido",
    energia = 30
} 

ramon = UnTripulante {
    nombre = "Ramon Ramonete",
    energia = 70
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
    dimension = 150,
    madera = 0
}
bergantin = UnBarco {
    tripulacion = [],
    oro = 0,
    balas = 0,
    dimension = 100,
    madera = 0
}
balandro = UnBarco {
    tripulacion = [],
    oro = 0,
    balas = 0,
    dimension = 50,
    madera = 0
}

galeonDeJeff = UnBarco {
    tripulacion = [jeff,carlos],
    oro = 20,
    balas = 20,
    dimension = 150,
    madera = 20
}

barcoMalandrines = UnBarco {
    tripulacion = [jorge,ramon],
    oro = 10,
    balas = 40,
    dimension = 100,
    madera = 10
}


esBarcoFantasma :: Barco -> Bool
esBarcoFantasma barco = all (estaMuerto) (tripulacion barco) || null(tripulacion barco)

cantidadTripulantes :: Barco -> Int
cantidadTripulantes= length.tripulacion

llenarBarcoMax :: Barco -> Barco
llenarBarcoMax barco = barco 
 {oro = 7* dimension barco, balas= cantidadTripulantes barco*3} 


type Enfrentamiento = Barco -> Barco -> Barco

enfrentamientoBarcos :: Enfrentamiento
enfrentamientoBarcos barco enemigo
 |esMasGrande barco enemigo && tieneBalas barco enemigo = ganaEncuentro barco enemigo
 |mismoTamano barco enemigo && tieneComponenteBarco madera barco (>=) enemigo = ganaEncuentro barco enemigo
 |esMasPequeno barco enemigo && tieneComponenteBarco (length.tripulacion) barco (>=) enemigo = ganaEncuentro barco enemigo
 |otherwise = pierdeEncuentro barco

ganaEncuentro :: Enfrentamiento
ganaEncuentro barcoGanador barcoPerdedor = barcoGanador {
oro = saquear oro barcoGanador barcoPerdedor,
madera = saquear madera barcoGanador barcoPerdedor,
balas = saquear balas barcoGanador barcoPerdedor
}
pierdeEncuentro :: Barco -> Barco
pierdeEncuentro barco = barco {oro = 0,
madera = 0,
balas = 0
}


saquear :: (Barco->Int) -> Barco -> Barco -> Int
saquear elementoSaqueado barco enemigo = (elementoSaqueado barco) + (elementoSaqueado enemigo)



-- data Barco = UnBarco {
--     tripulacion :: [Tripulante],
--     oro :: Int,
--     balas :: Int,
--     dimension :: Int,
--     madera :: Int
-- } deriving (Show,Eq)



esMasGrande :: Barco -> Barco -> Bool
esMasGrande barco enemigo = (dimension barco) > (dimension enemigo)

tieneBalas :: Barco -> Barco -> Bool
tieneBalas barco enemigo = (balas barco) >= (balas enemigo)
----------------------------------------------------------------


mismoTamano :: Barco -> Barco -> Bool
mismoTamano barco enemigo = (dimension barco) == (dimension enemigo)
------------------------------------------------------------------------

esMasPequeno :: Barco -> Barco -> Bool
esMasPequeno barco enemigo = (dimension barco) < (dimension enemigo)
------------------------------------------------------------------------
--Se puede parametrizar pero se pierde declaratividad

analizarTamano :: Barco -> (Int -> Int -> Bool) -> Barco -> Bool
analizarTamano barco parametro enemigo = (dimension barco) `parametro` (dimension enemigo)

tieneComponenteBarco :: (Barco -> Int) -> Barco -> (Int -> Int -> Bool) -> Barco -> Bool
tieneComponenteBarco componente barco parametro enemigo = (componente barco) `parametro` (componente enemigo)

---------------------------------------------------------

type Suceso = Barco -> Barco

embarcarTesoro :: Int -> Suceso
embarcarTesoro pesoEnOro barco = barco {
    tripulacion = (flip dividirRepartir (tripulacion barco)). cuantosNoMueren (transportarUnaCarga pesoEnOro) $ tripulacion barco,
    oro = pesoEnOro
    }

cuantosNoMueren :: Actividad -> [Tripulante] ->Int
cuantosNoMueren actividad = length.(filter (not.estaMuerto)).(map actividad)

dividirRepartir :: Int -> [Tripulante] -> [Tripulante]
dividirRepartir peso = map (transportarUnaCarga peso) 


modificarOroBarco :: Int -> Barco -> Barco
modificarOroBarco deltaOro barco = barco {oro = oro barco + deltaOro}



