module Thanos where

import Text.Show.Functions

type Habilidad = Int

data Personaje = UnPersonaje {
    nombre :: String,
    planeta :: String,
    edad :: Int,
    energia :: Int,
    habilidad :: [Habilidad]
} deriving (Show,Eq)


data Guantelete = UnGuantelete {
    material :: String,
    gemas :: Int
} deriving (Show,Eq)

data Universo = UnUniverso {
    personajes :: [Personaje]
} deriving (Show,Eq)

ironMan = UnPersonaje{
    nombre = "Tony Stark",
    planeta = "La Tierra",
    edad = 35,
    energia = 100,
    habilidad = [lanzarRayos,agilidad]
}

lanzarRayos = 5
conjurarHechizos = 3
agilidad = 2
estirarRaices = 4
sacarGarras = 1
hacerseElPiola = 0

drStrange = UnPersonaje{
    nombre = "Stephen Strange",
    planeta = "La Tierra",
    edad = 37,
    energia = 150,
    habilidad = [conjurarHechizos]
}
laViudaNegra = UnPersonaje{
    nombre = "La viuda negra",
    planeta = "La Tierra",
    edad = 35,
    energia = 100,
    habilidad = [agilidad]
}
groot = UnPersonaje{
    nombre = "Groot",
    planeta = "Planeta Gorgon",
    edad = 5,
    energia = 75,
    habilidad = [estirarRaices]
}
wolverine = UnPersonaje{
    nombre = "Wolverine",
    planeta = "La Tierra",
    edad = 27,
    energia = 66,
    habilidad = [sacarGarras,hacerseElPiola]
}


universoNormal = UnUniverso [ironMan,drStrange,wolverine,groot,laViudaNegra] 

chasquidoUniverso :: Universo -> Universo
chasquidoUniverso universo = universo {personajes = mitadDePersonajesUniverso universo} 

mitadDePersonajesUniverso :: Universo -> [Personaje]
mitadDePersonajesUniverso universo = take (length(personajes universo) `div` 2) (personajes universo)
    

aptoPendex :: Universo -> Bool
aptoPendex = any ((<45).edad).personajes

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . filter tieneMasDeUnaHabilidad . personajes 

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad = (>1).length.habilidad 



