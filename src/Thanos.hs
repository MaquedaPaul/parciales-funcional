module Thanos where

import Text.Show.Functions

type Habilidad = String

data Personaje = UnPersonaje {
    nombre :: String,
    planeta :: String,
    edad :: Int,
    energia :: Int,
    habilidad :: [Habilidad]
} deriving (Show,Eq)


data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

data Universo = UnUniverso {
    personajes :: [Personaje]
} deriving (Show,Eq)

type Gema = Personaje -> Personaje

ironMan = UnPersonaje{
    nombre = "Tony Stark",
    planeta = "La Tierra",
    edad = 35,
    energia = 100,
    habilidad = [lanzarRayos,agilidad]
}

lanzarRayos = "Lanzar rayos"
conjurarHechizos = "Conjurar hechizos"
agilidad = "Agilidad"
estirarRaices = "Estirar raices"
sacarGarras = "Sacar garras"
hacerseElPiola = "Hacerse el piola"

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
    edad = 55,
    energia = 75,
    habilidad = [estirarRaices]
}
wolverine = UnPersonaje{
    nombre = "Wolverine",
    planeta = "La Tierra",
    edad = 27,
    energia = 66,
    habilidad = [sacarGarras,hacerseElPiola,"Buenas tardes"]
}


universoNormal = UnUniverso [ironMan,drStrange,wolverine,groot,laViudaNegra] 
chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo
 |puedeChasquear guantelete = chasquidoUniverso universo
 |otherwise= universo
puedeChasquear :: Guantelete -> Bool
puedeChasquear guantelete = (material guantelete) == "uru" && estaCompleto guantelete

estaCompleto :: Guantelete -> Bool
estaCompleto = (==6).length.gemas 

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

gemaMente :: Int -> Gema
gemaMente energiaDebilitada personajeAfectado =  personajeAfectado {energia= energiaDebilitada}

gemaAlma :: Habilidad -> Gema
gemaAlma habilidadEliminada personaje = personaje {habilidad = filter (/= habilidadEliminada) (habilidad personaje)}

gemaEspacio :: String -> Gema
gemaEspacio planetaAEnviar personaje = personaje{planeta= planetaAEnviar, energia = (energia personaje) - 20}

gemaPoder :: Gema
gemaPoder personaje = personaje {energia=0,habilidad = efectoGemaPoder personaje}


efectoGemaPoder :: Personaje -> [Habilidad]
efectoGemaPoder personaje 
 |tieneMenosDosHabilidades personaje = []
 |otherwise = (habilidad personaje)

tieneMenosDosHabilidades :: Personaje -> Bool
tieneMenosDosHabilidades = (<=2).length.habilidad 


gemaTiempo :: Gema
gemaTiempo personaje = personaje {edad = efectoGemaTiempo personaje,
energia = (energia personaje) - 50}

efectoGemaTiempo :: Personaje -> Int
efectoGemaTiempo personaje
 |esMenor personaje = 18
 |otherwise = (edad personaje) `div` 2

esMenor :: Personaje -> Bool
esMenor = (<18).(`div` 2).edad

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

guanteleteDeGoma = UnGuantelete{
    material = "Goma",
    gemas = [gemaTiempo,gemaAlma "usar Mjolnir",gemaLoca (gemaAlma "programacion en Haskell"),(gemaMente (-50))]
}
gemasQueEncontreAbajoDeMiCama = [gemaPoder,gemaEspacio "Marte",gemaMente 50,gemaTiempo,gemaAlma "usar Mjolnir",gemaLoca (gemaAlma "programacion en Haskell")]

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl (usarGema) enemigo gemas

usarGema :: Personaje -> Gema -> Personaje
usarGema personaje gema  = gema personaje

--hola @Pablo. en primer lugar cuando te dicen "efecto de lado", es lo mismo que "efecto colateral" o directamente "efecto" que es el término que normalmente usamos

-- En funcional estuvimos trabajando sin efecto, los datos son inmutables por ende no hay algo que podamos hacer para realmente cambiar a la víctima, sin embargo eso no impide que se pueda cumplir con el requerimiento que se pide en ese punto. Entonces cómo es que se logra ejecutar el poder de cada una de las gemas sobre la víctima, si no se pueden cambiar los datos?

-- Si no se pueden cambiar los datos, se van creando nuevas víctimas que tienen los campos afectados según la gema que se utilizó.
-- Por cada gema utilizada en una víctima, creo una más y a esa le aplicó la gema siguiente en la lista hasta terminar

gemaMasPoderosa ::  Guantelete -> Personaje -> Gema
gemaMasPoderosa = gemaDeMayorPoder.gemas

gemaDeMayorPoder :: [Gema] -> Personaje -> Gema
gemaDeMayorPoder [gema] _ = gema
gemaDeMayorPoder (gema1:gema2:gemas) personaje
 |menorEnergiaEnPersonaje gema1 gema2 personaje =
 gemaDeMayorPoder (gema2:gemas) personaje
 |otherwise = gemaDeMayorPoder (gema1:gemas) personaje

menorEnergiaEnPersonaje gema1 gema2 personaje = (energia.gema1 $ personaje) > (energia.gema2 $ personaje)


----------------------------------------------------------------

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas gemaTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

--- - - - - - ---->>>>>gemaMasPoderosa punisher guanteleteDeLocos
--No se puede ejecutar, es una lista infinita en donde no termina más porque no hay una condición de corte. El guantelete de locos tiene infinitas gemas de tiempo y al usar "gemaMasPoderosa", se intenta comparar constantemente la gema que hasta ahora fue la más poderosa, con la siguiente que es igual de poderosa que la anterior, y que las miles de millones que siguen. Pero eso no lo sabe nuestra función, e intenta cumplir a través de la resolución "lazy" (porque Haskell aprovecha la transparencia referencial), por eso el programa no tira algún error como podría pasar en otros lenguajes de programación que utilizan la resolución "eager".
-- - - - --  -- >> en cambio usoLasTresPrimerasGemas guanteleteDeLocos punisher
--Sí se puede ejecutar, porque TIENE una condición de corte y es "Usar las tres primeras gemas solamente" por lo cual nuestra función "gemaMasPoderosa" podrá hacer su trabajo feliz.

