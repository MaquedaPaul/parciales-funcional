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
estaMuerto = (<=0).energia


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
    tripulacion = [jeff,timoti,carlos],
    oro = 30,
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

--El problema con embarcarTesoro es que no funciona para pesos muy grandes (en el sentido que los tripulantes quedan en negativos muy grandes), habría que utilizar recursividad para resolverlo, ya que se estaría comprobando todo el tiempo si está muerto, aunque esto también depende de la exigencia del profesor
--Alguien muerto quiere decir energia = 0, Alguien que su cuerpo está destrozadísimo podría ser energía = -500 (y obviamente está muerto)

embarcarTesoro :: Int -> Suceso
embarcarTesoro pesoEnOro barco  
 |(not.esBarcoFantasma $ barco) && ((/=0).cuantosNoMueren (transportarUnaCarga pesoEnOro).tripulacion $ barco) = 
     barco {
    tripulacion = (flip dividirRepartir (tripulacion barco)) .
    (div pesoEnOro ) . cuantosNoMueren (transportarUnaCarga pesoEnOro) $ tripulacion barco,
    oro = pesoEnOro
    }
 |otherwise = barco

cuantosNoMueren :: Actividad -> [Tripulante] ->Int
cuantosNoMueren actividad = length.(filter (not.estaMuerto)).(map actividad)

dividirRepartir :: Int -> [Tripulante] -> [Tripulante]
dividirRepartir peso = map (transportarUnaCarga peso) 

--No se utilizó, pero puede ser util
modificarOroBarco :: Int -> Barco -> Barco
modificarOroBarco deltaOro barco = barco {oro = oro barco + deltaOro}


enfrentarBarco :: Barco -> Suceso
enfrentarBarco barco enemigo 
 |not.esBarcoFantasma $ barco = enfrentamientoBarcos barco enemigo
 |otherwise = barco

encontrarGrog :: Suceso
encontrarGrog barco 
 |not.esBarcoFantasma $ barco = barco {tripulacion = beberGrogTripulacion 5 barco}
 |otherwise = barco

beberGrogTripulacion :: Int -> Barco -> [Tripulante] 
beberGrogTripulacion repeticiones = last.take (repeticiones+1).iterate (map beberGrog).tripulacion

enfrentarEsqueletos :: Int -> Suceso
enfrentarEsqueletos esqueletos barco 
 |(esqueletos /= 0) && (not.esBarcoFantasma $ barco) = barco {
 tripulacion = (enfrentamientoConEsqueletos esqueletos barco):
 (tail.tripulantesConVida $ barco)}
 |otherwise = barco

enfrentamientoConEsqueletos :: Int -> Barco -> Tripulante
enfrentamientoConEsqueletos esqueletos = last . take (esqueletos+1) . iterate (enfrentarEsqueleto) . primerTripulanteConVida 

tripulantesConVida :: Barco -> [Tripulante]
tripulantesConVida = filter (not.estaMuerto) . tripulacion

primerTripulanteConVida :: Barco -> Tripulante
primerTripulanteConVida = head.tripulantesConVida
-- take esqueletos .cycle.enfrentarEsqueleto

beberGrogConRepeticiones :: Int -> Tripulante -> Tripulante
beberGrogConRepeticiones repeticiones tripulante = tripulante {energia=20*repeticiones}

------------------------------------------------------------------

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

ordenarPor :: Ord a => (b -> a) -> [b] -> [b] 
ordenarPor ponderacion =
  foldl (\ordenada elemento -> filter ((< ponderacion elemento).ponderacion) ordenada
                                  ++ [elemento] ++ filter ((>= ponderacion elemento).ponderacion) ordenada) []


pasarTiendaGrog :: Suceso
pasarTiendaGrog barco
 |(not.null.tripulantesMuertos $ barco) && (not.esBarcoFantasma $ barco) && (tieneOroSuficiente barco) = barco {
     tripulacion = tripulacionConRevivido barco,
 oro =  30 - oro barco
 }
 |otherwise=barco

tieneOroSuficiente :: Barco -> Bool
tieneOroSuficiente = (>=30).oro 

tripulantesMuertos :: Barco -> [Tripulante]
tripulantesMuertos = filter (estaMuerto) . tripulacion

tripulanteRevivido :: Barco -> Tripulante
tripulanteRevivido = beberGrog . head . ordenarPor (not.estaMuerto) . tripulacion

tripulacionConRevivido :: Barco -> [Tripulante]
tripulacionConRevivido barco = (tripulanteRevivido barco):(tail (ordenarPor (not.estaMuerto).tripulacion $ barco))

primerTripulanteMuerto :: Barco -> Tripulante
primerTripulanteMuerto = head.tripulantesMuertos

type Travesia = [Suceso]

recompensa ::Int -> Suceso
recompensa recompensa barco 
 |not.esBarcoFantasma $ barco = barco{oro = oro barco + recompensa}
 |otherwise = barco

recompensaPorTripulante :: Suceso
recompensaPorTripulante barco 
 |not.esBarcoFantasma $ barco = 
     barco{
     oro = oro barco + ((200*).length . 
     filter (not.estaMuerto).tripulacion $ barco)
     }
 |otherwise = barco

dobleRecompensa :: Suceso
dobleRecompensa barco
 |not.esBarcoFantasma $ barco = barco{oro = oro barco * 2}
 |otherwise = barco

--Me dí cuenta que se podía parametrizar así xdd pero ya jue
parametrizando :: Barco -> Bool -> Suceso -> Barco
parametrizando barco condicion accion
 |condicion = accion barco
 |otherwise = barco

fuerteDeLosCondenados :: Travesia
fuerteDeLosCondenados = [enfrentarEsqueletos 100, pasarTiendaGrog, embarcarTesoro 30,recompensa 50]

travesiaFlameHeart :: Travesia
travesiaFlameHeart = [flip enfrentarBarco galeonTFH,flip enfrentarBarco bergantinTFH,encontrarGrog, embarcarTesoro 150,recompensaPorTripulante]

laGirita :: Travesia
laGirita = [pasarTiendaGrogConRepeticion 4, enfrentarEsqueletos 10,dobleRecompensa]

pasarTiendaGrogConRepeticion :: Int -> Suceso
pasarTiendaGrogConRepeticion repeticiones = last.take repeticiones.iterate (pasarTiendaGrog)

superarTravesia :: Barco -> Travesia -> Barco
superarTravesia barco = foldl (superarSuceso) barco

superarSuceso :: Barco -> Suceso -> Barco
superarSuceso barco suceso = suceso barco 

galeonTFH = UnBarco{
    tripulacion = [jorge,ramon,excelsio,mequedesinideasxd],
    oro = 0,
    balas = 50,
    dimension = 50,
    madera = 50
}

bergantinTFH = UnBarco{
    tripulacion = [aber,jojos,thorfinn],
    oro = 0,
    balas = 30,
    dimension = 100,
    madera = 30
}



jeff = UnTripulante {
    nombre = "Jeff McGregory",
    energia = 7
}
timoti = UnTripulante {
    nombre = "Timoti El Vanidoso",
    energia = 3
}


carlos = UnTripulante {
    nombre = "Carlos Malasangre",
    energia = 7
} 

jorge = UnTripulante {
    nombre = "Jorge el Malhabido",
    energia = 30
} 

ramon = UnTripulante {
    nombre = "Ramon Ramonete",
    energia = 30
} 

excelsio = UnTripulante {
    nombre = "Excelsio Malapeste",
    energia = 30
} 

mequedesinideasxd = UnTripulante {
    nombre = "la idea",
    energia = 30
} 

jojos = UnTripulante {
    nombre = "IT'S ME! DIO",
    energia = 10000-9990
} 

thorfinn = UnTripulante {
    nombre = "Thorfinn hijo de Thors",
    energia = 100000-99970
} 

aber = UnTripulante {
    nombre = "aberrr",
    energia = 10
} 

