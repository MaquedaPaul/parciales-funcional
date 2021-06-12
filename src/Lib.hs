{-
Nombre: Reskin, Gaston Ezequiel
Legajo: 167976-4
-}

module Lib where
import Text.Show.Functions

----------------------------------------------
-- Copiá acá el código de base
-- provisto en el enunciado
----------------------------------------------

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
-- indicando a qué punto pertenecen
----------------------------------------------

-- Punto 1 --

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq)

type Carrera = [Auto]

estaCercaDe :: Auto -> Auto -> Bool
estaCercaDe auto1 auto2 = abs (distancia auto1 - distancia auto2) < 10 && (auto1 /= auto2)

vaGanando :: Carrera -> Auto -> Bool
vaGanando carrera auto = maximum(map distancia carrera) == distancia auto

tieneAlgunAutoCerca :: Carrera -> Auto -> Bool
tieneAlgunAutoCerca carrera auto = any (estaCercaDe auto) (filter (/= auto) carrera)

vaTranquilo :: Carrera -> Auto -> Bool
vaTranquilo carrera auto = not(tieneAlgunAutoCerca carrera auto) && vaGanando carrera auto

vaAdelante :: Auto -> Auto -> Bool
vaAdelante auto1 auto2 = distancia auto1 < distancia auto2

enQuePuestoVa :: Carrera -> Auto -> Int
enQuePuestoVa carrera auto = length(filter (vaAdelante auto) carrera) + 1

--- Punto 2 ---

correr :: Int -> Auto -> Auto
correr tiempo auto = UnAuto {
    color = color auto, 
    velocidad = velocidad auto, 
    distancia = distancia auto + tiempo * (velocidad auto)
}

modificarVelocidad :: (Int -> Int) -> Auto -> Auto
modificarVelocidad modificador auto = UnAuto {
    color = color auto,
    velocidad = (modificador.velocidad) auto,
    distancia = distancia auto
} 

reducirVelocidad :: Int -> Int -> Int
reducirVelocidad reduccion velocidad = max (velocidad - reduccion) 0

-- Ejemplo de uso:
-- modificarVelocidad (reducirVelocidad 10) (UnAuto {color = "Rojo", velocidad = 100, distancia = 60})
-- deberia retornar UnAuto {color = "Rojo", velocidad = 90, distancia = 60}

--- Punto 3 ---
type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto carrera = afectarALosQueCumplen (estaCercaDe auto) (modificarVelocidad (reducirVelocidad 50)) carrera

miguelitos :: Int -> PowerUp
miguelitos cantidad auto carrera =  afectarALosQueCumplen (flip vaAdelante auto) (modificarVelocidad (reducirVelocidad cantidad)) carrera

jetPack :: Int -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (== auto) (modificarVelocidad (div 2).correr tiempo. modificarVelocidad (*2)) carrera

aplicarPowerUp :: PowerUp -> Auto -> Carrera -> Carrera
aplicarPowerUp pwrUp auto carrera = pwrUp auto carrera

--- Punto 4 ---

type Evento = Carrera -> Carrera
type Color = String
type Posicion = (Int, Color)

ejecutarEvento :: Carrera -> Evento -> Carrera
ejecutarEvento carrera evento = evento carrera

autoAPosicion :: Carrera -> Auto -> Posicion
autoAPosicion carrera auto = (enQuePuestoVa carrera auto, color auto) 

generarTablaDePosiciones :: Carrera -> [Posicion]
generarTablaDePosiciones carrera = map (autoAPosicion carrera) carrera

simularCarrera :: Carrera -> [Evento] -> [Posicion]
simularCarrera carrera eventos = generarTablaDePosiciones(foldl ejecutarEvento carrera eventos) 

correnTodos :: Int -> Evento
correnTodos n carrera = map (correr n) carrera

esDeColor :: Color -> Auto -> Bool
esDeColor colorAuto auto = color auto == colorAuto

buscarAutoXColor :: Color -> Carrera -> Auto
buscarAutoXColor colorAuto (auto:autos) | esDeColor colorAuto auto = auto 
                                        | otherwise = buscarAutoXColor colorAuto autos

usaPowerUp :: Color -> PowerUp -> Evento
usaPowerUp colorAuto pwrUp carrera = pwrUp (buscarAutoXColor colorAuto carrera) carrera 

autoRojo = UnAuto "Rojo" 120 0
autoBlanco = UnAuto "Blanco" 120 0
autoAzul = UnAuto "Azul" 120 0 
autoNegro = UnAuto "Negro" 120 0

carrera :: Carrera
carrera = [autoRojo, autoBlanco, autoAzul, autoNegro]

eventos :: [Evento]
eventos = [correnTodos 30, 
           usaPowerUp "Azul" (jetPack 3), 
           usaPowerUp "Blanco" terremoto, 
           correnTodos 40, 
           usaPowerUp "Blanco" (miguelitos 20),
           usaPowerUp "Negro" (jetPack 6),
           correnTodos 10]
{-
    Al usar:  
    simularCarrera carrera eventos 
       
    La terminal devuelve: 
    [(2,"Negro"),(3,"Rojo"),(4,"Azul"),(1,"Blanco")]
-}


--- Punto 5 --- 

{-
5A) Si se quisiera agregar un nuevo power up, un misil teledirigido, 
    que para poder activarlo se deba indicar el color del auto al que se quiere impactar, 
    ¿la solución actual lo permite o sería necesario cambiar algo de lo desarrollado en los puntos anteriores?
    Justificar

    se podria implementar usando la funcion buscarAutoXColor:
        buscarAutoXColor :: Color -> Carrera -> Auto
        buscarAutoXColor colorAuto (auto:autos) | esDeColor colorAuto auto = auto 
                                                | otherwise = buscarAutoXColor colorAuto autos

    y aplicando los efectos del misil teledirigido a dicho auto, independientemente de quien lo haya tirado.


5B) Si una carrera se conformara por infinitos autos, 
    ¿sería posible usar las funciones del punto 1b y 1c de modo que terminen de evaluarse? 
    Justificar.

    No, ya que dada una lista infinita de autos, por ejemplo, en la funcion vaTranquilo:
        vaTranquilo :: Carrera -> Auto -> Bool
        vaTranquilo carrera auto = not(tieneAlgunAutoCerca carrera auto) && vaGanando carrera auto
    
    Que utiliza la funcion tieneAlgunAutoCerca:
        tieneAlgunAutoCerca :: Carrera -> Auto -> Bool
        tieneAlgunAutoCerca carrera auto = any (estaCercaDe auto) (filter (/= auto) carrera)

    En el caso de que tieneAlgunAutoCerca, no consiga encontrar algun auto cercano, 
    el any va a estar ejecutandose infinitamente.

    En el caso de la funcion enQuePuestoVa, del punto 1c, pasa algo similar:
        enQuePuestoVa :: Carrera -> Auto -> Int
        enQuePuestoVa carrera auto = length(filter (vaAdelante auto) carrera) + 1

    la lista de autos "carrera" en este caso es infinita, 
    y la funcion no tiene ningun tipo de argumento que defina cuando para de filtrar,
    ya que busca revisar TODOS los autos de la lista, para ver en que puesto va. 
-}

