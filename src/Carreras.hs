module Lib where
import Text.Show.Functions()
import Data.List

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show,Eq)

mario = Auto {
    color = "rojo",
    velocidad = 30,
    distancia = 85
}

luigi = Auto {
    color = "verde",
    velocidad = 60,
    distancia = 90
}

tortuga = Auto {
    color = "marron",
    velocidad = 5,
    distancia = 2
}

type Carrera = [Auto]


estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto otroaAuto = (sonAutosDistintos unAuto otroaAuto) && ((<10).distanciaEntre2Autos unAuto $otroaAuto)


sonAutosDistintos :: Auto -> Auto -> Bool
sonAutosDistintos unAuto otroAuto = color unAuto /= color otroAuto


distanciaEntre2Autos :: Auto -> Auto -> Int
distanciaEntre2Autos unAuto otroAuto = abs (distancia unAuto - distancia otroAuto)


vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = vaPrimero unAuto unaCarrera && all (not.estaCerca unAuto) unaCarrera-- me falta pasarle lo de esta cerca

vaPrimero :: Auto -> Carrera -> Bool
vaPrimero unAuto unaCarrera = (distancia unAuto) == (maximum.map distancia $unaCarrera)


--Conocer en qué puesto está un auto en una carrera,que es 1 + la cantidad de autos de la carreraque le van ganando.

leVaGanando :: Auto -> Auto -> Bool
leVaGanando autoGanador autoPerdedor = distancia autoGanador >= (distancia autoPerdedor)

puesto :: Auto -> Carrera -> Int
puesto unAuto unaCarrera = (+1).length.filter (not.leVaGanando unAuto) $ unaCarrera

corre :: Int -> Auto -> Auto
corre tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo * (velocidad unAuto)}



alterarVelocidad :: Int -> Auto -> Int
alterarVelocidad valor unAuto = velocidad unAuto + valor

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad valor unAuto = unAuto{velocidad = max 0 $ alterarVelocidad (-valor) unAuto}

type Poder = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) ->[a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter(not.criterio) lista


terremoto :: Poder
terremoto unAuto unaCarrera = afectarALosQueCumplen (estaCerca unAuto) (bajarVelocidad 50) unaCarrera

--terremoto :: Poder
--terremoto unAuto unaCarrera = map (bajarVelocidad 50).filter (estaCerca unAuto) $unaCarrera

miguelitos :: Int -> Poder
miguelitos valor unAuto unaCarrera = afectarALosQueCumplen (not.leVaGanando unAuto) (bajarVelocidad valor) unaCarrera

--miguelitos :: Int -> Poder
--miguelitos valor unAuto unaCarrera = map (bajarVelocidad valor).filter (not.leVaGanando unAuto) $unaCarrera

modificarVelocidad :: (Int -> Int) -> Auto -> Auto
modificarVelocidad funcion unAuto = unAuto {velocidad = funcion.velocidad $unAuto}

jetpack :: Int -> Poder
jetpack tiempo unAuto unaCarrera = afectarALosQueCumplen (== unAuto) (modificarVelocidad (`div` 2).corre tiempo.modificarVelocidad (*2)) unaCarrera

type Evento = Carrera -> Carrera
type Color = String
type Posicion = (Int , Color)

ejecutarEvento :: Carrera -> Evento -> Carrera
ejecutarEvento unaCarrera unEvento = unEvento unaCarrera
-- (\unaCarrera unEvento -> unEvento unaCarrera) tambien se puede hacer asi es lo mismo

--puesto :: Auto -> Carrera -> Int
--puesto unAuto unaCarrera = (+1).length.filter (not.leVaGanando unAuto) $ unaCarrera

autoAPosicion :: Carrera -> Auto -> Posicion
autoAPosicion unaCarrera unAuto = (puesto unAuto unaCarrera , color unAuto )

generarTablaPosiciones :: Carrera -> [Posicion]
generarTablaPosiciones unaCarrera = map (autoAPosicion unaCarrera) unaCarrera

simularCarrera :: [Evento]-> Carrera -> [Posicion]
simularCarrera eventos unaCarrera = generarTablaPosiciones $ foldl (ejecutarEvento) unaCarrera eventos


correnTodos :: Int -> Evento
correnTodos tiempo unaCarrera = map (corre tiempo) unaCarrera

esDeColor :: Color -> Auto -> Bool
esDeColor colorAuto unAuto = colorAuto == (color unAuto)

buscarAutoXColor :: Color -> [Auto] -> Auto
buscarAutoXColor unColor (auto:autos)
    |esDeColor unColor auto = auto
    |otherwise = buscarAutoXColor unColor autos

usaPowerUp :: Color -> Poder -> Evento
usaPowerUp unColor powerUp unaCarrera = powerUp (buscarAutoXColor unColor unaCarrera) unaCarrera

autoRojo = Auto {color = "rojo", velocidad = 120 , distancia = 0}
autoAzul = Auto {color = "azul", velocidad = 120 , distancia = 0}
autoBlanco = Auto {color = "blanco", velocidad = 120 , distancia = 0}
autoNegro = Auto {color = "negro", velocidad = 120 , distancia = 0}

carreraSimulada :: Carrera
carreraSimulada = [autoRojo, autoAzul, autoBlanco, autoNegro] 

eventosSimulados :: [Evento]
eventosSimulados = [
    correnTodos 30,
    usaPowerUp "azul" (jetpack 3),
    usaPowerUp "blanco" terremoto ,
    correnTodos 40,
    usaPowerUp "blanco" (miguelitos 20),
    usaPowerUp "negro" (jetpack 6) ,
    correnTodos 10
    ]

--RESULTADO QUE ARROJA LA CARRERA 
--[(3,"negro"),(1,"azul"),(4,"rojo"),(2,"blanco")]


--5A Si te lo permite debido a que tenemos la funcion buscarAutoXColor y despues
-- aplicarmos los efectos del misil teledirigido a dicho auto y no nos interesa quien es el que lo dispara.

--5b No seria imposible ya que para poder usar vaTranquilo o elPuesto necesito tener obligatoriamente
-- una lista finita de elementos para poderlos comparar y asi poder obtener el resultado. Si la lista es 
-- infinita nunca terminaria de evaluarse y el programa quedaria colgado.