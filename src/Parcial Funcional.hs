{-
Nombre: Maqueda, Pablo
Legajo: 176262-0
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

-- 1)
-- Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada y definir funciones para resolver los siguientes problemas:


data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show,Eq)

mario = UnAuto {
    color = "rojo",
    velocidad = 50,
    distancia = 300
} 

luigi = UnAuto {
    color = "verde",
    velocidad = 70,
    distancia = 330
} 


type Carrera = [Auto]

carreraChampinion = [luigi,mario]

-- a)
-- Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10.

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (sonDistintos auto1 auto2) && ((10<).distanciaEntreAutos auto1 $ auto2)

sonDistintos :: Auto -> Auto -> Bool
sonDistintos auto1 = (/= color auto1).color

distanciaEntreAutos :: Auto -> Auto -> Int
distanciaEntreAutos auto1 = abs.((-).distancia $ auto1).distancia

-- b)
-- Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (ningunAutoCerca auto carrera) && (lesVaGanando auto carrera)

ningunAutoCerca :: Auto -> Carrera -> Bool 
ningunAutoCerca auto carrera = all (not.estaCerca auto) carrera


lesVaGanando :: Auto -> Carrera -> Bool 
lesVaGanando auto = all ((<(distancia auto)).distanciaEntreAutos auto)

--- La idea es filtrar por todos aquellos que no le ganan al auto, de eso obtengo el tamaño de la lista para luego en "enQuePuestoEsta" sumarle 1
--Pero me pide una lista de carrera, no entiendo por qué, más adelante veré cómo lo puedo solucionar y sino queda este comentario.

enQuePuestoEsta :: Auto -> Carrera -> Int
enQuePuestoEsta auto = undefined--(1+).cantidadDeDelanteros auto

--Aquellos que les va ganando
cantidadDeDelanteros :: Auto -> Carrera -> Int
cantidadDeDelanteros auto = undefined --length . filter (not.lesVaGanando auto)


-- a)
-- Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo.

correDuranteTiempo :: Int -> Auto -> Auto
correDuranteTiempo tiempo auto = distanciaRecorrida auto . 
 ecuacionDistancia tiempo $ auto

ecuacionDistancia :: Int -> Auto -> Int
ecuacionDistancia tiempo auto = ((+).distancia $ auto).(*tiempo).velocidad $ auto

distanciaRecorrida :: Auto -> Int ->  Auto
distanciaRecorrida auto deltaDistancia  = auto {
    distancia = (distancia auto) + deltaDistancia} 


-- b)i)
-- A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.

modificador :: Int -> (Int -> Int) -> Int
modificador velocidad modificante = (modificante velocidad)

-- bajarVelocidad' :: Int -> Auto -> Auto
-- bajarVelocidad' cantidadIndicada auto = undefined

-- ii)
-- Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidadIndicada auto = auto
 {velocidad = max 0 (modificador (velocidad auto) ((-)cantidadIndicada))}


-- 3)
-- Como se explicó inicialmente sobre las carreras que queremos simular, los autos que participan pueden gatillar poderes especiales a los que denominamos power ups.
-- Estos poderes son variados y tienen como objetivo impactar al estado general de la carrera, ya sea afectando al auto que lo gatilló y/o a sus contrincantes dependiendo de qué poder se trate.


-- afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
-- afectarALosQueCumplen criterio efecto lista
--   = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


-- a)
-- terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) 

-- b) 
-- miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.

miguelitos :: Int -> PowerUp
miguelitos cantidadIndicada auto carrera = afectarALosQueCumplen
 (flip lesVaGanando carrera) (bajarVelocidad cantidadIndicada) carrera

-- c)
-- jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.


jetPack :: Int -> PowerUp
jetPack duracion auto carrera = afectarALosQueCumplen
 (autoQueGatilla) (efectoJetPack duracion) carrera

efectoJetPack :: Int -> Auto -> Auto
efectoJetPack duracion = (mitadVelocidad).(correDuranteTiempo duracion).duplicarVelocidad

colorAuto :: Auto -> String
colorAuto auto = color auto

--Me suena a elem, pero no lo pude implementar
autoQueGatilla :: Auto -> Bool
autoQueGatilla auto = (==(colorAuto auto)).color $ auto


mitadVelocidad auto = auto {velocidad = modificador (velocidad auto) (`div`2)}

duplicarVelocidad auto = auto {velocidad = modificador (velocidad auto) ((*)2)}

-- modificador :: Int -> (Int -> Int) -> Int
-- modificador velocidad modificante = (modificante velocidad)


-- bajarVelocidad :: Int -> Auto -> Auto
-- bajarVelocidad cantidadIndicada auto = auto
--  {velocidad = max 0 (modificador (velocidad auto) ((-)cantidadIndicada))}



-- 4)
-- A partir de todo lo construido hasta ahora queremos finalmente simular una carrera, para lo cual se provee una lista de eventos, que son funciones que permiten ir de un estado de la carrera al siguiente, y el estado inicial de la carrera a partir del cual se producen dichos eventos. Con esta información buscamos generar una “tabla de posiciones”, que incluye la información de en qué puesto quedó cada auto asociado al color del auto en cuestión.


-- a)
-- Desarrollar la función:
-- simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
-- que permita obtener la tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo cada evento uno detrás del otro, partiendo del estado de la carrera recibido.

type Evento = Carrera -> Carrera

type Color = String

--simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
--Si bien no es exactamente el tipo que me piden, estoy trabajando a ciegas al
--no haber podido en su momento resolver "enQuePosicionEsta", por eso el tipo 
--es Carrera -> Int.


simularCarrera :: Carrera -> [Evento] -> [(Carrera -> Int, Color)]
simularCarrera carrera = tablaDePosiciones . estadoFinalCarrera carrera

tablaDePosiciones :: Carrera -> [(Carrera -> Int, Color)]
tablaDePosiciones carrera = zip (posiciones carrera).coloresAutos $ carrera

coloresAutos :: Carrera -> [Color]
coloresAutos = map color

posiciones :: Carrera -> [Carrera -> Int]
posiciones carrera = map (enQuePuestoEsta) carrera 

--Se supone que estadoFinalCarrera debería devolverme el resultado de producir
--Cada evento uno tras de otro, y con ese resultado en mano, armo mi tabla de posiciones.

estadoFinalCarrera carrera eventos = foldl (producirEvento) carrera eventos

-- --producirEvento :: Evento -> Carrera
--Producir evento es la acción de un elemento que necesita foldl, para luego aplicarsela a un conjunto de eventos.
producirEvento evento = undefined


-- b)
-- Desarrollar las siguientes funciones de modo que puedan usarse para generar los eventos que se dan en una carrera:
-- i)
-- correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado.

correnTodos :: Int -> Evento
correnTodos tiempo = map (correDuranteTiempo tiempo)

-- type PowerUp = Auto -> Carrera -> Carrera

--Nuevamente me suena a elem, pero no lo pude implemenar.

--usaPowerUp :: String -> PowerUp  ->Carrera -> Carrera 
usaPowerUp colorDelAuto powerup = powerup.head.(busquedaAuto colorDelAuto)


busquedaAuto :: String -> Carrera -> Carrera
busquedaAuto colorAbuscar carrera = flip filter carrera ((==colorAbuscar).color)

--filter (=="hola") .coloresAutos
--filter (flip elem.(coloresAutos) $ carreraChampinion)
--flip filter carreraChampinion (elem ("hola").color)

-- correDuranteTiempo :: Int -> Auto -> Auto
-- correDuranteTiempo tiempo auto = distanciaRecorrida auto . 
--  ecuacionDistancia tiempo $ auto


-- 5)
-- En base a tu solución, responder:
-- a)
-- Si se quisiera agregar un nuevo power up, un misil teledirigido, que para poder activarlo se deba indicar el color del auto al que se quiere impactar, ¿la solución actual lo permite o sería necesario cambiar algo de lo desarrollado en los puntos anteriores? Justificar.

--Si bien la solución actual está incompleta, considero que es posible porque la función usaPowerUp lo permite.



-- b)
-- Si una carrera se conformara por infinitos autos, ¿sería posible usar las funciones del punto 1b y 1c de modo que terminen de evaluarse? Justificar.

--No sería posible con el 1b, ya que estaría intentando evaluar en una carrera
-- que tiene autos que en principio pueden estar desordenados, e incluso si estuvieran ordenados, debido al algoritmo lazy de haskell, y a la implementación del "all" en la solución, estaría intentando comprobarlo para todos, pero sabaemos que es infinito por lo cual es imposible.



-- a)
-- Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10.

-- b)
-- Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).
