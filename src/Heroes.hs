import Text.Show.Functions
import Data.Char
import Data.List

data Heroe = Heroe {
    nombre :: String,
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareasRealizadas :: [Tarea]
} deriving (Show)

data Artefacto = Artefacto {
    nombreArtefacto :: String,
    rareza :: Int
}deriving (Show,Eq)

heracles :: Heroe
heracles = Heroe
 {  nombre = "Heracles" , 
    epiteto = "Guardian del Olimpo" ,
    reconocimiento = 700 ,
    artefactos = [pistola, relampagoDeZeuz],
    tareasRealizadas = [matarAlLeonDeNemea]
    }

lanzaDelOlimpo :: Artefacto 
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 1100

xiphos:: Artefacto 
xiphos = Artefacto "Xiphos" 50

relampagoDeZeuz :: Artefacto
relampagoDeZeuz = Artefacto "El relampago de Zeuz" 500

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

--------------------------Funciones Auxiliares---------------------------------------
mapArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefactos funcion unHeroe = unHeroe {artefactos = funcion.artefactos $unHeroe}

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza unArtefacto = unArtefacto{rareza = rareza unArtefacto * 3}

triplicarRarezaDeArtefactos :: [Artefacto] -> [Artefacto]
triplicarRarezaDeArtefactos unosArtefactos = map triplicarRareza unosArtefactos

tieneMinimoNRareza :: Int -> Artefacto -> Bool
tieneMinimoNRareza valor unArtefacto = rareza unArtefacto > valor

desecharArtefactoMalos :: [Artefacto] -> [Artefacto]
desecharArtefactoMalos listaArtefactos = filter (tieneMinimoNRareza 1000) listaArtefactos

modificarEpitetoConNIteracion :: Int -> Heroe -> Heroe
modificarEpitetoConNIteracion valor unHeroe = unHeroe {epiteto = epiteto unHeroe ++ replicate valor 'n'}

modificarEpiteto :: String -> Heroe -> Heroe
modificarEpiteto nuevoEpiteto unHeroe = unHeroe {epiteto = nuevoEpiteto}

perderNArtefactos :: Int -> [Artefacto] -> [Artefacto]
perderNArtefactos n unosArtefactos = drop n unosArtefactos

reconocimientoMayorA :: Int -> Heroe -> Bool
reconocimientoMayorA valor unHeroe = reconocimiento unHeroe > valor

reconocimientoIgualA :: Int -> Heroe -> Bool
reconocimientoIgualA valor unHeroe = reconocimiento unHeroe == valor

epitetoMayorA :: Int -> Heroe -> Bool
epitetoMayorA valor unHeroe = (> valor).length.epiteto $unHeroe

sumatoriaRarezasMayorA :: Int -> Heroe -> Bool
sumatoriaRarezasMayorA valor unHeroe = sumatoriaRarezasDeUnHeroe unHeroe > valor

sumatoriaRarezasDeUnHeroe :: Heroe -> Int
sumatoriaRarezasDeUnHeroe unHeroe = sum.map rareza $artefactos unHeroe
--------------------------------------------------------------------------------------

paseALaHistoria :: Heroe -> Heroe
paseALaHistoria unHeroe
    |reconocimientoMayorA 1000 unHeroe = modificarEpiteto "El mitico" unHeroe
    |reconocimientoMayorA 500 unHeroe = modificarEpiteto "El magnifico".agregarArtefacto lanzaDelOlimpo $unHeroe
    |reconocimientoMayorA 100 unHeroe = modificarEpiteto "Hoplita" . agregarArtefacto xiphos $unHeroe
    |otherwise = unHeroe

type Tarea = Heroe -> Heroe

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto unHeroe = agregarArtefacto unArtefacto.ganarReconocimiento (rareza unArtefacto) $unHeroe

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe}

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento valor unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + valor}

escalarElOlimpo :: Tarea
escalarElOlimpo unHeroe = agregarArtefacto relampagoDeZeuz.mapArtefactos(desecharArtefactoMalos.triplicarRarezaDeArtefactos).ganarReconocimiento 500 $unHeroe

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras unHeroe = modificarEpitetoConNIteracion cuadras.modificarEpiteto "Gros" $unHeroe

matarUnaBestia :: String -> Debilidad -> Tarea
matarUnaBestia nombreBestia debilidadBestia unHeroe
    |debilidadBestia unHeroe = modificarEpiteto ("El asesino de " ++ nombreBestia) unHeroe
    |otherwise = mapArtefactos (perderNArtefactos 1).modificarEpiteto "El cobarde" $unHeroe
    
siTieneElArtefacto :: Artefacto -> Heroe -> Bool
siTieneElArtefacto unArtefacto unHeroe = elem unArtefacto (artefactos unHeroe)


type Debilidad = (Heroe -> Bool)

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea unHeroe = matarUnaBestia "Leon de Nemea" (epitetoMayorA 20) unHeroe

hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea unHeroe = unaTarea unHeroe

sePresumen :: Heroe -> Heroe -> (Heroe,Heroe)
sePresumen unHeroe otroHeroe 
    |reconocimientoMayorA (reconocimiento otroHeroe) unHeroe = (unHeroe , otroHeroe)
    |reconocimientoIgualA (reconocimientoIgualA otroHeroe) unHeroe && sumatoriaRarezasMayorA (sumatoriaRarezasDeUnHeroe otroHeroe) unHeroe = (unHeroe , otroHeroe)
    |reconocimientoIgualA (reconocimientoIgualA otroHeroe) unHeroe = (otroHeroe , unHeroe)