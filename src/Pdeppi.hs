module Lib where
import Text.Show.Functions()
import Data.List 

type Cupon = Comida -> Comida

data Persona = Persona {
    nombrePersona :: String,
    direccion :: String,
    dineroDisponible :: Int,
    comidaFavorita :: Comida,
    cupones :: [Cupon]
} deriving (Show)

data Comida = Comida {
    nombreComida :: String,
    costo :: Int,
    ingredientes :: [String]
} deriving (Show,Eq)

paula :: Persona
paula = Persona{ 
    nombrePersona = "Paula" ,
    direccion = "Thames al 1585",
    comidaFavorita = hamburguesaDeluxe,
    dineroDisponible = 3600,
    cupones = []}

juampi :: Persona
juampi = Persona {
    nombrePersona = "Juan Pablo",
    direccion = "Boatti 336",
    comidaFavorita = milanesaConFritas,
    dineroDisponible = 780,
    cupones = [sinTACCis,esoNoEsCocaPapi "fernet"]
}

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida {nombreComida = "hamburguesa Deluxe",costo = 350 , ingredientes = ["pan", "carne","lechuga","tomate","panceta","queso","huevo frito"]}

milanesaConFritas :: Comida
milanesaConFritas = Comida {nombreComida = "milanesa con papas fritas" , costo = 400 , ingredientes = ["carne", "pan rallado" , "papas fritas"]}

comprar :: Comida -> Persona -> Persona
comprar unaComida unaPersona 
    |leAlcanzaLaPlata unaComida unaPersona && saleMenosDe 200 unaComida = (nuevaComidaFavorita unaComida) . cobramosLaComida unaComida $ unaPersona

    |leAlcanzaLaPlata unaComida unaPersona = cobramosLaComida unaComida unaPersona

    |otherwise = unaPersona

--  |otherwise = error "No se puede comprar, no le alcanza la plata"

cobramosLaComida :: Comida -> Persona -> Persona
cobramosLaComida unaComida unaPersona = unaPersona {dineroDisponible = dineroDisponible unaPersona - costo unaComida}

leAlcanzaLaPlata :: Comida -> Persona -> Bool
leAlcanzaLaPlata unaComida unaPersona = dineroDisponible unaPersona > costo unaComida

nuevaComidaFavorita :: Comida -> Persona -> Persona
nuevaComidaFavorita unaComida unaPersona = unaPersona {comidaFavorita = unaComida}

saleMenosDe :: Int -> Comida -> Bool
saleMenosDe valor unaComida = valor < costo unaComida

carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras unasComidas = modificarDineroDisponible (-(costoDelCarrito unasComidas)) 

--`` alt + 96
-- \ alt + 92


    --(+100).sum.map costo $ unasComidas

costoDelCarrito :: [Comida] -> Int
costoDelCarrito unasComidas = (+100).sum.map costo $ unasComidas


modificarDineroDisponible :: Int -> Persona -> Persona
modificarDineroDisponible deltaDinero unaPersona = unaPersona {dineroDisponible = dineroDisponible unaPersona + deltaDinero}


-- CUPONES


semanaVegana :: Cupon
semanaVegana unaComida 
    |laComidaEsVegana unaComida = reducirPorcentajeCostoComida 50 unaComida
    |otherwise = unaComida

laComidaEsVegana :: Comida -> Bool
laComidaEsVegana unaComida = not $ contieneIngrediente "carne" unaComida || contieneIngrediente "huevo" unaComida || contieneIngrediente "queso" unaComida


contieneIngrediente :: String -> Comida -> Bool
contieneIngrediente unIngrediente = elem unIngrediente.ingredientes 

-- contieneCarne :: Comida -> Bool
-- contieneCarne unaComida = elem "carne" $ingredientes unaComida

-- contieneHuevos :: Comida -> Bool
-- contieneHuevos unaComida = elem "huevos" $ingredientes unaComida

-- contieneQueso :: Comida -> Bool
-- contieneQueso unaComida = elem "queso" $ingredientes unaComida

agregarTextoAlNombreComida :: String -> Comida -> Comida
agregarTextoAlNombreComida texto unaComida = unaComida {nombreComida = nombreComida unaComida ++ texto}

agregarBebidaIngredientes :: String -> Comida -> Comida
agregarBebidaIngredientes bebida unaComida = unaComida {ingredientes = bebida : (ingredientes unaComida) }
-- ++ [bebida]
esoNoEsCocaPapi :: String -> Cupon 
esoNoEsCocaPapi bebida unaComida = agregarBebidaIngredientes bebida.agregarTextoAlNombreComida " Party" $unaComida

agregarTextoALosIngredientesComida :: String -> Comida -> Comida
agregarTextoALosIngredientesComida texto unaComida = unaComida {ingredientes = map (++ texto) $ ingredientes unaComida}

sinTACCis :: Cupon
sinTACCis  = agregarTextoALosIngredientesComida " libre de gluten" 

reducirPorcentajeCostoComida :: Int -> Comida -> Comida
reducirPorcentajeCostoComida porcentajeReducir unaComida = unaComida {costo = div ((100 - porcentajeReducir)* costo unaComida) 100}

findeVegetariano :: Cupon
findeVegetariano unaComida 
    |not.contieneIngrediente "carne" $unaComida = reducirPorcentajeCostoComida 30 unaComida
    |otherwise = unaComida

aumentamosPrecioComida :: Int -> Comida -> Comida
aumentamosPrecioComida valor unaComida = unaComida {costo = costo unaComida + valor}

filtramosIngredientesComidaMenoresA :: Int -> Comida -> Comida
filtramosIngredientesComidaMenoresA valor unaComida = unaComida {ingredientes = filter ((<valor).length) $ ingredientes unaComida}


largaDistancia :: Cupon
largaDistancia unaComida = filtramosIngredientesComidaMenoresA 10.aumentamosPrecioComida 50 $ unaComida

-- Lo comento para tenerlo mas a mano y no tener que subir
--comprar :: Comida -> Persona -> Persona
--comprar unaComida unaPersona 

comprarConCupones :: Persona -> Persona
comprarConCupones unaPersona =  comprar (aplicarCuponesComida (cupones unaPersona) (comidaFavorita unaPersona)) unaPersona

aplicarCuponesComida :: [Cupon] -> Comida -> Comida
aplicarCuponesComida unosCupones unaComida = foldl (\unaComida unCupon -> unCupon unaComida) unaComida unosCupones

superComida :: [Comida] -> Comida
superComida unasComidas = Comida {
    nombreComida = sinVocales.juntarNombres $ unasComidas,
    costo = sumPrecios unasComidas,
    ingredientes = sinIngredientesRepetidos.juntarIngredientes $unasComidas
}

sumPrecios :: [Comida] -> Int
sumPrecios unasComidas = sum.map costo $unasComidas

juntarIngredientes :: [Comida] -> [String]
juntarIngredientes unasComidas = concatMap ingredientes unasComidas

sinIngredientesRepetidos :: [String] -> [String]
sinIngredientesRepetidos [] = []
sinIngredientesRepetidos (cabeza:cola) = cabeza : filter (cabeza /=) cola

juntarNombres :: [Comida] -> String
juntarNombres unasComidas = concatMap ((" " ++) . nombreComida ) unasComidas

esVocal :: Char -> Bool
esVocal letra = elem letra ['a','e','i','o','u']

sinVocales :: String -> String
sinVocales unString = filter (not.esVocal) unString

-- Lo comento para tenerlo mas a mano y no tener que subir
-- comprar :: Comida -> Persona -> Persona
-- comprar unaComida unaPersona 

compraDeluxe :: Persona -> [Comida] -> Persona
compraDeluxe unaPersona unasComidas = comprar (superComida (duplicamosPrecioComidas.filterComidasMasBaratasQue 400 $unasComidas)) unaPersona

filterComidasMasBaratasQue :: Int -> [Comida] -> [Comida]
filterComidasMasBaratasQue valor unasComidas = filter ((< valor).costo) unasComidas

mapCosto :: (Int -> Int) -> Comida -> Comida
mapCosto funcion unaComida = unaComida {costo = funcion . costo $unaComida}

duplicamosPrecioComidas :: [Comida] -> [Comida]
duplicamosPrecioComidas unasComidas = map (mapCosto (*2)) unasComidas