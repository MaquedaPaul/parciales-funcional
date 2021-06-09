module Barbaros where

import Text.Show.Functions
import Data.Char

type Habilidad = String

-- data Barco = UnBarco {
--     tripulacion :: [Tripulante],
--     oro :: Int,
--     balas :: Int,
--     dimension :: Int,
--     madera :: Int
-- } deriving (Show,Eq)

data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
} deriving (Show)

dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

type Objeto = Barbaro -> Barbaro

ardilla :: Objeto
ardilla barbaro = barbaro

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro= agregarHabilidad "hacer magia" barbaro {objetos= []}

agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad habilidad barbaro = barbaro {
    habilidades = habilidad:(habilidades barbaro)}

modificarFuerza :: Int -> Barbaro -> Barbaro
modificarFuerza deltaFuerza barbaro = barbaro{
    fuerza = fuerza barbaro + deltaFuerza
} 

espada :: Int -> Objeto
espada peso = modificarFuerza (peso*2)

amuletosMisticos :: String -> Objeto
amuletosMisticos habilidad = agregarHabilidad habilidad

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 = objeto1.objeto2

megafono :: Objeto
megafono barbaro = barbaro {habilidades = [map toUpper . concat . habilidades $ barbaro]}

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

type Evento = Barbaro -> Bool
type Aventura = [Evento]

type GrupoBarbaros = [Barbaro]

aventuraBarbarica :: Aventura
aventuraBarbarica = [invasionDeSuciosDuendes,cremalleraDelTiempo,ritualDeFechorias]

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes = sobreviveSi any (== "Escribir Poesia Atroz").habilidades

cremalleraDelTiempo :: Evento
cremalleraDelTiempo = sobreviveSi flip elem (noTienePulgares).nombre

noTienePulgares :: [String]
noTienePulgares = ["Faffy","Astro"]

sobreviveSi operacion condicion atributo = operacion (condicion) atributo

ritualDeFechorias :: Evento 
ritualDeFechorias barbaro = any (flip pasaPrueba barbaro) [saqueo,gritoDeGuerra,caligrafia] --sobreviveSi

type Prueba = Barbaro -> Bool

pasaPrueba :: Prueba -> Barbaro -> Bool
pasaPrueba prueba = prueba

saqueo :: Prueba
saqueo barbaro = (tieneHabilidad "saquear" barbaro) && (tieneFuerza 80 barbaro)

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad habilidad = any (==habilidad).habilidades 

tieneFuerza :: Int -> Barbaro -> Bool
tieneFuerza fuerzaRequerida = (>=fuerzaRequerida).fuerza

gritoDeGuerra :: Prueba
gritoDeGuerra barbaro = ((*4).cantidadObjetos $ barbaro) < (poderGritoGuerra barbaro)

cantidadObjetos :: Barbaro -> Int
cantidadObjetos = length.objetos

poderGritoGuerra :: Barbaro -> Int
poderGritoGuerra = length.concat.habilidades.megafono

caligrafia :: Prueba
caligrafia = cumpleEstandarBarbarico.habilidades

cumpleEstandarBarbarico :: [String] -> Bool
cumpleEstandarBarbarico habilidades = (all comienzaConMayuscula habilidades) && (all masDeTresVocales habilidades)

comienzaConMayuscula :: String -> Bool
comienzaConMayuscula palabra = ((==).head $ palabra).toUpper $ head palabra 

masDeTresVocales :: String -> Bool
masDeTresVocales = (>3).length

vocales :: String -> String
vocales palabra = filter (esVocal) palabra

esVocal :: Char -> Bool
esVocal caracter = elem (caracter) ['a','e','i','o','u']

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros aventura = filter (flip sobreviveAventura aventura) barbaros

sobreviveAventura :: Barbaro -> Aventura -> Bool
sobreviveAventura barbaro aventura = all (sobreviveEvento barbaro) aventura

sobreviveEvento :: Barbaro -> Evento -> Bool
sobreviveEvento barbaro evento = evento barbaro 