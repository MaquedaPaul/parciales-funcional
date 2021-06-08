module Barbaros where

import Text.Show.Functions

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




