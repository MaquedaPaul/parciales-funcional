module Vacaciones where

import Text.Show.Functions


data Turista = UnTurista{
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
}deriving(Show,Eq)

ralf = UnTurista{
    cansancio = 5,
    stress = 3,
    viajaSolo = True,
    idiomas = ["Español","Ingles"]
}

type Excursion = Turista -> Turista

irPlaya :: Excursion
irPlaya turista 
 |(viajaSolo turista) = alterarCansancio (-) 5 turista
 |otherwise= alterarStress (-) 1 turista

alterarCansancio :: (Int->Int->Int) -> Int -> Turista -> Turista
alterarCansancio alteracion  cansancioReducido turista =  turista {cansancio=
 (cansancio turista) `alteracion` cansancioReducido}

alterarStress :: (Int->Int->Int) -> Int -> Turista -> Turista
alterarStress alteracion stressReducido turista  = turista {stress =
 (stress turista) `alteracion` stressReducido}

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje elementoApreciado = (alterarStress (-)).length $ elementoApreciado 

salirHablarIdioma :: String -> Excursion
salirHablarIdioma idioma turista = turista {viajaSolo=False,idiomas= aprenderIdioma idioma turista}

aprenderIdioma :: String -> Turista -> [String]
aprenderIdioma idioma turista = (idioma):(idiomas turista)

caminarCiertosMinutos :: Int -> Excursion
caminarCiertosMinutos minutos = 
 (alterarCansancio (+) (intensidadCaminata minutos)) . 
 (alterarStress (-) (intensidadCaminata minutos))


intensidadCaminata :: Int -> Int
intensidadCaminata  = (`div` 4)

paseoEnBarco :: String -> Excursion
paseoEnBarco "fuerte" turista = (alterarCansancio (+) 10) . (alterarStress (+) 6) $ turista
paseoEnBarco "moderada" turista= turista
paseoEnBarco "tranquila" turista = (salirHablarIdioma "Aleman").(apreciarElementoPaisaje "mar").(caminarCiertosMinutos 10) $ turista






