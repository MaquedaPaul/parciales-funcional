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


-----------------------------------------------------


modeloTuristas = [ana,beto,cathi]

ana = UnTurista{
    cansancio = 0,
    stress = 21,
    viajaSolo = False,
    idiomas = ["Español"]
}

beto = UnTurista{
    cansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomas = ["Aleman"]
}

cathi = UnTurista{
    cansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomas = ["Aleman","Catalan"]
}

----------------------------------------------------------

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = (reducirEnPorcentaje (-10)).excursion

alterarStressPorcentual porcentaje turista = turista {stress =
 (stress turista) - ((porcentaje* (stress turista)) `div` 100)}


--(alterarStress (-) ((reducirEnPorcentaje 10 turista))) . excursion $ turista 



reducirEnPorcentaje :: Int -> Turista -> Turista
reducirEnPorcentaje porcentaje turista = alterarStress (+) (div (porcentaje * stress turista) 100) turista


cambiarStress delta turista = turista {stress = stress turista + delta}

cambiarStressPorcentual porciento turista =
  cambiarStress (div (porciento * stress turista) 100) turista
  
hacerExcursion' :: Excursion -> Turista -> Turista
hacerExcursion' excursion = cambiarStressPorcentual (-10) . excursion

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista
    
esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun (length.idiomas) turista excursion > 0

excursionesIsla :: [Excursion]
excursionesIsla = [irPlaya,salirHablarIdioma "Frances",paseoEnBarco "tranquila",paseoEnBarco "tranquila",paseoEnBarco "moderada",paseoEnBarco "fuerte",apreciarElementoPaisaje "El senior de las donas",caminarCiertosMinutos 5]

excursionesDesestresantes :: [Excursion] -> Turista -> [Excursion]
excursionesDesestresantes excursiones turista = filter (esDesestresantePara turista) excursiones

esDesestresantePara ::  Turista -> Excursion ->Bool
esDesestresantePara turista excursion = deltaExcursionSegun stress turista excursion <= (-3)




