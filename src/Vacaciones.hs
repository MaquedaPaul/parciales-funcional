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

type Tour = [Excursion]

completo :: Tour
completo = [caminarCiertosMinutos 20,apreciarElementoPaisaje "cascada",caminarCiertosMinutos 40, salirHablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursionElegida = [paseoEnBarco "tranquila",excursionElegida,caminarCiertosMinutos 120]

islaVecina :: String -> Excursion -> Tour
islaVecina "fuerte" excursionElegida = [apreciarElementoPaisaje "lago",paseoEnBarco "fuerte",excursionElegida,paseoEnBarco "fuerte"]

islaVecina marea excursionElegida = [irPlaya,paseoEnBarco marea,excursionElegida,paseoEnBarco marea]

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldl (flip hacerExcursion) (aumentarStressPorTour tour turista) tour

hacerTour' :: Turista -> Tour -> Turista
hacerTour' turista tour =
  foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

aumentarStressPorTour :: Tour -> Turista -> Turista
aumentarStressPorTour tour = alterarStress (+) (length tour) 

toursBienPiolas = [completo,(ladoB irPlaya),islaVecina "fuerte" irPlaya]


esConvincente :: [Tour] -> Turista -> Bool
esConvincente tours turista = (any.any) (((not.viajaSolo) turista &&).(esDesestresantePara turista)) tours

esConvincente'' :: [Tour] -> Turista -> Bool
esConvincente'' tours turista = any (propuestaConvincente turista) tours

propuestaConvincente :: Turista -> Tour -> Bool
propuestaConvincente turista tour = any (loDejaAcompaniado turista) (excursionesDesestresantes tour turista)

loDejaAcompaniado :: Turista -> Excursion -> Bool
loDejaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista



--------------------------------------------------------

propuestaConvincente' :: Turista -> [Tour] -> Bool
propuestaConvincente' turista = any (esConvincente' turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista

excursionesDesestresantes' :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes' turista = filter (esDesestresante turista)

esConvincente' :: Turista -> Tour -> Bool
esConvincente' turista = any (dejaAcompaniado turista) . excursionesDesestresantes' turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista

---------------------------------------------------------------------------

efectividadTour :: Tour -> [Turista] -> Int
efectividadTour tour = sum . map (flip espiritualidad tour) . filter (flip propuestaConvincente tour) 
    
    
    --map espiritualidad (filter (flip propuestaConvincente tour) turistas)


espiritualidad :: Turista -> Tour -> Int
espiritualidad turista = (*(-1)).(deltaRutina turista)

deltaRutina :: Turista -> Tour -> Int
deltaRutina turista tour = deltaSegun nivelDeRutina (hacerTour tour turista) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + stress turista


---------------------------------------------------------

efectividad' :: Tour -> [Turista] -> Int
efectividad' tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente' tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour = negate . deltaRutina' tour

deltaRutina' :: Tour -> Turista -> Int
deltaRutina' tour turista =
   deltaSegun nivelDeRutina (hacerTour tour turista) turista

-- nivelDeRutina :: Turista -> Int
-- nivelDeRutina turista = cansancio turista + stress turista



--4)
--a)

infinitasExcursiones :: Excursion -> Tour 
infinitasExcursiones excursion = excursion:(infinitasExcursiones excursion)

infinitasPlayas :: Tour
infinitasPlayas = infinitasExcursiones irPlaya

--b)
-- --¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
-- *Vacaciones> propuestaConvincente ana (take 5 infinitasPlayas)
-- True
-- *Vacaciones> propuestaConvincente ana (take 10 infinitasPlayas)
-- True
-- *Vacaciones> propuestaConvincente beto (take 10 infinitasPlayas)
-- False
-- *Vacaciones> propuestaConvincente beto (take 1 infinitasPlayas) 
--Se puede saber si cortamos la lista con take, no es necesario tomar millones de ejemplos, con solo el primero o a lo sumo 10 se puede averiguar.

--c)
-- *Vacaciones> efectividadTour (take 5000 infinitasPlayas) modeloTuristas
--30
--Converge a 30
salidaLocal :: Excursion
salidaLocal = salirHablarIdioma "melmacquiano"

playasEternas :: Tour
playasEternas = salidaLocal : repeat irPlaya

-- b)
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c)
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}
