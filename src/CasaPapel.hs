module CasaDePapel where

import Text.Show.Functions
-- import Data.Char
-- import Data.List

type Arma = Rehen -> Rehen
type Habilidad = String


data Ladron = UnLadron {
    nombreLadron :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
} deriving (Show)



data Rehen = UnRehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    plan :: [Plan]
} deriving (Show)


tokio = UnLadron {
    nombreLadron = "Tokio",
    habilidades = ["trabajo psicologico","entrar en moto"],
    armas = [pistola 9,pistola 9,ametralladora 30]
}
profesor = UnLadron {
    nombreLadron = "Profesor",
    habilidades = ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"],
    armas = []
}

pablo = UnRehen {
    nombreRehen = "Pablo",
    nivelComplot = 40,
    nivelMiedo = 30,
    plan = [esconderse]
}

arturito = UnRehen {
    nombreRehen = "Arturo",
    nivelComplot = 70,
    nivelMiedo = 50,
    plan = [esconderse, atacarAlLadronCon pablo]
    }

pistola :: Int -> Arma
pistola calibre rehen = modificarComplotRehen (-(5*calibre)) . 
 modificarMiedoRehen ((*3).length.rehenNombre $ rehen) $ rehen


rehenNombre :: Rehen -> String
rehenNombre = nombreRehen

modificarComplotRehen :: Int -> Rehen -> Rehen
modificarComplotRehen deltaComplot rehen = rehen {nivelComplot = nivelComplot rehen + deltaComplot}

-- modificarAtributoRehen :: Int -> (Rehen -> Int) -> Rehen -> Rehen
-- modificarAtributoRehen deltaAtributo atributo rehen = rehen {atributo= atributo rehen + deltaAtributo}


modificarMiedoRehen :: Int -> Rehen -> Rehen
modificarMiedoRehen deltaMiedo rehen = rehen {nivelMiedo = nivelMiedo rehen + deltaMiedo}


-- Pistola: reduce el nivel de complot de un rehén en 5 veces su calibre, y aumenta su miedo en 3 por la cantidad de letras de su nombre
-- Ametralladora: siempre reduce el nivel de complot a la mitad, y aumenta su miedo en la cantidad de balas que le quedan.


ametralladora :: Int -> Arma
ametralladora cantidadBalas rehen = modificarComplotRehen 
 (- mitadComplot rehen) . modificarMiedoRehen cantidadBalas $ rehen


mitadComplot :: Rehen -> Int
mitadComplot = (`div`2).nivelComplot

--Disparos: disparar al techo como medida disuasiva. Se usa el arma que le genera más miedo al rehén intimidado


type Intimidar = Ladron -> Rehen -> Rehen

disparos :: Intimidar
disparos unLadron rehen = armaQueLeGeneraMasMiedo rehen (ladronArmas unLadron)


armaQueLeGeneraMasMiedo ::  Rehen -> [Arma] -> Rehen
armaQueLeGeneraMasMiedo rehen unasArmas = maximoSegun (nivelMiedo) . map (aplicarArma rehen) $ unasArmas


maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


listaArmas = [pistola 5, ametralladora 30]



aplicarArma :: Rehen -> Arma -> Rehen
aplicarArma rehen unArma = unArma rehen


-- armaConMasMiedo :: [Arma] -> Rehen -> Bool
-- armaConMasMiedo armas rehen = filter ((==(nivelConMasMiedo armas rehen)).nivelMiedo $ rehen) armas

-- nivelConMasMiedo :: [Arma] -> Rehen -> Int
-- nivelConMasMiedo armas = maximum . nivelesDeMiedo armas

-- nivelesDeMiedo :: [Arma] -> Rehen -> [Int]
-- nivelesDeMiedo armas rehen = map nivelMiedo . map (aplicarArma rehen) $ armas

-- (maximum (map (nivelMiedo) (map (aplicarArma pablo) $ listaArmas)))






-- filter (==nivelMiedo)




{-
Hacerse el malo: 
Cuando el que se hace el malo es Berlín, aumenta el miedo del rehén tanto como la cantidad de letras que sumen sus habilidades.
Cuando Río intenta hacerse el malo, le sale mal y en cambio aumenta el nivel de complot del rehén en 20. 
En otros casos, el miedo del rehén sube en 10. 

-}

-- hacerseElMalo :: Intimidar
-- hacerseElMalo (UnLadron "Berlin" _ unasHabilidades) rehen = modificarMiedoRehen (lenght.concat $ unasHabilidades) 
-- hacerseElMalo (UnLadron "Rio" _ _ ) rehen = modificarComplotRehen 20 rehen
-- hacerseElMalo (UnLadron _ _ _) rehen = modificarMiedoRehen 10 rehen
 
-- Ctrl + K + C
-- Ctrl + K + U
-- Ctrl + Flecha
-- Ctrl + Shift + Flecha
-- Ctrl + Flecha arriba

--A los rehenes no les gusta ser rehenes, por eso intentan rebelarse contra los ladrones, siempre que tengan más complot que miedo, 
--ideando planes como:

-- Atacar al ladrón: le quita tantas armas como la cantidad de letras del nombre de su compañero de ataque, dividido por 10.

type Plan = Ladron -> Ladron

ladronArmas :: Ladron -> [Arma]
ladronArmas = armas

quitarArmasLadron :: Int -> Ladron -> Ladron
quitarArmasLadron cantidadSacar unLadron = unLadron {armas = take (cantidadTotalArmas unLadron - cantidadSacar).armas $ unLadron}


cantidadTotalArmas :: Ladron -> Int
cantidadTotalArmas = length.armas

cantidadTotalArmasLista :: [Ladron] -> Int
cantidadTotalArmasLista unLadron = sum. map cantidadTotalArmas $unLadron

atacarAlLadronCon ::  Rehen -> Plan
atacarAlLadronCon compañeroAtaque unLadron = quitarArmasLadron (flip div 10.length.nombreRehen $ compañeroAtaque) unLadron

atacarAlLadronCon' ::  Rehen -> Plan
atacarAlLadronCon' compañeroAtaque unLadron = quitarArmasLadron (flip div 10.longitudNombre $ compañeroAtaque) unLadron

longitudNombre :: Rehen -> Int
longitudNombre rehen = length.nombreRehen $rehen





--Esconderse: Hace que un ladrón pierda una cantidad de armas igual a su cantidad de habilidades dividido 3. 

cantidadHabilidadesLadron :: Ladron -> Int
cantidadHabilidadesLadron unLadron = length.habilidades $unLadron

esconderse :: Plan
esconderse unLadron = quitarArmasLadron (div (cantidadHabilidadesLadron unLadron)  3) unLadron


--  2-  Saber si un ladrón es inteligente. Ocurre cuando tiene más de dos habilidades, además el Profesor es la mente maestra, 
--  por lo que indudablemente es inteligente.

esInteligente :: Ladron -> Bool
esInteligente unLadron = ((>2).cantidadHabilidadesLadron $unLadron) || ((== "Profesor").nombreLadron $unLadron)

-- 3 Que un ladrón consiga un arma nueva, y se la agregue a las que ya tiene.


consigueArmaNueva :: Ladron -> Arma -> Ladron
consigueArmaNueva unLadron unArma = unLadron {armas = unArma : armas unLadron}

--4 Que un ladrón intimide a un rehén, usando alguno de los métodos planeados.

-- 5 Que un ladrón calme las aguas, disparando al techo frente a un grupo de rehenes, 
--de los cuales se calman los que tengan más de 60 de complot.

complotRehenMayorA :: Int -> Rehen -> Bool
complotRehenMayorA numero rehen = (> numero).nivelComplot $rehen

calmeLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmeLasAguas ladron unosRehenes = filter(complotRehenMayorA 60) . map (disparos ladron) $unosRehenes 

-- 6-Saber si un ladrón puede escaparse de la policía. Esto se cumple cuando alguna de las habilidades del ladrón empieza con “disfrazarse de”.

empiezaConDisfrazarseDe :: Habilidad -> Bool
empiezaConDisfrazarseDe unaHabilidad = (== "disfrazarse de") . take 14 $ unaHabilidad

puedeEscaparse :: Ladron -> Bool
puedeEscaparse ladron = any (empiezaConDisfrazarseDe) (habilidades ladron)

{-
    7-Saber si la cosa pinta mal, que es cuando dados unos ladrones y unos rehenes,
     el nivel de complot promedio de los rehenes es mayor al nivel de miedo promedio multiplicado por la cantidad de armas de los ladrones.
-}

--sumarNivelComplot :: [Rehen] -> Int
--sumarNivelComplot unosRehenes = sum.map (nivelComplot) $unosRehenes

nivelComplotPromedio :: [Rehen] -> Int
nivelComplotPromedio unosRehenes = div (sum.map (nivelComplot) $unosRehenes) (length unosRehenes)

nivelMiedoPromedio :: [Rehen] -> Int
nivelMiedoPromedio unosRehenes = div (sum.map (nivelMiedo) $unosRehenes) (length unosRehenes)

cantidadArmasLadrones :: [Ladron] -> Int
cantidadArmasLadrones unosLadrones = length.concat.map (armas) $unosLadrones

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal unosLadrones unosRehenes = nivelComplotPromedio unosRehenes > (nivelMiedoPromedio unosRehenes * cantidadArmasLadrones unosLadrones)

{-
type Plan = Ladron -> Ladron

esconderse :: Plan
esconderse unLadron = quitarArmasLadron (div (cantidadHabilidadesLadron unLadron)  3) unLadron

atacarAlLadron ::  Rehen -> Plan
atacarAlLadron compañeroAtaque unLadron = quitarArmasLadron (flip div 10.length.nombreRehen $compañeroAtaque) unLadron
-}

seRebelan :: [Rehen] -> Ladron -> Ladron
seRebelan unosRehenes  unLadron = seRebelanContraLadron unosRehenes unLadron 


seRebelanContraLadron :: [Rehen] -> Ladron -> Ladron
seRebelanContraLadron unosRehenes unLadron = foldl (aplicarPlan) unLadron (concatMap plan unosRehenes)

rehenesConComplotModificado :: Int -> [Rehen] -> [Rehen]
rehenesConComplotModificado deltaComplot unosRehenes = modificarComplotRehenes deltaComplot unosRehenes


modificarComplotRehenes :: Int -> [Rehen] -> [Rehen]
modificarComplotRehenes deltaComplot unosRehenes = map (modificarComplotRehen deltaComplot) unosRehenes

aplicarPlan :: Ladron -> Plan -> Ladron
aplicarPlan ladron unPlan = unPlan ladron


-- 9 Necesito resolver el 8 para poderlo hacer

planValencia :: [Rehen] -> [Ladron] -> Int
planValencia unosRehenes unosLadrones =  (*1000000).cantidadTotalArmasLista.map (seRebelanContraLadron unosRehenes.flip consigueArmaNueva (ametralladora 45)) $unosLadrones

--10 No se puede ya que un ladron al contar con una cantidad infinita de armas no terminaria nunca de multiplicar

--11 Hay 2 respuestas posibles
-- si solamente dentro de los planes de los ladrones esta atacarAlLadron si se puede ya que no me interesan las habilidades
-- Pero en cambio si dentro de la lista de planes esta esconderse no se va a poder ya que al ser infinito el programa se me queda colgado
