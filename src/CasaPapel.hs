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

type Plan = String

data Rehen = UnRehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    plan :: Plan
} deriving (Show,Eq)


tokio = UnLadron {
    nombreLadron = "Tokio",
    habilidades = ["trabajo psicologico","entrar en moto"],
    armas = [pistola 30,pistola 15]
}
profesor = UnLadron {
    nombreLadron = "Profesor",
    habilidades = ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"],
    armas = [pistola 30]
}


pablo = UnRehen {
    nombreRehen = "Pablo",
    nivelComplot = 40,
    nivelMiedo = 40,
    plan = "Esconderse"
}

arturito = UnRehen {
    nombreRehen = "Arturo",
    nivelComplot = 70,
    nivelMiedo = 50,
    plan = "Esconderse y luego atacar con Pablo"
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


disparos :: [Arma] -> Rehen -> Rehen
disparos unasArmas rehen = undefined


armaQueLeGeneraMasMiedo ::  Rehen -> [Arma] -> Rehen
armaQueLeGeneraMasMiedo rehen unasArmas = maximoSegun (nivelMiedo) . map (aplicarArma rehen) $ unasArmas


maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


listaArmas = [pistola 5, ametralladora 30]



aplicarArma :: Rehen -> Arma -> Rehen
aplicarArma rehen unArma  = unArma rehen


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

-- hacerseElMalo :: Ladron -> Rehen -> Rehen
-- hacerseElMalo (Ladron "Berlin" _ unasHabilidades) rehen = modificarMiedoRehen (lenght.concat $ unasHabilidades) 
-- hacerseElMalo (Ladron "Rio" _ _ ) rehen = modificarComplotRehen 20 rehen
-- hacerseElMalo (Ladron _ _ _) rehen = modificarMiedoRehen 10 rehen
 
-- Ctrl + K + C
-- Ctrl + K + U
-- Ctrl + Flecha
-- Ctrl + Shift + Flecha
-- Ctrl + Flecha arriba

-- Atacar al ladrón: le quita tantas armas como la cantidad de letras del nombre de su compañero de ataque, dividido por 10.








