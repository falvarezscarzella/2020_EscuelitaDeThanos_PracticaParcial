module Lib where
import Text.Show.Functions

laVerdad = True

--Punto 1
type Habilidad = String
type Planeta = String
data Personaje = UnPersonaje{
    nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad], 
    planeta :: Planeta
}

type Gema = Personaje -> Personaje
type Material = String
data Guantelete = Guantelete{
    material :: Material,
    gemas :: [Gema]
}

type Universo = [Personaje]

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo | puedeUsarseGuantelete guantelete = take ((`div`2).length $ universo) universo
                              | otherwise = universo

puedeUsarseGuantelete :: Guantelete -> Bool
puedeUsarseGuantelete guantelete = ((==6).length.gemas $ guantelete) && ((=="uru").material $ guantelete)

--Punto 2

saberDeUnUniverso :: (Universo -> a) -> Universo -> a
saberDeUnUniverso funcion universo = funcion universo

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any (<45) . map edad

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia 

--Punto 3

laMente :: Int -> Gema
laMente energiaQuitada = quitarEnergia energiaQuitada

quitarEnergia :: Int -> Personaje -> Personaje
quitarEnergia energiaQuitada personaje = personaje{energia = energia personaje - energiaQuitada}

elAlma :: Habilidad -> Gema
elAlma habilidadAQuitar = quitarEnergia 10 . eliminarHabilidad habilidadAQuitar

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad habilidad personaje = personaje{habilidades = filter (/= habilidad) (habilidades personaje)}

elEspacio :: Planeta -> Gema
elEspacio planeta = quitarEnergia 20 . transportarA planeta

transportarA :: Planeta -> Personaje -> Personaje
transportarA planeta personaje = personaje{planeta = planeta} 

elPoder :: Gema
elPoder personaje = quitarEnergia (energia personaje) . quitarleTodasHabilidades $ personaje

quitarleTodasHabilidades :: Personaje -> Personaje
quitarleTodasHabilidades personaje | (<=2).length.habilidades $ personaje = personaje{habilidades = []}
                                   | otherwise = personaje

elTiempo :: Gema
elTiempo = quitarEnergia 50 . quitarMitadEdad

quitarMitadEdad :: Personaje -> Personaje
quitarMitadEdad personaje = personaje{edad = (max 18.(`div`2).edad) personaje}

laGemaLoca :: Gema -> Gema
laGemaLoca gema = gema . gema 

--Punto 4

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete "Goma" [elTiempo, elAlma "Usar Mjolnir", laGemaLoca (elAlma "Programacion en Haskell")]

--Punto 5

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldl (\personaje gema -> gema $ personaje) personaje gemas

--Punto 6

laGemaMasPoderosa :: Guantelete -> Personaje -> Gema
laGemaMasPoderosa guantelete personaje = laGemaMasPoderosaDe (gemas guantelete) personaje

laGemaMasPoderosaDe :: [Gema] -> Personaje -> Gema
laGemaMasPoderosaDe [gema] _ = gema
laGemaMasPoderosaDe (gema1:gema2:gemas) personaje | (energia.gema1) personaje <= (energia.gema2) personaje = laGemaMasPoderosaDe (gema1:gemas) personaje
                                                  | otherwise = laGemaMasPoderosaDe (gema2:gemas) personaje

--Punto 7

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete
