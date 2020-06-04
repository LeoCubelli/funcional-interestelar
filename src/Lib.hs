module Lib where
import Text.Show.Functions

laVerdad = True

type Nombre = String
type Posicion = (Float, Float, Float)
type Edad = Float
type Año = Float

data Astronauta = UnAstronauta {
    nombreAstronauta :: Nombre,
    edadAstronauta :: Edad,
    planetaAstronauta :: Planeta
} deriving (Show)

nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p

data Planeta = UnPlaneta {
     nombrePlaneta :: Nombre,
     posicionPlaneta :: Posicion,
     pasoDelTiempo :: (Float -> Float)
} deriving (Show)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

data Nave =
      NaveVieja {
          piloto :: Astronauta,
          tanquesOxigeno :: Float
     }
      | NaveFuturista {
          piloto :: Astronauta
     } deriving (Show)


--Punto 1a
distanciaEntrePlanetas :: Planeta -> Planeta -> Float
distanciaEntrePlanetas p1 p2 = (sqrt.abs) ((diferencia coordX p1 p2)^2 + (diferencia coordY p1 p2)^2 + (diferencia coordZ p1 p2)^2)

diferencia :: Num a => (Posicion -> a) -> Planeta -> Planeta -> a
diferencia coord planeta1 planeta2 = (coord . posicion) planeta1 - (coord . posicion) planeta2 

--Punto 1b
cuantoTarda :: Float -> Planeta -> Planeta -> Float
cuantoTarda velocidad planeta1 = ((/velocidad).distanciaEntrePlanetas planeta1)

--Punto 2
calcularAños :: Astronauta -> Año -> Año
calcularAños astronauta años = ((tiempo.planeta) astronauta ) años

pasarTiempo :: Float -> Astronauta -> Astronauta
pasarTiempo años = aumentarEdad (calcularAños astronauta años)

aumentarEdad :: Float -> Astronauta -> Astronauta
aumentarEdad años astronauta = astronauta {
    edadAstronauta = edad astronauta + años 
}

--Punto 3 
tierra = UnPlaneta "Tierra" (0, 0, 0) id
marte = UnPlaneta "Marte" (4, 3, 0) (*2)
astronauta = UnAstronauta "Carlos" 20 tierra

modificarPlaneta :: Planeta -> Astronauta -> Astronauta
modificarPlaneta planeta astronauta = astronauta {
    planetaAstronauta = planeta
}

viajar :: Nave -> Planeta -> Planeta -> Astronauta
viajar (NaveVieja astronauta tanques) planeta1 planeta2
    | tanques > 6 = (modificarPlaneta planeta2 . aumentarEdad (cuantoTarda 10 planeta1 planeta2)) astronauta
    | otherwise = (modificarPlaneta planeta2 . aumentarEdad (cuantoTarda 5 planeta1 planeta2)) astronauta

viajar (NaveFuturista astronauta) planeta1 planeta2 = modificarPlaneta planeta2 astronauta

--Punto 4