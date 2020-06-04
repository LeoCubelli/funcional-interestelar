module Lib where
import Text.Show.Functions

laVerdad = True

type Nombre = String
type Posicion = (Float, Float, Float)
type Edad = Int

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
     pasoDelTiempo :: (Int -> Int)
} deriving (Show)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

tierra = UnPlaneta "Tierra" (0, 0, 0) id
marte = UnPlaneta "Marte" (4, 3, 0) (*2)

--Punto 1a
distanciaEntrePlanetas :: Planeta -> Planeta -> Float
distanciaEntrePlanetas p1 p2 = (sqrt.abs) ((diferencia coordX p1 p2) + (diferencia coordY p1 p2) + (diferencia coordZ p1 p2))

diferencia :: Num a => (Posicion -> a) -> Planeta -> Planeta -> a
diferencia coord planeta1 planeta2 = (coord . posicion) planeta1 - (coord . posicion) planeta2 

--Punto 1b
cuantoTarda :: Float -> Planeta -> Planeta -> Float
cuantoTarda velocidad planeta1 = ((/velocidad).distanciaEntrePlanetas planeta1)

--Punto 2
astronauta = UnAstronauta "Carlos" 29 marte

calcularAños :: Astronauta -> Int -> Int
calcularAños astronauta años = ((tiempo.planeta) astronauta ) años

pasarTiempo :: Astronauta -> Int -> Astronauta
pasarTiempo astronauta años = astronauta {
    edadAstronauta = edad astronauta + (calcularAños astronauta años) 
}

--Punto 3 