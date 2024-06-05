module Library where
import PdePreludat

-- ---------------------- Dominio -------------------------
data Personaje = UnPersonaje {
    nombre :: Nombre, 
    dinero :: Dinero,
    felicidad :: Felicidad
} deriving (Show, Eq)

-- ---------------------- Definicion de Tipos -------------------------
type Nombre = String
type Dinero = Number
type Felicidad = Number

type Actividad = Personaje -> Personaje
-- ---------------------- Ejemplos -------------------------

homero :: Personaje
homero = UnPersonaje "Homero Simpson" 0.5 200

skinner :: Personaje
skinner = UnPersonaje "Skinner" 1000 30

lisa :: Personaje
lisa = UnPersonaje "Lisa Simpson" 20 500

-- ---------------------- Funciones Genericas -------------------------
-- ------------ Personaje
felicidadSegunF :: (Felicidad -> Felicidad) -> Personaje -> Personaje
felicidadSegunF f personaje = personaje {felicidad = max 0 . f . felicidad $ personaje}

dineroSegunF :: (Dinero -> Dinero) -> Personaje -> Personaje
dineroSegunF f personaje = personaje {dinero = f . dinero $ personaje}

nombreSegunF :: (Nombre -> Nombre) -> Personaje -> Personaje
nombreSegunF f personaje = personaje {nombre = f . nombre $ personaje}

-- ---------------------- Funciones -------------------------
-- ------------ Parte 1
-- Funcion 1
irEscuela :: Actividad
irEscuela personaje = 
    felicidadSegunF (cantSegunNombre personaje) personaje

cantSegunNombre :: Personaje -> (Felicidad -> Felicidad)
cantSegunNombre personaje 
    | nombre personaje == "Lisa" = (+20)
    | otherwise = subtract 20    

-- Funcion 2
comerDonas :: Number -> Actividad
comerDonas cant = 
    felicidadSegunF (felicidadXDona cant) . dineroSegunF (subtract 10)

felicidadXDona :: Number -> (Number -> Number)
felicidadXDona cant = (+(cant * 10))

-- Funcion 3
irTrabajar :: String -> Actividad
irTrabajar laburo = dineroSegunF (+ length laburo)

-- Funcion 4
irTrabajarDirector :: Actividad
irTrabajarDirector = irTrabajar "Escuela Elemental" . felicidadSegunF (subtract 20)

-- Funcion 5
irALaFacu :: Actividad
irALaFacu = 
    felicidadSegunF (subtract 1000000) . nombreSegunF (agregarPrefijo "Ingeniero ")

agregarPrefijo :: String -> Nombre -> Nombre
agregarPrefijo prefijo nombre = prefijo ++ nombre 

{-

EJEMPLOS DE USO:

- Homero come una docena de donas

> comerDonas 12 homero
UnPersonaje
    { nombre = "Homero Simpson"
    , dinero = -9.5
    , felicidad = 320
    }

- Skinner va a trabajar como director

> irTrabajarDirector skinner
UnPersonaje
    { nombre = "Skinner"
    , dinero = 1017
    , felicidad = 10
    }

- Lisa va a la escuela y luego realiza la actividad inventada.

> (irEscuela . irALaFacu) lisa
UnPersonaje
    { nombre = "Ingeniero Lisa Simpson"
    , dinero = 20
    , felicidad = 0
    }

-}










-- RESTAR 20 A FELCIIDAD ??














