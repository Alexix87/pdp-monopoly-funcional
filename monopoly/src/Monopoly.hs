import Text.Show.Functions

montoInicial :: Int
montoInicial = 500

{-- tipos de datos: --}

data Propiedad = Propiedad{
    nombreDePropiedad :: String,
    precio :: Int
} deriving (Show)

data Participante = Participante {
    nombreDeParticipante :: String,
    dinero :: Int,
    tactica :: Tactica, 
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
}deriving (Show)

type Accion = (Participante -> Participante)

type Tactica = String

accionista :: Tactica
accionista = "Accionista"

oferenteSingular :: Tactica
oferenteSingular = "Oferente singular"

compradorCompulsivo :: Tactica
compradorCompulsivo = "Comprador compulsivo"

agregarAccion :: Accion -> Participante -> Participante
agregarAccion unaAccion unParticipante = unParticipante { acciones =(acciones unParticipante) ++ [unaAccion]}

aumentaDinero :: Int -> Participante -> Participante
aumentaDinero unMonto unParticipante = unParticipante { dinero = (+unMonto).dinero $ unParticipante } 

 
{--acciones: --}
pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = aumentaDinero 40 unParticipante {tactica = compradorCompulsivo}

enojarse :: Accion
enojarse unParticipante = (aumentaDinero 50).agregarAccion gritar $ unParticipante

gritar :: Accion
gritar unParticipante = unParticipante { nombreDeParticipante = ("AHHH" ++).nombreDeParticipante $ unParticipante }

ganadoresDeSubasta :: [Tactica]
ganadoresDeSubasta = [accionista, oferenteSingular]

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad unaPropiedad unParticipante = unParticipante {dinero = (dinero unParticipante) - (precio unaPropiedad), propiedadesCompradas = propiedadesCompradas unParticipante ++[unaPropiedad]}

esGanadorDeSubasta :: Participante -> Bool
esGanadorDeSubasta unParticipante = (flip elem ganadoresDeSubasta).tactica $ unParticipante

subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante
    | esGanadorDeSubasta unParticipante = comprarPropiedad unaPropiedad unParticipante
    | otherwise = unParticipante


pagarAAccionistas :: Accion
pagarAAccionistas unParticipante  = unParticipante

carolina :: Participante
carolina = Participante "Carolina" montoInicial accionista [] [pasarPorElBanco, pagarAAccionistas] 

manuel :: Participante
manuel = Participante "Manuel" montoInicial oferenteSingular [] [pasarPorElBanco, enojarse]

{- Para pruebas: -}
cordoba :: Propiedad
cordoba = Propiedad "Cordoba" 200

juan :: Participante 
juan = Participante "Juan" montoInicial compradorCompulsivo [] [subastar cordoba]

juega :: Participante -> Participante
juega unParticipante = acciones unParticipante !! 0 $ unParticipante
