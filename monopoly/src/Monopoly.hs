import Text.Show.Functions

data Propiedad = Propiedad{
    nombreDePropiedad :: String,
    precio :: Int
} deriving (Show, Eq)

data Participante = Participante {
    nombreDeParticipante :: String,
    dinero :: Int,
    tactica :: Tactica, 
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
}deriving (Show)

type Accion = (Participante -> Participante)

type Tactica = String

montoInicial :: Int
montoInicial = 500

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

disminuyeDinero :: Int -> Participante -> Participante
disminuyeDinero unMonto unParticipante = unParticipante { dinero = (dinero unParticipante) - unMonto }

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

esAccionista :: Participante -> Bool
esAccionista unParticipante = (==accionista).tactica $ unParticipante

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante 
    | esAccionista unParticipante = aumentaDinero 200 unParticipante
    | otherwise = disminuyeDinero 100 unParticipante

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = (<150).precio $unaPropiedad

valorDeAlquiler :: Propiedad -> Int
valorDeAlquiler unaPropiedad
    | esPropiedadBarata unaPropiedad = 10
    | otherwise = 20

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = (flip aumentaDinero unParticipante).sum.(map valorDeAlquiler).propiedadesCompradas $ unParticipante

quitarAccion :: Participante -> Participante
quitarAccion unParticipante = unParticipante { acciones = tail.acciones $ unParticipante }

puedeComprarPropiedad :: Propiedad -> Participante -> Bool
puedeComprarPropiedad unaPropiedad unParticipante = (dinero unParticipante) >= (precio unaPropiedad) 

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante
    | puedeComprarPropiedad unaPropiedad unParticipante = comprarPropiedad unaPropiedad unParticipante
    | otherwise = (hacerBerrinchePor unaPropiedad).(aumentaDinero 10).agregarAccion gritar $ unParticipante 
    
carolina :: Participante
carolina = Participante "Carolina" montoInicial accionista [] [pasarPorElBanco, pagarAAccionistas] 

manuel :: Participante
manuel = Participante "Manuel" montoInicial oferenteSingular [] [pasarPorElBanco, enojarse]

ultimaRonda :: Participante -> Accion
ultimaRonda unParticipante = (foldl1 (.)).reverse $ acciones unParticipante

tieneMasDinero :: Participante -> Participante -> Bool
tieneMasDinero participante1 participante2 = (> dinero participante2).dinero $ participante1 

defineGanador :: Participante -> Participante -> Participante
defineGanador participante1 participante2
    | tieneMasDinero participante1 participante2 = participante1
    | otherwise = participante2

juegoFinal :: Participante -> Participante -> Participante
juegoFinal participante1 participante2 = defineGanador (ultimaRonda participante1 $ participante1) (ultimaRonda participante2 $ participante2)
    
cordoba :: Propiedad
cordoba = Propiedad "Cordoba" 550

misiones :: Propiedad
misiones = Propiedad "Misiones" 450



{- Para pruebas: -}
juan :: Participante 
juan = Participante "Juan" montoInicial accionista [] [pasarPorElBanco,pagarAAccionistas]
