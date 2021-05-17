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
agregarAccion unaAccion unParticipante = unParticipante { acciones =unaAccion : (acciones unParticipante)}

aumentarDinero :: Int -> Participante -> Participante
aumentarDinero unMonto unParticipante = unParticipante { dinero = (+unMonto).dinero $ unParticipante } 

disminuirDinero :: Int -> Participante -> Participante
disminuirDinero unMonto unParticipante = unParticipante { dinero = (dinero unParticipante) - unMonto }

cambiarTactica :: Tactica -> Participante -> Participante
cambiarTactica unaTactica unParticipante = unParticipante {tactica = unaTactica}

agregarPropiedad :: Propiedad -> Participante -> Participante
agregarPropiedad unaPropiedad unParticipante = unParticipante { propiedadesCompradas = unaPropiedad : (propiedadesCompradas unParticipante) }

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (aumentarDinero 40).cambiarTactica compradorCompulsivo $ unParticipante

enojarse :: Accion
enojarse unParticipante = (aumentarDinero 50).agregarAccion gritar $ unParticipante

gritar :: Accion
gritar unParticipante = unParticipante { nombreDeParticipante = ("AHHH" ++).nombreDeParticipante $ unParticipante }

ganadoresDeSubasta :: [Tactica]
ganadoresDeSubasta = [accionista, oferenteSingular]

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad unaPropiedad unParticipante = (disminuirDinero.precio $ unaPropiedad).agregarPropiedad unaPropiedad $ unParticipante

esGanadorDeSubasta :: Participante -> Bool
esGanadorDeSubasta unParticipante = (flip elem ganadoresDeSubasta).tactica $ unParticipante

subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante
    | (esGanadorDeSubasta unParticipante) && (dinero unParticipante >= precio unaPropiedad) = comprarPropiedad unaPropiedad unParticipante
    | otherwise = unParticipante

esAccionista :: Participante -> Bool
esAccionista unParticipante = (==accionista).tactica $ unParticipante

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante 
    | esAccionista unParticipante = aumentarDinero 200 unParticipante
    | otherwise = disminuirDinero 100 unParticipante

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = (<150).precio $unaPropiedad

valorDeAlquiler :: Propiedad -> Int
valorDeAlquiler unaPropiedad
    | esPropiedadBarata unaPropiedad = 10
    | otherwise = 20

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = (flip aumentarDinero unParticipante).sum.(map valorDeAlquiler).propiedadesCompradas $ unParticipante

puedeComprarPropiedad :: Propiedad -> Participante -> Bool
puedeComprarPropiedad unaPropiedad unParticipante = (dinero unParticipante) >= (precio unaPropiedad) 

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante
    | puedeComprarPropiedad unaPropiedad unParticipante = comprarPropiedad unaPropiedad unParticipante
    | otherwise = (hacerBerrinchePor unaPropiedad).(aumentarDinero 10).gritar $ unParticipante 
    
carolina :: Participante
carolina = Participante "Carolina" montoInicial accionista [] [pagarAAccionistas, pasarPorElBanco] 

manuel :: Participante
manuel = Participante "Manuel" montoInicial oferenteSingular [] [enojarse, pasarPorElBanco]

ultimaRonda :: Participante -> Accion
ultimaRonda unParticipante = (foldl (.) id).acciones $ unParticipante

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
juan = Participante "Juan" montoInicial accionista [] [hacerBerrinchePor cordoba,subastar misiones,pasarPorElBanco]
