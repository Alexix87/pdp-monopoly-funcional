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
    | esPropiedadBarata unaPropiedad = 150
    | otherwise = 200

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = (flip aumentaDinero unParticipante).sum.(map valorDeAlquiler).propiedadesCompradas $ unParticipante

esDueño :: Propiedad -> Participante -> Bool
esDueño unaPropiedad unParticipante = any (==unaPropiedad) $ propiedadesCompradas unParticipante

quitarAccion :: Participante -> Participante
quitarAccion unParticipante = unParticipante { acciones = tail.acciones $ unParticipante }

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante
    | not.esDueño unaPropiedad $ unParticipante =  (aumentaDinero 10).agregarAccion gritar $ unParticipante
    | otherwise = quitarAccion unParticipante

carolina :: Participante
carolina = Participante "Carolina" montoInicial accionista [] [pasarPorElBanco, pagarAAccionistas] 

manuel :: Participante
manuel = Participante "Manuel" montoInicial oferenteSingular [] [pasarPorElBanco, enojarse]


{- Para pruebas: -}
cordoba :: Propiedad
cordoba = Propiedad "Cordoba" 200

misiones :: Propiedad
misiones = Propiedad "Misiones" 100

juan :: Participante 
juan = Participante "Juan" montoInicial accionista [misiones,cordoba] [hacerBerrinchePor cordoba]

juega :: Participante -> Participante
juega unParticipante = acciones unParticipante !! 0 $ unParticipante
