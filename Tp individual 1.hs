import Text.Show.Functions
data Tesoro= UnTesoro {nom:: String, valor:: Int} deriving Show
data Pirata=UnPirata {nombre:: String, botin:: [Tesoro]} deriving Show

jack=UnPirata "Jack Sparrow" [UnTesoro"llavero" 10000,UnTesoro "moneda" 6]
david=UnPirata "David Jones" [UnTesoro "cajita musical" 1]
anne= UnPirata "Anne Bonny" [UnTesoro "doblones" 100, UnTesoro "frasco de arena" 1]
pruebaPirata=UnPirata "prueba" [UnTesoro"llaver" 100001,UnTesoro "m" 6]

llavero=UnTesoro"llavero" 10000


--TESOROS PIRATAS
cantidadDeTesoros::Pirata->Int
cantidadDeTesoros pirata=length (botin pirata)

listaValores::[Tesoro]->[Int]
listaValores lista= map valor lista
pirataAfortunado::(Pirata)->Bool
pirataAfortunado pirata= (sum.listaValores) (botin pirata)>10000

esMismoTesoroConValorDiferente:: Tesoro-> Tesoro->Bool
esMismoTesoroConValorDiferente t1 t2=((nom t1)==(nom t2))&&((valor t1) /= (valor t2))
loTieneElOtro ::Pirata -> Tesoro -> Bool
loTieneElOtro pirata t= any(esMismoTesoroConValorDiferente t)(botin pirata)
tienenElMismoTesoroConValorDiferente::Pirata->Pirata->Bool
tienenElMismoTesoroConValorDiferente p1 p2 = any(loTieneElOtro p2) (botin p1)

valorTesoroMasValioso ::Pirata ->Int
valorTesoroMasValioso pirata= (maximum.listaValores) (botin pirata)

adquirir ::Tesoro->Pirata->Pirata
adquirir cosa pirata= UnPirata (nombre pirata) (cosa:(botin pirata))

tesoroNoValioso::Tesoro->Bool
tesoroNoValioso t=(valor t)<100
perdioValiosos :: Pirata->Pirata
perdioValiosos pirata= UnPirata(nombre pirata) (filter (tesoroNoValioso) (botin pirata))

perdioNombre:: Pirata->String->Pirata
perdioNombre pirata obj=UnPirata( nombre pirata) (filter((/=obj).nom)(botin pirata))



--TEMPORADA DE SAQUEOS
--Formas de saqueo
saqueoValioso::Tesoro->Bool
saqueoValioso t=(valor t)>100

saqueoEspecifico::String->Tesoro->Bool
saqueoEspecifico objeto t=objeto==(nom t)

saqueoComplejo ::String->Tesoro->Bool
saqueoComplejo objeto tesoro= saqueoValioso tesoro  || saqueoEspecifico  objeto tesoro

saqueoAmoroso::Tesoro->Bool
saqueoAmoroso tesoro=False


saquear formaSaqueo tesoro pirata |formaSaqueo tesoro=adquirir tesoro pirata
                                  |otherwise=pirata


--NAVEGANDO LOS SIETE MARES
data Barco = UnBarco{nave::String, formaSaqueo::(Tesoro->Bool),tripulacion::[Pirata]} deriving Show
perla= UnBarco "Perla Negra" saqueoValioso [jack, anne]
holandes= UnBarco "Holandes Herrante" saqueoAmoroso [david]
data Isla= UnaIsla {n::String, t::Tesoro}
islaTortuga=UnaIsla "Isla Tortuga" (UnTesoro "frasco de arena" 1)
data Ciudad=UnaCiudad {ciudad::String, riquezas::[Tesoro]}
buenosAires=UnaCiudad "Buenos Aires" [UnTesoro "Oro" 123456]

--MANEJO DE LA TRIPULACION
nuevoPirata::Barco->Pirata->Barco
nuevoPirata barco pirata=UnBarco (nave barco) (formaSaqueo barco) ((tripulacion barco)++ [pirata])
sacarPirata::Barco->String->Barco
sacarPirata barco p=UnBarco(nave barco) (formaSaqueo barco) (filter((/=p).nombre)(tripulacion barco))

--ANCLAR ISLA DESHABITADA
anclarIslaDeshabitada::Isla->Barco->Barco
anclarIslaDeshabitada isla barco=UnBarco (nave barco)(formaSaqueo barco)  (map (adquirir (t isla)) (tripulacion barco))

--ATACAR CIUDAD
saqueoCiudad:: Barco-> Ciudad -> Barco
saqueoCiudad barco city= UnBarco (nave barco) (formaSaqueo barco) (zipWith(saquear (formaSaqueo barco)) (riquezas city)(tripulacion barco))

--ABORDAR OTRO BARCO
cantidadPiratas::Barco->Int
cantidadPiratas barco= length (tripulacion barco)
primeraGanadora::Barco->Barco->Bool
primeraGanadora b1 b2 =cantidadPiratas b1>cantidadPiratas b2
embarcacionGanadora embarcacion1 embarcacion2 |primeraGanadora embarcacion1 embarcacion2 = embarcacion1
                                              |primeraGanadora embarcacion2 embarcacion1 = embarcacion2
                                              |otherwise = UnBarco "EMPATE" saqueoAmoroso []

