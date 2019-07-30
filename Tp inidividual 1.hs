import Text.Show.Functions

data Pirata=UnPirata{apodo::String, botin::[Tesoro]} deriving Show
instance Eq Pirata where
 (==) p1 p2= (apodo p1)==(apodo p2)


jack=UnPirata "Jack" [(UnTesoro "brujula" 10000),(UnTesoro "frasco" 0)]
david=UnPirata "David" [(UnTesoro "cajita" 1)] 
anne=UnPirata "Anne" [(UnTesoro "doblones" 100),(UnTesoro "frasco" 1)]

cuantosTesorosTiene::Pirata->Int
cuantosTesorosTiene= length.botin

valorBotin::Pirata->Float
valorBotin= sum.(map(valor)).botin

afortunado::Pirata->Bool
afortunado=(>10000).valorBotin

tienenElMismoDiferenteValor::Pirata->Pirata->Bool
tienenElMismoDiferenteValor p1 p2=any(loTieneConOtroValor p2) (botin p1)

loTieneConOtroValor::Pirata->Tesoro->Bool
loTieneConOtroValor pirata t= any(\tesoro-> (nombre tesoro)==(nombre t) && (valor tesoro)/=(valor t)) (botin pirata)

esElMasValioso::[Tesoro]->Tesoro->Bool
esElMasValioso tesoros tesoro= all(\t-> (valor t)<(valor tesoro)) tesoros

tesoroMasValioso::Pirata-> Tesoro
tesoroMasValioso pirata =(head.(filter(esElMasValioso (botin pirata)))) (botin pirata)

valorTesoroMasValioso::Pirata->Float
valorTesoroMasValioso= valor.tesoroMasValioso

adquirir::Tesoro->Pirata->Pirata
adquirir tesoro pirata=pirata{botin=tesoro:(botin pirata)}

tesoroValioso::Tesoro->Bool
tesoroValioso=(>100).valor

nombreEspecifico::String->Tesoro->Bool
nombreEspecifico nombreT= (==nombreT).nombre 

perdidaEspecifica::String->Pirata->Pirata
perdidaEspecifica nombreTesoro pirata=pirata{botin=filter(not.(nombreEspecifico nombreTesoro)) (botin pirata)}

------------------------------Temporada de saqueos
type Saqueo=(Tesoro->Bool)

saqueoValioso=tesoroValioso
saqueoEspecifico=nombreEspecifico
saqueoAmoroso::Tesoro->Bool
saqueoAmoroso tesoro=False

saqueoComplejo::[Saqueo]->Tesoro->Bool
saqueoComplejo saqueos tesoro= any(\saqueo-> saqueo tesoro) saqueos

saquear::Saqueo->Pirata->Tesoro->Pirata
saquear saqueo pirata tesoro
 |saqueo tesoro=adquirir tesoro pirata
 |otherwise=pirata

--saquear jack (saqueoComplejo [saqueoValioso, (saqueoEspecifico "sombrero")]) (UnTesoro "sombrero" 1)
-----------------------------------------Navegando
data Barco=UnBarco{tripulacion::[Pirata], formaSaqueo::(Tesoro->Bool)} deriving Show
instance Eq Barco where
 (==) b1 b2= sonLosMismos (tripulacion b1) (tripulacion b2)
instance Ord Barco where
 (<=) b1 b2=(cantPiratas b1)<=(cantPiratas b2)

perla=UnBarco [jack, anne] saqueoValioso

incorporar::Pirata->Barco->Barco
incorporar pirata barco= barco{tripulacion= pirata:(tripulacion barco)}

echar::Pirata->Barco->Barco
echar pirata barco=barco{tripulacion= filter(/=pirata) (tripulacion barco)}

data Isla=UnaIsla{nombreIsla::String , tesoroIsla::Tesoro} deriving Show
islaRon= UnaIsla "Isla del ron" (UnTesoro "ron" 25)

anclar::Isla->Barco->Barco
anclar isla barco=barco{tripulacion= map(adquirir (tesoroIsla isla)) (tripulacion barco)}

data Ciudad=UnaCiudad{nombreCiudad::String, tesorosCiudad::[Tesoro]} deriving Show
buenosAires=UnaCiudad "Buenos Aires" [(UnTesoro "Mate" 15)]

atacar::Ciudad->Barco->Barco
atacar ciudad barco=barco{tripulacion = zipWith(saquear (formaSaqueo barco)) (tripulacion barco) (tesorosCiudad ciudad) }


perderTodo::Pirata->Pirata
perderTodo pirata=pirata{botin=[]}

cantPiratas=length.tripulacion

barcoAbordado::Barco->Barco->Barco
barcoAbordado atacante victima
 |(cantPiratas atacante)>(cantPiratas victima)= victima{tripulacion=map(perderTodo) (tripulacion victima)}
 |otherwise=victima

---------------------------------------------------------------SEGUNDA PARTE
importeNominal=1000
data Tesoro=UnTesoro String Float| Bono [Float] | LeLiq String deriving Show

nombre::Tesoro->String
nombre (UnTesoro nombre _)=nombre
nombre (Bono _)="Bono"
nombre (LeLiq _)="LeLiq"
valor::Tesoro->Float
valor (UnTesoro _ valor)= valor
valor (Bono valores)= ((maximum valores) - (minimum valores))*1.5
valor (LeLiq pais)= dependeDelPais pais

dependeDelPais "argentina"=importeNominal*1.74

saqueoBuitre::Tesoro->Bool
saqueoBuitre=(=="Bono").nombre

saqueoFobico::String->Tesoro->Bool
saqueoFobico nombre= not.(saqueoEspecifico nombre)

---------------------------------Universidad pirata
type Universidad=(Barco->Barco)
uade barco=barco{formaSaqueo=not.(formaSaqueo barco)}
uba barco=barco{formaSaqueo=(saqueoComplejo [(formaSaqueo barco),saqueoBuitre,saqueoValioso])}
uai=id

---------------------------------------------------------
historiaDeBarco::[(Barco->Barco)]->Barco->Barco
historiaDeBarco historias barco= foldl (\barco historia->historia barco) barco historias

--historiaDeBarco [(atacar buenosAires),(anclar islaRon), (echar jack)] perla

sonLosMismos::[Pirata]->[Pirata]->Bool
sonLosMismos piratas1 piratas2= all(\pirata->elem pirata piratas1)piratas2

barcosInvencibles::(Barco->Barco)->[Barco]->[Barco]
barcosInvencibles historias barcos= filter(\barco->(historias barco)==barco) barcos

--barcosInvencibles (historiaDeBarco [(atacar buenosAires),(anclar islaRon), (echar jack)]) [perla]

elMejorBarco::(Barco->Barco)->[Barco]->Barco
elMejorBarco historias barcos=maximum(barcosInvencibles historias barcos)

-----------------------------------------------------------
tripulacionInfinita=UnBarco [UnPirata ("Pirata numero" ++ show n) ([UnTesoro ("Tesoro"++ show n) 5])| n<-[1..]] saqueoValioso

-----------Con una tripulacion inifnita:
----------Funciones que no devuelven nada por quedarse evaluando infinitamente: cantPiratas, barcoAbordado
----------Devulven respuesta infinita:incorporar, echar,anclar, todas las univesidades
---------devuelven respuesta finita:atacar, si la ciudad tiene una cant de objetos finita
---------Depende: historiaDeBarco, barcosInvencibles, elMejorBarco, pues dependen de que las historias que le hayan ocurrido al barco le hayan recortado la tripulacion a un numero finito. En caso contario devuelven una respuesta infinita