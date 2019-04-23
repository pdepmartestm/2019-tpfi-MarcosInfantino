import Text.Show.Functions
--type pirata=(nombre,lista)
--tripulacion (saqueo, [pirata])
jack=("jack",[("llavero",10000),("moneda",6)])
lst=[("0",5),("0",6)]
cantidadDeTesoros (nombre,lista)=length lista
valor::(string,int)->int
valor (x,y)=y
listaValores::[(string,int)]->[int]
listaValores lista= map valor lista
pirataAfortunado::(String,[(String, Int)])->Bool
pirataAfortunado(nombre,lista)= (sum.listaValores) lista>10000


esMismoTesoroConValorDistinto (nombre1, valor1) (nombre2, valor2)= (nombre1==nombre2)&&(valor1/=valor2)
loTieneOtroPirataConDistintoValor (_,botin) tesoro=any(esMismoTesoroConValorDistinto tesoro) botin
tienenElMismoTesoroConValorDiferente (_,botin) otroPirata= any(loTieneOtroPirataConDistintoValor otroPirata) botin



valorTesoroMasValioso ::(String,[(String, Int)]) ->Int
valorTesoroMasValioso (nombre, lista)= (maximum.listaValores) lista
adquirirTesoro (nombre,lista) t=(nombre,lista++[t])

tesoroNoValioso::(String,Int)->Bool
tesoroNoValioso (x,y)=y<100
perdioValiosos (nombre,lista)= (nombre,filter (tesoroNoValioso) lista)


perdioNombre::(String,[(String, Int)])->String->(String,[(String, Int)])
perdioNombre (nombre,lista) nom=(nombre, filter((/=nom).fst)lista)







--formas de saqueo
saqueoValioso (nom,valor)=valor>100
saqueoEspecifico objeto (nom,valor)=objeto==nom
saqueoComplejo objeto tesoro= saqueoValioso tesoro  || saqueoEspecifico  objeto tesoro
saqueoAmoroso tesoro=False
saquear formaSaqueo tesoro (nom, lista) = (nom, lista ++ filter (formaSaqueo) [tesoro] )

saquear formaSaqueo tesoro pirata 
  | formaSaqueo tesoro = adquirir tesoro pirata
  | otherwise = pirata


tripulantes(barco, saqueo,listaPiratas)=listaPiratas
--tripulacion=(nombre,formaSaqueo,pirata)
--tripulacion= ("perla negra",saqueoValioso,[("jack",[("llavero",10000),("moneda",6)])])
nuevoPirata (nombre, saqueo, listaPiratas) pirata=(nombre, saqueo, listaPiratas ++ [pirata])
sacarPirata (nombre, saqueo, listaPiratas) nom=(nombre, saqueo, filter((/=nom).fst))

adquirir cosa (nombre, lista)=(nombre, cosa:lista)
--adquirirRon (nombre,lista) =(nombre, lista ++ [("Ron",25)])

anclarIslaDeshabitada (_,tesoro) (nombre, saqueo , listaPiratas)=(nombre,saqueo,  map (adquirir tesoro) listaPiratas)

--ciudad=(ciudad, listaTesoros)

saqueoCiudad (nombre, formaSaqueo,listaPiratas) (ciudad, listaTesoros)= (nombre, formaSaqueo, zipWith(saquear formaSaqueo) listaTesoros listaPiratas)

cantidadPiratas (nombre, formaSaqueo, listaPiratas)= length listaPiratas

primeraM b1 (nombre2, formaSaqueo2, listaPiratas2) =(length listaPiratas1)>(length listaPiratas2)
primeraM (nombre1,formaSaqueo1,listaPiratas1) (nombre2, formaSaqueo2, listaPiratas2) =(length listaPiratas1)>(length listaPiratas2)
--segundaM [(nombre1,formaSaqueo1,listaPiratas1),(nombre2, formaSaqueo2, listaPiratas2)]=(length listaPiratas1)<(length listaPiratas2)
--empatadas [(nombre1,formaSaqueo1,listaPiratas1),(nombre2, formaSaqueo2, listaPiratas2)]=(length listaPiratas1)==(length listaPiratas2)


embarcacionGanadora embarcacion1 embarcacion2 |primeraM embarcacion1 embarcacion2 = embarcacion1
                                              |primeraM embarcacion2 embarcacion1 = embarcacion2
                                              |otherwise =("EMPATE",saqueoAmoroso,[])
