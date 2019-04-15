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

mismoValorDif (x,y) (z,w)= x==z && y/=w


valorTesoroMasValioso ::(String,[(String, Int)]) ->Int
valorTesoroMasValioso (nombre, lista)= (maximum.listaValores) lista
adquirirTesoro (nombre,lista) t=(nombre,lista++[t])

tesoroNoValioso::(String,Int)->Bool
tesoroNoValioso (x,y)=y<100
perdioValiosos (nombre,lista)= (nombre,filter (tesoroNoValioso) lista)


perdioNombre::(String,[(String, Int)])->String->(String,[(String, Int)])
perdioNombre (nombre,lista) nom=(nombre, filter((/=nom).fst)lista)

tesoroValioso (x,y)=y>100
filtrarValioso::(String, Int)-> [(String, Int)]
filtrarValioso t=filter( tesoroValioso)[t]
--saquear:: String-> (String,[(String, Int)])->(String,Int)-> (String,[(String, Int)])
saquear "valor" (nombre,lista) tesoro = (nombre,lista ++ (filtrarValioso tesoro))

saquear "amor" (nombre,lista) tesoro= (nombre,lista)

saquear objeto (nombre,lista) tesoro= (nombre,lista ++ (filter((==objeto).fst)[tesoro]))

--saquear ("amor","valor") (nombre,lista) tesoro = (nombre,lista ++ (filtrarValioso tesoro))
--saquear  "valor" "amor" (nombre,lista) tesoro = (nombre,lista ++ (filtrarValioso tesoro))
--saquear "amor" objeto (nombre,lista) tesoro= (nombre,lista ++ (filter((==objeto).fst)[tesoro]))
--saquear "valor" objeto (nombre,lista) tesoro=(nombre,lista ++[ fst ((filter((==objeto).fst)[tesoro])++ (filtrarValioso tesoro))])

nuevoPirata (saqueo, listaPiratas) pirata=(saqueo, listaPiratas ++ [pirata])
sacarPirata (saqueo, listaPiratas) nom=(saqueo, filter((/=nom).fst))