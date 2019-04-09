--type pirata=(nombre,lista)
jack=("jack",[("llavero",10000),("moneda",6)])
lst=[("0",5),("0",6)]
cantidadDeTesoros (nombre,lista)=length lista
valor::(string,int)->int
valor (x,y)=y
listaValores::[(string,int)]->[int]
listaValores lista= map valor lista
pirataAfortunado::(String,[(String, Int)])->Bool
pirataAfortunado(nombre,lista)= (sum.listaValores) lista>10000

--mismoValorDif (x,y) (z,w)= x==z && y!=w


valorTesoroMasValioso ::(String,[(String, Int)]) ->Int
valorTesoroMasValioso (nombre, lista)= (maximum.listaValores) lista
adquirirTesoro (nombre,lista) t=(nombre,lista++[t])

tesoroNoValioso::(String,Int)->Bool
tesoroNoValioso (x,y)=y<100
perdioValiosos (nombre,lista)= (nombre,filter (tesoroNoValioso) lista)

nombreDado::(String, Int)-> String-> Bool
nombreDado (x,y) z= x==z
perdioNombre::(String,[(String, Int)])->String->(String,[(String, Int)])
--(filter(!=(nom,- )))lista
--perdioNombre (nombre,lista) nom=(nombre, filter(!=(nom,-))lista)

tesoroValioso (x,y)=y>100
filtrarValioso::(String, Int)-> [(String, Int)]
filtrarValioso t=filter( tesoroValioso)[t]
saquear:: String-> (String,[(String, Int)])->(String,Int)-> (String,[(String, Int)])
saquear "valor" (nombre,lista) tesoro = (nombre,lista ++ (filtrarValioso tesoro))

saquear "amor" (nombre,lista) tesoro= (nombre,lista)
--tesoroCorrecto (x,y) z= x==z
--filtrarTesoroCorrecto t obj= filter(tesoroCorrecto t obj)[t]
--saquear objeto (nombre,lista) tesoro= (nombre,lista ++ (filtrarTesoroCorrecto tesoro objeto))
