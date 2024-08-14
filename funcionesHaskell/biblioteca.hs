length' xs = sum[1|_<-xs]

length´ xs = sum[1|_<-xs]

eliminaMin st = [c|c <-st, c `elem`['A'..'Z']]

eliminaMay st = [c|c <-st, c `elem`['a'..'z']]

eliminaSim st = [c|c <-st, c `elem`['A'..'z']]

--zip -- conjunto de pares ordenados

--trd(1,2,3) --funcion que independientemente te devuelva el 3er valor de la tripleta

--que triangulo recto cuyos lados miden enteros menores que 10 tienen un perimetro igual a 24.
--triangulo recto [(6,8,10),(8,6,10)]

--let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
--let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
--let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
--rightTriangles'
--[(6,8,10)]

--soloMay :String -> String
--soloMay st = [c|c<-st,c`elem`['A'..'Z']]

--perimetroTrian: int->int->int->int
--perimetroTrian d = a+b+c

--let triangulorecto = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2, a+b+c == 24]

trd xs = case xs of (_,_,result) -> result -- Funcion que extrae el tercer valor de una tripleta
