--Biblioteca de Funciones Haskell - Martin Alvarez Salazar - 19460870 - TecNM Colima
-- Potencia, esta funcion saca la potencia de un numero
triple x = x * x * x

-- Añade un elemento al final de una lista
turnList x = x : []
elenfin x y = y ++ turnList x

elenfin1 x y = y ++ x:[]
elenfin2 x y = y ++ [x]

-- Obtiene elemento en medio si es impar, de lo contrario se regresa una lista vacia 
enmedio x = if length x `mod` 2 == 0
               then []
               else [x !! (length x `div` 2)]

enmedio2 x = if length x `mod` 2 == 0
               then []
               else [x !! div (length x) 2]

--Ingresar un elemento en medio del elemento
insElemC x y = if length x `mod` 2 == 0
                then take (length x `div` 2) x ++ [y] ++ drop (length x `div` 2) x
                else take (length x `div` 2) x ++ [y] ++ drop (length x `div` 2) x

--Ingresar un elemento en cualquier posicion de la lista
insElem x y z = if y > length x
                 then []
                 else take y x ++ [z] ++ drop y x

--Ejemplos de clase
--Listas
pantalon = ["negro","azul","jean"]
camisa = ["uniforme","formal","casual"]
--Funcion para obtener la longitud de una cadena
length' xs = sum[1 | _ <- xs]
--Funcion para eliminar las letras minusculas de una lista
eliminaMin st = [c | c<-st, c `elem` ['A'..'Z']]
--Funcion para eliminar las letras mayusculas de una lista
eliminaMay st = [c | c<-st, c `elem` ['a'..'z']]
--Funcion para eliminar los simbolos que no sean letras de una lista
eliminaSim st = [c | c<-st, c `elem` ['A'..'z']]

--Funciones, funciones where, let, case
--Funcion que retorna un texto de acuerdo al índice de masa corporal
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Estas bajo de peso"
    | weight / height ^ 2 <= 25.0 = "Estas de peso normal"
    | weight / height ^ 2 <= 30.0 = "Tienes un poco de sobrepeso"
    | otherwise                   = "Estas obeso chaval"

--Funcion que retorna si el primer valor es mayor, menor o igual que el segundo
miCompara :: (RealFloat a) => a -> a -> String
miCompara x y
    | x > y     = "El primer numero es mayor que el segundo - GT"
    | x < y     = "El primer numero es menor que el segundo - LT"
    | otherwise = "Los numeros son iguales - EQ"

--Funcion que retorna las raices de una ecuacion cuadratica ax^2+bx+c
raicesEcuCuad :: Float -> Float -> Float -> (Float,Float,String)
raicesEcuCuad a b c 
    | discriminante < 0 = (0,0,"i")
    | otherwise = ((-b + raiz)/denominador,(-b - raiz)/denominador,"r")
  where
    discriminante = ((b * b) - (4 * a * c))
    raiz = sqrt (discriminante)
    denominador = (2 * a) 

--Funcion que retorna el IMC por cada una de las duplas
calBmis :: (RealFloat a) => [(a, a)]->[a]
calBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

--Funcion que retorna el area de un cilindro
aCilindro :: (RealFloat a) => a -> a -> a
aCilindro r h =
    let lateral = 2 * pi * r * h
        circulo = pi * r ^2
    in  lateral + 2 * circulo

--Funcion que retorna el area tronco de un cono
aTroncoCono :: (RealFloat a) => a -> a -> a -> a
aTroncoCono rs ri g = 
    let circulosuperior = pi * rs ^2
        circuloinferior = pi * ri ^2
        lateral = pi * (ri + rs) * g
    in  lateral + circuloinferior + circulosuperior

--Funcion que describe si la lista es vacia, unitaria o larga
describeLista :: [a] -> String
describeLista xs = "La lista es " ++  case xs of []  -> "una lista vacia."
                                                 [x] -> "una lista unitaria."
                                                 xs  ->  "una lista larga."

--Funcion que retorna un texto con el nombre y el numero del mes 
mesAnio :: Int -> String
mesAnio m = "El mes numero " ++ show(m) ++ " corresponde al mes de "++ case m of 
        {1  -> "Enero.";
        2  -> "Febrero.";
        3  -> "Marzo.";
        4  -> "Abril.";
        5  -> "Mayo.";
        6  -> "Junio.";
        7  -> "Julio.";
        8  -> "Agosto.";
        9  -> "Septiembre.";
        10 -> "Octubre.";
        11 -> "Noviembre.";
        12 -> "Diciembre.";
        m  -> "Error.";}                      

--Funcion que retorna el ultimo valor de un tripleta
trd xs = case xs of (_,_,result) -> result              