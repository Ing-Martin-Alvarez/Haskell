import Data.Char(digitToInt)
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
tercero (x, y, z) = z
trdTres (_, _, z) = z 

--Funcion que retorna todos los triangulos posibles
triangulosRectangulos = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a+b+c ==24]  

--Funcion que retorna los dias de la semana
diaSem :: Int -> String
diaSem s = "El numero " ++ show(s) ++ " corresponde al dia de la semana "++ case s of 
        {1  -> "Lunes.";
        2  -> "Martes.";
        3  -> "Miercoles.";
        4  -> "Jueves.";
        5  -> "Viernes.";
        6  -> "Sabado.";
        7  -> "Domingo.";
        s  -> "Error.";} 

--Funcion que retorna el factorial de un numero
fact :: (Integral a) => a -> a
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

--Funcion que retorna la suma de vectores
sumaVectores :: (Num a) => (a, a)-> (a, a) -> (a, a)
sumaVectores (x1,x2)(y1,y2) = (x1+y1,x2+y2)

--Funcion head
head' :: [a] -> a
head' [] = error "No puedes utilizar head con listas vacias!!"
head' (x:_) = x

--Funcion maximo de una lista con recursividad
maximun' :: (Ord a) => [a] -> a
maximun' [] = error "Maximo de una lista vacia"
maximun' [x] = x
maximun' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximun' xs

--Funcion que retorna un valor de la sucesion de Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

--Funcion que retorna la sucesion de Fibonacci
listaFibonacci n = [fib n | n <- [0..n]]

--Funcion que cambia los numeros de 0 por 1 y unos por cero de un nuero binario
complementoUno [] = "Esta vacio"
complementoUno ['1'] = ['0']
complementoUno ['0'] = ['1']
complementoUno ('1':xs) = ['0']++(complementoUno xs)
complementoUno ('0':xs) = ['1']++(complementoUno xs)

--Funcion replicate con recursividad
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n v
    | n <= 0    = []
    | otherwise = v:replicate' (n-1) v

--Funcion take con recursividad
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

--Funcion reverse con recursividad
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--Funcion elem con recursividad
elem' :: (Eq a) => a -> [a] -> Bool
elem' v [] = False
elem' v (x:xs)
    | v == x    = True
    | otherwise = v `elem'` xs

--Funcion metodo de ordenacion Quicksort con recursividad
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let apuntadorbajo = quicksort [a | a <- xs, a <= x]
        apuntadoralto = quicksort [a | a <- xs, a > x]
    in  apuntadorbajo ++ [x] ++ apuntadoralto

--Funcion metodo de ordenacion Quicksort recursivo con where
quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x:xs) =
      apuntadorbajo ++ [x] ++ apuntadoralto
      where apuntadorbajo = quicksort2 [a | a <- xs, a <= x]
            apuntadoralto = quicksort2 [a | a <- xs, a > x]

--Funcion que convierte de binarios a decimales
convierte2a10 n = sum[a*b|(a,b)<-zip[digitToInt d|d<-reverse(show n)][2^p|p<-[0..length(show n)]]]

--Funcion que convierte de decimal a binario
base10abase2, calcBin :: Int -> [Int]
base10abase2 0 = [0]
base10abase2 n = reverse (calcBin n)

calcBin 0 = []
calcBin n = n `mod` 2 : calcBin (n `div` 2)

--Funcion que define un arbol binario
data Arbol a = Hoja
           | Nodo a (Arbol a)(Arbol a)
           deriving (Show, Eq)

--Creacion de un arbol
arbol1 = Nodo 24(Nodo 36
                         (Nodo 96 Hoja Hoja)
                         (Nodo 110
                                 (Hoja)(Nodo 15 Hoja Hoja)))
                 (Nodo 28 
                         (Nodo 20 Hoja Hoja)
                         (Nodo 16
                                (Nodo 0 Hoja Hoja)(Hoja)))

--Funcion que permite obtner la raiz de un arbol
raizA Hoja         = error "Arbol Vacio"
raizA (Nodo x _ _) = x

--https://es.slideshare.net/JoseAAlonso/definiciones-por-recursin-en-haskell
--https://haskellhero-es.grifart.cz/index.php?page=lessons&lesson=73
--https://www.glc.us.es/~jalonso/vestigium/i1m2013-ejercicios-de-arboles-binarios-en-haskell/