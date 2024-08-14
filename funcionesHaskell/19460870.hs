--Biblioteca de Funciones Recursivas - Martin Alvarez Salazar - 19460870 - TecNM Colima

--Funcion maximo de una lista con recursividad (hecho en clase)
maximun' :: (Ord a) => [a] -> a
maximun' [] = error "Maximo de una lista vacia"
maximun' [x] = x
maximun' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximun' xs

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

--Funcion que retorna un valor de la sucesion de Fibonacci (hecha en clase)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

--Funcion que retorna la sucesion de Fibonacci
listaFibonacci n = [fib n | n <- [0..(n-1)]]

--Funcion metodo de ordenacion Quicksort recursivo con where
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
      apuntadorbajo ++ [x] ++ apuntadoralto
      where apuntadorbajo = quicksort [a | a <- xs, a <= x]
            apuntadoralto = quicksort [a | a <- xs, a > x]

--Funcion que convierte de decimal a binario
base10abase2 :: Int -> [Int]
base10abase2 0 = [0]
base10abase2 n = reverse (calcBin n)

calcBin :: Int -> [Int]
calcBin 0 = []
calcBin n = n `mod` 2 : calcBin (n `div` 2)