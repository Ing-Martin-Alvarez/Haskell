import Data.Char(digitToInt)

--Funcion que convierte de Binario a Decimal
convOctDecHex :: [Int] -> Int
convOctDecHex [] = 0
convOctDecHex (x:xs) = x + 2 * (convOctDecHex xs)

--Funcion que cuenta cuantas letras, simbolos y numeros hay en una lista
--queHay [] = "Esta vacio"
--queHay st = [a | a<-st, a `elem` ['A'..'z']]
--queHay st = [b | b<-st, b `elem` ['0'..]]

--Funcion que cambia los numeros de 0 por 1 y unos por cero de un nuero binario
complementoUno [] = "Esta vacio"
complementoUno ['1'] = ['0']
complementoUno ['0'] = ['1']
complementoUno ('1':xs) = ['0']++(complementoUno xs)
complementoUno ('0':xs) = ['1']++(complementoUno xs)

--Funcion que convierte de Binario a Decimal
convierte2a10 n = sum[a*b|(a,b)<-zip[digitToInt d|d<-reverse(show n)][2^p|p<-[0..length(show n)]]]

--data NombreTipo variable = vacio
--                         |Nodo variable (NombreTipo variable)(NombreTipo variable)
--                         deriving(Show, Eq)
--Ejemplo
data Arbol a = Hoja
           | Nodo a (Arbol a)(Arbol a)
           deriving (Show, Eq)

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

--https://haskellhero-es.grifart.cz/index.php?page=lessons&lesson=73