--Instituto Tecnologico de Colima
--Materia: Programacion Logica y Funcional
--Docente: Ma. Elena Martinez Duran
--Alumno: Martin Alvarez Salazar
--No.Cuenta: 19460870
--Funcion que define un arbol binario
data Arbol a = Hoja
           | Nodo a (Arbol a)(Arbol a)
           deriving (Show, Eq)

--Creacion de un arbol1
arbol1 = Nodo 24(Nodo 36
                         (Nodo 96 Hoja Hoja)
                         (Nodo 110
                                 (Hoja)(Nodo 15 Hoja Hoja)))
                 (Nodo 28 
                         (Nodo 20 Hoja Hoja)
                         (Nodo 16
                                (Nodo 0 Hoja Hoja)(Hoja)))

--Creacion de un arbol2
arbol2 = Nodo 10 (Nodo 15
                         (Nodo 24 Hoja Hoja)
                         (Nodo 27 Hoja Hoja))
                (Nodo 18 
                         (Hoja)(Nodo 24 Hoja Hoja))

--Creacion de un arbol3
arbol3 = Nodo 50 (Nodo 30 
                         (Nodo 30 Hoja Hoja) 
                         (Nodo 35 Hoja Hoja))
                 (Nodo 60 
                         (Hoja) (Nodo 80 Hoja Hoja))

--Creacion de un arbolp
arbolp = Nodo 52 (Nodo 36 
                         (Nodo 67 Hoja Hoja) 
                         (Nodo 22 
                                  (Nodo 5 Hoja Hoja)
                                  (Nodo 2 Hoja Hoja)))
                 (Nodo 25 
                         (Nodo 37 
                                 (Hoja) 
                                 (Nodo 4 Hoja Hoja)) 
                         (Nodo 6 Hoja Hoja))

--Creacion arbolpOrdenado
arbolpOrdenado = Nodo 6 (Nodo 4 
                               (Nodo 2 Hoja Hoja) 
                               (Nodo 5 Hoja Hoja)) 
                        (Nodo 25 
                               (Nodo 22 Hoja Hoja) 
                               (Nodo 37 
                                       (Nodo 36 Hoja Hoja) 
                                       (Nodo 52 Hoja (Nodo 67 Hoja Hoja))))

--Funcion que permite obtner la raiz de un árbol
raizB Hoja         = error "Arbol Vacio"
raizB (Nodo r _ _) = r

--Funcion que permite obtener el numero de hojas de un árbol
--nHojas :: Arbol a -> Int
--nHojas Hoja         = 1
--nHojas (Nodo r i d) = nHojas i + nHojas d

esNil Hoja = True 
esNil (Nodo x i d)= False

nHojas :: Arbol a -> Int
nHojas Hoja = 0
nHojas (Nodo x i d) = 
    if esNil i && esNil d
     then 1
     else nHojas i + nHojas d

--Funcion que permite obtener el numero de nodos de un árbol
tamañoB :: Arbol a -> Int
tamañoB Hoja         = 0
tamañoB (Nodo r i d) = 1 + tamañoB i + tamañoB d

--Funcion que permite obtener la profundidad de un árbol
profundidadB :: Arbol a -> Int
profundidadB Hoja = 0
profundidadB (Nodo x i d) = 1 + max (profundidadB i) (profundidadB d)

--Funcion que devuelve la lista de nodos en orden
enOrdenB :: (Ord a) => Arbol a -> [a]
enOrdenB Hoja = []
enOrdenB (Nodo r i d) = enOrdenB i ++ [r] ++ enOrdenB d

--Funcion que retorna el recorrido en pre orden de un árbol
preOrdenB :: Arbol a -> [a]
preOrdenB Hoja         = []
preOrdenB (Nodo r i d) = r : (preOrdenB i ++ preOrdenB d)

--Funcion que retorna el recorrido en post orden de un árbol
postOrdenB :: Arbol a -> [a]
postOrdenB Hoja         = []
postOrdenB (Nodo r i d) = postOrdenB i ++ postOrdenB d ++ [r]

--Funcion que realiza la suma de todos los nodos del árbol utilizando funciones de orden superior
sumaArbolB :: Arbol Integer -> Integer
sumaArbolB Hoja = 0
sumaArbolB (Nodo r i d) = sumar r (sumaArbolB i) (sumaArbolB d)
    where
        sumar x y z = x + y + z

--Funcion que retorna True si cumple con ser un árbol de búsqueda y False si no lo es
todosArbolBB p Hoja = True
todosArbolBB p (Nodo x i d)= p x && todosArbolBB p i && todosArbolBB p d

esArbolBB Hoja = True
esArbolBB (Nodo x i d) = todosArbolBB (<=x) i && todosArbolBB(>x) d && esArbolBB i && esArbolBB d 

--Funcion que regresa si el elemento pertenece al arbol o no
perteneceBB :: (Ord a) => a -> Arbol a -> Bool
perteneceBB x Hoja = False
perteneceBB x (Nodo r i d)
      | x == r = True      
      | x < r = perteneceBB x i
      | otherwise = perteneceBB x d

--Funcion que inserta un valor al arbol
insertarBB :: (Ord a) => a -> Arbol a -> Arbol a
insertarBB x Hoja = Nodo x Hoja Hoja
insertarBB x (Nodo r i d)
         | x <= r = Nodo r (insertarBB x i) d
         | otherwise = Nodo r i (insertarBB x d)

--Funcion que construye un árbol de búsqueda a partir de una lista de valores 
listaAArbolBB :: (Ord a) => [a] -> Arbol a
listaAArbolBB = foldr insertarBB Hoja

--Funcion que obtiene una lista ordenada aplicando el recorrido de arboles
arbolOrdenado :: (Ord a) => [a] -> [a]
arbolOrdenado = enOrdenB . listaAArbolBB