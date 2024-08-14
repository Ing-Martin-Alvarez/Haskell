data ArbolB a = VacioB | NodoB a (ArbolB a) (ArbolB a) deriving (Show)
data ArbolBB b = VacioBB | NodoBB b (ArbolBB b) (ArbolBB b) deriving (Show)


a = 
     NodoB 10 (NodoB 15 (NodoB  24 VacioB VacioB) (NodoB  27 VacioB VacioB))
     (NodoB 18 (VacioB) (NodoB 24 VacioB VacioB))

b = 
     NodoBB 50 (NodoBB 30 (NodoBB  30 VacioBB VacioBB) (NodoBB  35 VacioBB VacioBB))
     (NodoBB 60 (VacioBB) (NodoBB 80 VacioBB VacioBB))

--Determina la raíz de un árbol binario
raizB :: ArbolB a -> a
raizB VacioB = error "RAIZ DE ARBOL VACIO"
raizB (NodoB x i d) = x

--Determina el número de hojas que tiene un árbol binario 
esNil VacioB= True 
esNil (NodoB x i d)= False

nHojas :: ArbolB a -> Int
nHojas VacioB = 0
nHojas (NodoB x i d) = 
    if esNil i && esNil d
     then 1
     else nHojas i + nHojas d

--Retorna el número de nodos del árbol
tamanoB :: ArbolB a -> Int
tamanoB VacioB = 0
tamanoB (NodoB x i d) = 1+ tamanoB i + tamanoB d

--Retorna el número de niveles del árbol, considerando que la raíz es el nivel 1
profundidadB :: ArbolB a -> Int
profundidadB VacioB = 0
profundidadB (NodoB x i d) = 1 + max (profundidadB i) (profundidadB d)

--Retorna el recorrido en entre orden de un árbol
enOrdenB :: ArbolB a -> [a]
enOrdenB VacioB= []
enOrdenB (NodoB r i d)= enOrdenB i ++ (r: enOrdenB d)

--Retorna el recorrido en pre orden de un árbol
preOrdenB :: ArbolB a -> [a]
preOrdenB VacioB= []
preOrdenB (NodoB r i d)=r:(preOrdenB i ++ preOrdenB d)

--Retorna el recorrido en post orden de un árbol
postOrdenB :: ArbolB a -> [a]
postOrdenB VacioB= []
postOrdenB (NodoB r i d)= postOrdenB i ++ postOrdenB d ++ [r]

--Realiza la suma de todos los nodos del árbol utilizando funciones de orden superior
sumaArbolB VacioB = 0
sumaArbolB (NodoB x i d) = sumar (sumaArbolB i) x (sumaArbolB d)
  where
   sumar x y z = x + y + z

--Retorna True si cumple con ser un árbol de búsqueda y False si no lo es 
todosArbolBB p VacioBB = True
todosArbolBB p (NodoBB x i d)= p x && todosArbolBB p i && todosArbolBB p d

esArbolBB VacioBB = True
esArbolBB (NodoBB x i d) = todosArbolBB (<=x) i && todosArbolBB(>x) d && esArbolBB i && esArbolBB d 

--Retorna True si el valor se encuentra en el árbol binario de búsqueda
perteneceBB valor VacioBB = False
perteneceBB valor (NodoBB x i d)
   | valor == x = True
   | valor < x = perteneceBB valor i
   | otherwise = perteneceBB valor d

--Inserta el valor en el árbol binario de búsqueda 
insertarBB valor VacioBB = NodoBB valor VacioBB VacioBB
insertarBB valor (NodoBB x i d) 
  | valor <= x = NodoBB x (insertarBB valor i) d
  | otherwise = NodoBB x i (insertarBB valor d)

--Construye un árbol de búsqueda a partir de una lista de valores 
listaAArbolBB lista = foldr insertarBB VacioBB lista 

--Obtiene una lista ordenada aplicando el recorrido de arboles
enOrdenBB VacioBB= []
enOrdenBB (NodoBB r i d)= enOrdenBB i ++ (r: enOrdenBB d)

arbolOrdenado lista = enOrdenBB.listaAArbolBB 