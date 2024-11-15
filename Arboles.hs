data Arbol a = ArbolVacio | Raiz a (Arbol a)(Arbol a) deriving Show

{-Ejercicio 1-}

longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz _ izquierdo derecho) = 1 + longitud izquierdo + longitud derecho

{-Ejercicio 2-}

profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz _ izquierdo derecho) =  if longitud derecho > longitud izquierdo 
                                            then longitud derecho + 1
                                            else longitud izquierdo + 1


{-Ejercicio 3-}

ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izquierdo derecho) = ancho izquierdo + ancho derecho

{-Ejercicio 4-}

data TipoRecorrido = InOrder | PreOrder | PostOrder deriving Show

recorrido :: Arbol a -> TipoRecorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz a izquierdo derecho) InOrder = recorrido izquierdo InOrder 
                                                ++ [a] 
                                                ++ recorrido derecho InOrder
recorrido (Raiz a izquierdo derecho) PreOrder = [a] 
                                                ++ recorrido izquierdo PreOrder 
                                                ++ recorrido derecho PreOrder
recorrido (Raiz a izquierdo derecho) PostOrder = recorrido izquierdo PostOrder 
                                                ++ recorrido derecho PostOrder 
                                                ++ [a]

{-Ejercicio 5-}
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a ArbolVacio ArbolVacio) = [[a]]
niveles (Raiz a izquierdo derecho) = [a] 
                                        : combinarNiveles (niveles izquierdo) 
                                                         (niveles derecho)

combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x ++ y) : combinarNiveles xs ys

{-Ejercicio 6-}
maximo :: Arbol a -> a
maximo ArbolVacio = error "No hay elementos"
maximo (Raiz a _ ArbolVacio) = a
maximo (Raiz a _ derecho) = maximo derecho 

{-Ejercicio 7-}
minimo :: Arbol a -> a
minimo ArbolVacio = error "No hay elementos"
minimo (Raiz a ArbolVacio _) = a
minimo (Raiz a _ izquierdo) = minimo izquierdo

{-Ejercicio 8-}
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio elem = error "No existe el elemento en el arbol"
eliminar (Raiz x ArbolVacio derecho) elem =  if x == elem
                                                then derecho
                                                else eliminar derecho elem
eliminar (Raiz x izquierdo ArbolVacio) elem = if x == elem
                                                then izquierdo
                                                else eliminar izquierdo elem
eleminar (Raiz x izquierdo derecho) elem = if elem < x
                                                then(Raiz x(eliminar izquierdo elem) derecho)
                                                else if elem > x
                                                    then (Raiz x (eliminar derecho elem) izquierdo)
                                                    else (Raiz (minimo derecho) izquierdo (eliminar derecho(minimo derecho)))