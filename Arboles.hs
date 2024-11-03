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