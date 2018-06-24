;;------------------------------------------------------------------------------
;;
;; Universidad de Costa Rica
;; Escuela de Ciencias de la Computación e Informática
;; CI-1441 Paradigmas Computacionales
;; I-2018, Prof. Dr. Alvaro de la Ossa Osegueda
;;
;;------------------------------------------------------------------------------
;;
;; lablisp1.lisp -- Laboratorio de LISP, parte 1
;;                  Definiciones de funciones de ejemplo de Lisp
;;
;;------------------------------------------------------------------------------
;;
;; A. Funciones de manejo/procesamiento de listas:
;;
;;    (tamano l)          -> cantidad de elementos de la lista l
;;
;;    (nesimo n l)        -> n-esimo elemento de la lista l
;;
;;    (miembro x l)       -> t si x es elemento de la lista l, nil si no
;;
;;    (miembro* x l)      -> la sublista de l en la que aparece x a la cabeza,
;;                           nil si no
;;
;;    (maximo l)          -> el maximo elemento en la lista de numeros l
;;
;;    (maximo* l)         -> auxiliar de maximo que incluye una variable
;;                           temporal para llevar cuenta del maximo
;;
;;    (concatena l1 l2)   -> la concatenacion de l1 y l2
;;
;;    (reemplaza x y l)   -> la lista que resulta de reemplazar en la lista l
;;                           todas las ocurrencias de x por y
;;
;;    (reversa l)         -> la lista reversa de la lista l
;;
;;    (posicion x l)      -> la posicion de x en l, si x esta en l, y n+1 si no,
;;                           donde n es el tamaño de la lista.
;;
;;    (posicion* x l)     -> la posicion de x en l, si x esta en l, y 0 si no.
;;
;;    (posicion** x l n)  -> auxiliar de posicion* que recibe como tercer
;;                           argumento (n) la longitud o tamano de l
;;
;;    (posiciones x l)    -> lista con las posiciones de x en l
;;
;;    (posiciones* x l i) -> auxiliar de posiciones que recibe como tercer
;;                           argumento (i) las posiciones ya recorridas en l
;;
;;------------------------------------------------------------------------------
;;
;; B. Funciones que usan operadores aritmeticos:
;;
;;    (max* x y)          -> el maximo de x y y
;;
;;    (conviertetemp temp s)
;;                        -> si s es 'f convierte temp a Fahrenheit,
;;                           si s es 'c convierte temp a Celsius
;;
;;    (fact n)            -> el factorial de n
;;
;;------------------------------------------------------------------------------
;;
;; C. Funciones de procesamiento de listas con uso de operadores aritmeticos:
;;
;;    (sumalista l)       -> la suma de los elementos de la lista l
;;
;;    (sumatoria l)       -> expresion simbolica que representa la suma de los
;;                           elementos de l
;;
;;    (sumatoria* l)      -> expresion simbolica que representa la suma de los
;;                           elementos de l, sin el primero operador '+
;;
;;------------------------------------------------------------------------------
;;
;; D. Ejemplo: Las torres de Hanoi
;;
;;    (hanoi n i d m)     -> resuelve el problema de las torres de Hanoi
;;                           pasando los n discos de la torre inicial (i) a la
;;                           torre de destino (d) pasando por una torre
;;                           intermedia (m)
;;    (hanoi* n)          -> version que usa sus propios nombres de torres
;;    (decir t1 t2)       -> auxiliar de hanoi y hanoi2 que muestra un paso
;;                           de la solucion, de la torre t1 a la t2
;;
;;------------------------------------------------------------------------------
;;
;; E. La negacion en LISP
;;
;;    (negacion e)        -> t si e devuelve nil y nil si e devuelve
;;                           cualquier otro valor
;;
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; A. Funciones de manejo/procesamiento de listas:
;;------------------------------------------------------------------------------


;;--- (tamano l) -> tamano de la lista l (numero) ------------------------------
;;
;;--- Ejemplo: (tamano '(a b c)) -> 3
;;
;;--- Errores: 1. el argumento no es una lista

(defun tamano (l)
   (cond ((null l) 0)
         (t (+ 1 (tamano (cdr l))))))


;;--- (nesimo n l) -> el n-esimo elemento de la lista l (objeto) ---------------
;;
;;--- Ejemplo: (nesimo 3 '(a b c d e)) -> c
;;
;;--- Errores: 1. el primer argumento no es numerico
;;             2: la lista no tiene tantos elementos

(defun nesimo (n l)
   (cond ((eq n 1) (car l))
         (t (nesimo (- n 1) (cdr l)))))


;;--- (miembro x l) -> t si x es miembro de l, nil si no (booleano) ------------
;;
;;--- Ejemplos: (miembro 'c '(a b c d e)) -> t
;;              (miembro '(a b) '(c (d e f) (a b) (x y) z)) -> t
;;              (miembro 'x '(a b c)) -> nil
;;
;;--- Errores: 1. l no es una lista

(defun miembro (x l)
   (cond ((null l) nil)
         ((equal x (car l)) t)
         (t (miembro x (cdr l)))))


;;--- (miembro* x l) -> la sublista de l a partir de x, ------------------------
;;                      si x es miembro de l, nil si no (lista)
;;
;;--- Ejemplos: (miembro* 'c '(a b c d e)) -> (c d e)
;;              (miembro* '(a b) '(c (d e f) (a b) (x y) z)) -> ((a b) (x y) z)
;;              (miembro* '(a b) '(c (d e f) (x y) z)) -> nil
;;
;;--- Errores: 1. l no es una lista

(defun miembro* (x l)
   (cond ((null l) nil)
         ((equal x (car l)) l)
         (t (miembro* x (cdr l)))))


;;--- (maximo l) -> el elemento maximo de la lista L (numero) ------------------
;;
;;--- Ejemplo: (maximo '(10 40 52 115 44 -23)) -> 115
;;
;;--- Errores: 1. l no es una lista
;;             2. elemento no numerico en l

(defun maximo (l)
   (maximo* 0 l))

(defun maximo* (m l)
   (cond ((null l) m)
         ((> (car l) m) (maximo* (car l) (cdr l)))
         (t (maximo* m (cdr l)))))


;;--- (concatena l1 l2) -> la concatenacion de las listas l1 y l2 (lista) ------
;;
;;--- Ejemplo: (concatena '(a b c) '(d e)) -> (a b c d e)
;;
;;--- Errores: 1. l1 o l2 no son una lista

(defun concatena (l1 l2)
   (cond ((null l1) l2)
         (t (cons (car l1)
                  (concatena (cdr l1) l2)))))


;;--- (reemplaza viejo l nuevo) -> la lista que resulta de reemplazar todas ----
;;                                 las instancias de viejo con nuevo en l ------
;;
;;--- Ejemplos: (reemplaza 'a '(a b a c a d) 'x) -> (x b x c x d)
;;              (reemplaza '(x y) '(a b (x y) m) 'z) -> (a b z m)
;;
;;--- Errores: 1. l no es una lista

(defun reemplaza (viejo l nuevo)
   (cond ((null l) nil)
         ((equal viejo (car l)) (cons nuevo
                                      (reemplaza viejo (cdr l) nuevo)))
         (t (cons (car l)
                  (reemplaza viejo (cdr l) nuevo)))))


;;--- (reversa l) -> el resultado de invertir los elementos de l ---------------
;;
;;--- Ejemplo: (reversa '(a b c)) -> (c b a)
;;
;;--- Errores: 1. l no es una lista

(defun reversa (l)
   (cond ((null l) nil)
         (t (append (reversa (cdr l))
                    (cons (car l) nil)))))


;;--- (posicion x l) -> la posicion de la primera instancia del objeto x -------
;;                      en la lista l.
;;
;;--- Ejemplo: (posicion 'a '(b c a d g a a z)) -> 3
;;
;;--- Errores: 1. k es no es una lista

;;--- Version que devuelve la posicion, si x esta en l, y n+1 si no, donde n
;;    es el tamano de la lista.
;;
;;    Ejemplos: (posicion 'x '(b (c x) x a)) -> 3
;;              (posicion 'x '(b c a d g a a z)) -> 9

(defun posicion (x l)
   (cond ((null l) 0)
         ((eq (car l) x) 1)
         ((null (cdr l)) 2)
         (t (+ 1 (posicion x (cdr l))))))

;;--- Version que devuelve la posicion, si x esta en l, y 0 si no.
;;    Ineficiente porque tiene que recorrer la lista dos veces.
;;
;;    Ejemplo: (posicion* 'x '(b c a d g a a z)) -> 0

(defun posicion* (x l) (posicion** x l (length l)))

(defun posicion** (x l n)
   (let ((p (posicion x l)))
      (cond ((> p n) 0)
            (t p))))


;;--- (posiciones x l) -> las posiciones de las instancias del objeto x --------
;;                        en la lista l
;;
;;--- Ejemplos: (posiciones 'a '(b c a d g a x a z)) -> (3 6 8)
;;              (posiciones 'm '(b c a d g a x a z)) -> nil
;;
;;--- Errores: 1. k no es una lista

(defun posiciones (x l)
    (posiciones* x l 0))

(defun posiciones* (x l i)
   (cond ((null l) nil)
         ((eq (car l) x) (cons (+ i 1) (posiciones* x (cdr l) (+ i 1))))
         (t (posiciones* x (cdr l) (+ i 1)))))


;;------------------------------------------------------------------------------
;; B. Funciones que usan operadores aritmeticos:
;;------------------------------------------------------------------------------


;;--- (max* x y) el maximo de x y y --------------------------------------------
;;
;;--- Ejemplo: (max* 3 4) -> 4
;;
;;--- Errores: x o y no es un numero

(defun max* (x y)
   (cond ((> x y) x)
         (t y)))


;;--- (conviertetemp temp s) -> la temperatura equivalente a temp en el --------
;;                              sistema s. Si s es 'c, convierte temp a grados
;;                              Fahrenheit; si es 'f, a grados Celsius
;;
;;--- Ejemplos: (conviertetemp -32 'f) -> 0
;;              (conviertetemp 0 'c) -> -32
;;
;;--- Errores: 1. temp no es numerico
;;             2. s no es 'f o 'c

(defun conviertetemp (temp s)
   (cond ((equal s 'f) (+ (/ (* temp 9) 5) 32))
         ((equal s 'c) (/ (* (- temp 32) 5) 9))
         (t 'error)))


;;--- (fact n) -> el factorial de n --------------------------------------------
;;
;;--- Ejemplo: (fact 5) -> 120
;;
;;--- Errores: 1. n no es numerico

(defun fact (n)
   (cond ((equal n 0) 1)
         (t (* n (fact (- n 1))))))


;;------------------------------------------------------------------------------
;; C. Funciones de procesamiento de listas con uso de operadores aritmeticos:
;;------------------------------------------------------------------------------


;;--- (sumalista l) -> la suma de los elementos de la lista l ------------------
;;
;;--- Ejemplo: (sumalista '(10 20 30 40 50)) -> 150
;;
;;--- Errores: 1. l no es una lista
;;             2. elemento no numerico en l

(defun sumalista (l)
   (cond ((null l) 0)
         (t (+ (car l) (sumalista (cdr l))))))


;;--- (sumatoria l) -> la sumatoria (la expresion simbolica) de los ------------
;;                     elementos de la lista l
;;
;;--- Ejemplo: (sumatoria '(10 20 30 40 50)) -> (+ 10 + 20 + 30 + 40 + 50)
;;
;;--- Errores: 1. l no es una lista

(defun sumatoria (l)
   (cond ((null l) nil)
        ( t (append (append '(+)
                            (cons (car l) nil))
                    (sumatoria (cdr l))))))


;;--- Version que elimina el primer '+ en la lista resultante
;;--- Ejemplo: (sumatoria* '(10 20 30 40 50)) -> (10 + 20 + 30 + 40 + 50)

(defun sumatoria* (l)
   (cond ((null l) nil)
         (t (cons (car l)
                  (sumatoria (cdr l))))))


;;------------------------------------------------------------------------------
;; D. Ejemplo: Las torres de Hanoi
;;------------------------------------------------------------------------------


;;--- (hanoi n a b c) -> resuelve Hanoi para N discos pasando de a a b por c ---
;;
;;--- Ejemplo: (hanoi 3 'a 'b 'c) -> t
;;                despliegue: (a -> b), (a -> c), (b -> c), (a -> b), (c -> a),
;;                            (c -> b), (a -> b)
;;
;;--- Errores: 1. n no es numerico

(defun hanoi (n a b c)
   (cond ((eq n 0) t)
         (t (hanoi (- n 1) a c b)
            (decir a b)
            (hanoi (- n 1) c b a))))

(defun decir (a b)
   (print (cons a
                (cons '->
                      (cons b nil)))))


;;--- (hanoi* n) -> resuelve Hanoi para N discos pasando de 'a a 'b por 'c' --
;;
;;--- Ejemplo: (hanoi* 3) -> t
;;                despliegue: (a -> b), (a -> c), (b -> c), (a -> b), (c -> a),
;;                            (c -> b), (a -> b)
;;
;;--- Errores: 1. n no es numerico

(defun hanoi* (n)
   (hanoi n 'a 'b 'c))


;;------------------------------------------------------------------------------
;; E. La negacion en LISP:
;;------------------------------------------------------------------------------


;;--- (negacion s) -> t si s devuelve nil, nil en caso contrario ---------------
;;
;;--- Ejemplo: (negacion (miembro 'a '(a b c)) -> nil

(defun negacion (s)
   (cond (s nil)
         (t t)))

;;------------------------------------------------------------------------------
