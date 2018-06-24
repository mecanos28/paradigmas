;; ------------------------------------------------------------------------
;;
;; UCR - Facultad de Ingenieria - ECCI
;; CI_1441 Paradigmas Computacionales
;; I-2018, Prof. Dr. Alvaro de la Ossa
;; B52864 Carlos Gamboa Vargas
;; B56219 Fernando Rojas Melendez
;; Tarea Programada III
;;
;; ------------------------------------------------------------------------

;;--- (negacion s) -> t si s devuelve nil, nil en caso contrario ---------------
;;
;;--- Ejemplo: (negacion (miembro 'a '(a b c)) -> nil

(defun negacion (s)
   (cond (s nil)
     	(t t)))

;; --- bpp (N,A) ----------------------------------------------------------
;; 	S es el sub arbol de A cuya raiz es N
;; 	S debe ser nil si el sub arbol no existe
;; --- ejemplo: (bpp ’d ’(a b (c d e) (f (g h) i))) -> d  = a b c d -------

(defun bpp (n a)
	(cond
    	((if (or
        	(negacion (listp a))
        	(negacion (atom n)))
        	(print "Entradas no validas") nil)
    	)
    	;Si es la raiz retorna todo el árbol
    	((eq n (car a))
        	(and (print (cons n nil)) a))
    	;Si no procesa el subarbol del primer hijo de la n y le pasa los recorridos    
    	(t
        	(subarbol n (cdr a) (cons (car a) nil)))
	)
)


;(subarbol (N,A,R) Recorre los subarboles del arbol y devuelve el subarbol del elemento cuando lo encuntra
;N es el elemento a encontrar
;A es la lista que representa el árbol
;R es el recorrido hasta el momento
(defun subarbol (n a r)
	(cond
    	;Si no hay subarbol ya acabo y no lo encontró, devuelve nil
    	((null a)
        	(and (print r) nil))
    	;Si n es una hoja del subarbol imprimo los recorridos y devuelvo la hoja
    	((eq n (car a))
        	(and (print(append r (cons (car a) nil))) (car a)))
    	;Si n es el padre de un subarbol, concatena n a los recorridos y devuelve el subarbol
    	((and (listp (car a))
        	(eq n (caar a))) (and (print(append r (cons (caar a) nil))) (car a)))
    	;Si no, pego el subarbol a la lista deshaciendo la sublista para poder ir hoja por hoja del subarbol en la nueva llamada    
    	((listp (car a))
        	(subarbol n (append (car a) (cdr a)) r))
    	;Si leí un elemento que que no es sublista del arbol y no es n, lo añado a recorridos y continuo    
    	(t
        	(subarbol n (cdr a) (append r (cons (car a) nil))))
       	 
    	;;Al usar AND podemos imprimir y devolver un valor cuando se cumple una condición
    	;;Al pasarle el atributo r a subarbol se usa append porque mezcla las dos listas en vez de meter una lista como elemento de la otra.
	)
)



;; --- bap (N,A) ----------------------------------------------------------
;; 	S es el sub arbol de A cuya raiz es N
;; 	nil si el sub arbol no existe
;; --- ejemplo: (bap ’d ’(a b (c d e) (f (g h) i))) -> d = a b c f d ------

(defun bap(N A)
	(cond
   	 ((if (or
   		 (negacion (listp a))
   		 (negacion (atom n)))
   		 (print "Entradas no validas") nil)
   	 )
    	((equal (car A) N)
        	(write (car A))
        	A
    	)
    	(t	 
        	(write (car A))
        	(bap2 N (cdr A))
    	)
	)
)

(defun bap2 (N A)
	(cond
    	((null A) '(nil))
    	((consp (car A))
        	(cond
            	((equal (caar A) N)
                	(write (caar A))
                	(car A)
            	)
            	(t
                	(write (caar A))
                	(let ((resto (cdr (car A))))
                    	(bap2 N (append (cdr A) resto))
                	)
            	)
        	)
       	 
    	)
    	((equal (car A) N)
        	(write (car A))
        	(car A)
    	)
    	(t
        	(write (car A))
        	(bap2 N (cdr A))
    	)
	)
)

;; --- potencia (L) ---------------------------------------------------
;; 	P es el conjunto potencia de L
;; --- ejemplo: (potencia ’(a b c)) -> (nil (a) (b) (c) (a b) (a c) (b
;;   	c) (a b c)) --------------------------------------------------------
 
(defun potencia (C)
	(cond
   	 ((negacion (listp c))
   		 (print "Entradas no validas") nil)
    	((null C) '(nil))
    	(t (let ((actual (potencia (cdr C))))
        	(append (mapcar (lambda (resto) (cons (car C) resto)) actual) actual)
       	)
    	)
	)
)

;; --- cartesiano (A,B) ---------------------------------------------------
;; 	C es el producto cartesiano de A y B
;; --- ejemplo: (cartesiano ’(a b c) ’(d e)) -> ((a . d) (a . e) (b . d) (b
;;  	. e) (c . d) (c . e)) -----------------------------------------------

(defun cartesiano(A B)
	(cond
   	 ((if (or
   		 (negacion (listp a))
   		 (negacion (listp b)))
   		 (print "Entradas no validas") nil)
   	 )
    	((null A) nil)
    	(t (let ((actual (cartesiano (cdr A) B)))
        	(append (mapcar (lambda (resto) (cons (car A) resto)) B) actual)
       	)
    	)
	)
)

;; (encripta H Ae As) → el resultado de encriptar la hilera de entrada H con un engranaje
;;formado por los alfabetos de entrada (Ae) y salida (As), y el estado final de la m´aquina,
;;formado por el par (ae . as), donde ae y as son los s´ımbolos de los alfabetos de entrada y
;;salida, respectivamente, en los que qued´o la m´aquina luego de encriptar H.

;;Es decir, que encripta devuelve un par de la forma (h . e), donde h es la hilera encriptada
;;y e el estado final de la m´aquina; e es de la forma (ue . us), donde ue y us son los ´ultimos
;;s´ımbolos de los alfabetos de entrada y salida, respectivamente, visitados por encripta

;; H es la hilera a encriptar
;; Ae es el alfabeto de entrada
;; As es el alfabeto de salida

;; ejemplo: (encripta '(a l f a r e r o) '(a l f r e o) '(0 1 2 3 4 5)) -> (0 1 2 0 3 4 3 5 (O 5))


(defun encripta (H Ae As)
	(cond
    	;; Si coincide la letra con la del alfabeto entrada, se toma la del alfabeto salida y se llama a encriptar la cola de la hilera.
    	((eq (car H) (car Ae))
        	(cons (car As) (encripta (cdr H) (rotarAlfabeto Ae) (rotarAlfabeto As))))
    	;; Si ya ecnriptamos toda la hilera en vez de devolver la letra de salida, devolvemos un par formado por los ultimos de los dos alfabetos que tbn son los ultimos dos usados
    	((null H)
        	(cons (append (cons (car (last Ae)) nil) (cons (car (last As)) nil)) nil))
    	;; Si no hemos terminado y si no coincide letra de H con la de Ae, entonces se rotan los dos alfabetos y se vuelve a llamar el método    
    	(t
        	(encripta H (rotarAlfabeto Ae) (rotarAlfabeto As)))
	)
)


;; (rotaAlfabeto a) → devuelve el alfabeto a con su primer elemento ahora como último y so cadr como primero
(defun rotarAlfabeto (a)
	;;Se mezcla la cola del alfabeto con una nueva lista formada por el primer elemento y nil, dándole vuelta
	(append (cdr a) (cons (car a) nil))
)


;; --- decripta (Hs Ae As Ef) ----------------------------------------
;; 	Decodifica la hilera encriptada Hs usando los alfabetos Ae y As,
;; 	iniciando en la posicion del engranaje descrita por el estado final  
;; 	de  la  maquina (Ef) cuando se encripto la hilera que produjo la
;; 	hilera Hs. decripta devuelve en He la hilera decodificada.
;; ------------------------------------------------------------------------

;; ejemplo: (decripta '(0 1 2 0 3 4 3 5) '(a l f r e o) '(0 1 2 3 4 5) '(o 5))

(defun decripta (Hs Ae As Ef)
    (cond
   	 ((if (and
   		 (eq (car Ef) (car Ae))
   		 (eq (cadr Ef) (car As)))
   		 (decripta2 Hs (reversa Ae) (reversa As)))
   	 )
   	 (t (decripta Hs (rotarAlfabeto Ae) (rotarAlfabeto As) Ef))
    )
)

(defun decripta2 (Hs Ae As)
	(cond
    	;; Si coincide la letra con la del alfabeto entrada, se toma la del alfabeto salida y se llama a encriptar la cola de la hilera.
    	((eq (car Hs) (car As))
        	(cons (car Ae) (decripta2 (cdr Hs) (rotarAlfabeto Ae) (rotarAlfabeto As))))
    	;; Si ya ecnriptamos toda la hilera en vez de devolver la letra de salida, devolvemos un par formado por los ultimos de los dos alfabetos que tbn son los ultimos dos usados
    	((null Hs)
        	nil)
    	;; Si no hemos terminado y si no coincide letra de H con la de Ae, entonces se rotan los dos alfabetos y se vuelve a llamar el método    
    	(t
        	(decripta2 Hs (rotarAlfabeto Ae) (rotarAlfabeto As)))
	)
)

;;--- (reversa l) -> el resultado de invertir los elementos de l ---------------
;;
;;--- Ejemplo: (reversa '(a b c)) -> (c b a)
;;
;;--- Errores: 1. l no es una lista

(defun reversa (l)
   (cond ((null l) nil)
     	(t (append (reversa (cdr l))
                	(cons (car l) nil)))))
