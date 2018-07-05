;;------------------------------------------------------------------------------
;;
;; Universidad de Costa Rica
;; Escuela de Ciencias de la Computacin e Informtica
;; CI-1441 Paradigmas Computacionales
;; I-2018, Prof. Dr. Alvaro de la Ossa Osegueda
;;
;;------------------------------------------------------------------------------
;;
;; lablisp2.lisp -- Laboratorio de LISP, parte 2
;;                  Ejercicios
;;
;;------------------------------------------------------------------------------
;;
;; 1. Listas de propiedades
;;
;; Suponga que existe una lista de acceso global de la forma ( .. (o p v) .. ),
;; denominada "propiedades", en la que cada terna (o p v) representa una
;; propiedad p con valor v de un objeto o. Supongo adems que para cada objeto y
;; propiedad pueden existir en esa lista varias ternas con valores distintos. Lo
;; que no puede haber son ternas repetidas.
;;
;; Ejemplo:

(setq propiedades '( (tel1 color negro) (tel1 tamaño grande) (tel1 red 4g)
                     (tel2 color rojo) (tel2 red 3g)
                     (tel3 tamaño mini) (tel3 red 4g) (tel3 marca patito)
                     (tel4 red 3g) (tel4 red 4g) (tel4 color gris) ))

;; 1.1. Programe una funcin "prop", de dos parmetros, que reciba un objeto o
;;      (tomo) y una propiedad p (tomo), y devuelve todos los valores v
;;      asociados a o y p (una lista).
;;
;; Ejemplo: (prop 'tel4 'red) -> (3g 4g)
;;
;; -->

(defun prop (o p) 
	(prop* o p propiedades)
)

(defun prop* (o p l) 
	(cond ((null l) nil)
	((and (eq (caar l) o)(eq (cadar l) p))
		(cons (caddar l) (prop* o p (cdr l))) 
	)
	(t (prop* o p (cdr l)))
	)
)

;; 1.2. Programe una funcin "nprop" (de "nueva propiedad"), que reciba un
;;      un objeto (tomo), una propiedad (tomo) y un valor (cualquier
;;      expresin), añada la terna formada por estos tres elementos a la lista
;;      global de propiedades y devuelva la terna.
;;
;; Ejemplo: (nprop 'tel1 'marca 'forro) -> (tel1 marca forro)
;;
;; -->

(defun nprop (o p v)
	(setf propiedades (append propiedades (cons (list o p v) nil) 
                       ))
	(list o p v)
)

;; 1.3. Programe una funcin "xprop", que reciba un objeto (tomo) y una
;;      propiedad (tomo) y elimine de la lista "propiedades" todas las ternas
;;      correspondientes y devuelva la lista de ternas eliminadas.
;;
;; Ejemplo: (xprop 'tel4 'red) -> ((tel4 red 3g) (tel4 red 4g))
;;
;; -->

(defun xprop (o p) 
	(setf propiedades (xprop* o p propiedades)
	)
)

(defun xprop* (o p l) 
	(cond ((null l) nil)
	((and (eq (caar l) o)(eq (cadar l) p))
		(xprop* o p (cdr l))
	)
	(t (cons (car l) (xprop* o p (cdr l))))
	)
)

;; 1.4. Programa una funcin "similares", que reciba una propiedad (tomo) y un
;;      valor (cualquier expresin), y devuelve la lista de objetos que tienen
;;      esa propiedad con ese valor.
;;
;; Ejemplo: (similares 'red '4g) -> (tel1 tel3 tel4)
;;
;; -->

(defun similares (p v) 
	(similares* p v propiedades)
)

(defun similares* (p v l) 
	(cond ((null l) nil)
	((and (eq (caddar l) v)(eq (cadar l) p))
		(cons (caddar l) (similares* p v (cdr l))) 
	)
	(t (similares* p v (cdr l)))
	)
)



(defun similares (p v) 
	(similares* p v propiedades)
)

(defun similares* (p v l) 
	(cond ((null l) nil)
	((and (eq (cadar l) p)(eq (caddar l) v))
		(cons (caar l) (similares* p v (cdr l))) 
	)
	(t (similares* p v (cdr l)))
	)
)

;;------------------------------------------------------------------------------
;;
;; 2. Las funciones setf y setq
;;
;; Ejecute la llamada siguiente en el intérprete:

(setq x '(a b c d e))

;; Después, compile las funciones siguientes:

(defun foo2 (val1 val2 cir)
   (foo2* val1 val2 cir (cdr cir)))

(defun foo2* (val1 val2 cir pri)
   (cond ((eq val1 (car cir)) (setf (car cir) val2))
         ((eq (car cir) pri) nil)
         (t (foo2* val1 val2 (cdr cir) pri))))

(defun circular (lista)
   (setf (cdr (last lista)) lista))

;; Finalmente, explique qué hace el intérprete, paso a paso, y a qué apunta
;; la variable x al finalizar la evaluación de la llamada siguiente:

(and (circular x) (foo2 'c 'm x) (write (car x)) t)

;; -->

;;------------------------------------------------------------------------------
;;
;; 3. Explique qué hace la función siguiente.
;;

(defun foo3 (objeto1 objeto2 lista)
   (cond ((null lista) nil)
         ((equal objeto2 (car lista))
            (cons (car lista) (cons objeto1 (cdr lista))))
         (t (cons (car lista) (foo3 objeto1 objeto2 (cdr lista))))))

;; -->


;;------------------------------------------------------------------------------
;;
;; 4.a. ¿Qué hace la función foo?

(defun foo (l1 l2 lim)
   (cond ((not (eq (length l1) (length l2))) 'error)
         (t (foo* l1 l2 lim))))

(defun foo* (l1 l2 lim)
   (foon nil
      (mapcar #'(lambda (x y)
                   (cond ((> (+ x y) lim) (cons x y))
                         (t nil)))
              l1 l2)))

(defun foon (e l)
   (cond ((null l) nil)
         ((eq (car l) e) (foon e (cdr l)))
         (t (cons (car l) (foon e (cdr l))))))

;; -->

;; 4.b. Programe una función foo5*2 que haga lo mismo que foo*, pero sin usar
;;      la función foon, ni la expresión lambda pasada como argumento a mapcar.
;;
;; -->