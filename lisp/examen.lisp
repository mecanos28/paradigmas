(defun miembro (x l)
	(cond
		((null l) nil)
		((eq (car l) x) t)
		(t (miembro x (cdr l)))
	)
)

(defun mc (a b)
	(cond
		((and (mc* a b) (mc* b a)) "carepicha")
		(t "carepicha2")
	)
)

(defun mc* (a b)
	(cond
		((null a) t)
		((miembro (car a) b) (mc* (cdr a) b))
		(nil)
	)
)

(defun merge1 (a b)
	(cond
		((null a) b)
		((null b) a)
		((< (car a) (car b)) (append (cons (car a) nil) (merge1 (cdr a) b)))
		(t (append (cons (car b) nil) (merge1 a (cdr b))))
	)
)

(defun sumariza (l)
	(cons (sumarizaI 0 l) (sumarizaE 0 l))
)

(defun sumarizaI (i l)
	(cond
		((null l) (+ i 0))
		((eq (caar l) 'i) (+ i (sumarizaI (cadar l) (cdr l))))
		(t (+ i (sumarizaI 0 (cdr l))))
	)
)

(defun sumarizaE (i l)
	(cond
		((null l) (+ i 0))
		((eq (caar l) 'e) (+ i (sumarizaE (cadar l) (cdr l))))
		(t (+ i (sumarizaE 0 (cdr l))))
	)
)

;; (sumariza '((i 10) (i 15) (e 10) (e 16)))