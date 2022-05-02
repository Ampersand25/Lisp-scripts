;4.

;a) Definiti o functie care intoarce suma a doi vectori.
(defun len (lista)
	(if (null lista) 0 (+ 1 (len (cdr lista))))
)

(defun sumVec (vec_a vec_b)
	(if (not (equal (len vec_a) (len vec_b))) (error "Lungimi inegale vectori!")
		(cond
			((null vec_a) nil) ; ((null vec_b) nil)
			(t (cons (+ (car vec_a) (car vec_b)) (sumVec (cdr vec_a) (cdr vec_b))))
		)
	)
)

;b) Definiti o functie care obtine dintr-o lista data lista tuturor atomilor care apar, pe orice nivel, dar in aceeasi ordine.
;De exemplu: (((A B) C) (D E)) --> (A B C D E)
(defun allAtoms (lista)
	(cond
		((null lista) ())
		((atom (car lista)) (cons (car lista) (allAtoms (cdr lista))))
		('t (append (allAtoms (car lista)) (allAtoms (cdr lista))))
	)
)

;c) Sa se scrie o functie care plecand de la o lista data ca argument, inverseaza numai secventele continue de atomi.
;Exemplu: (a b c (d (e f) g h i)) ==> (c b a (d (f e) i h g))
(defun revContSeqRec (lista col)
	(cond
		((null lista) col)
		((listp (car lista)) (append col (cons (revContSeqRec (car lista) (list)) (revContSeqRec (cdr lista) (append)))))
		((not 'nil) (revContSeqRec (cdr lista) (cons (car lista) col)))
	)
)

(defun revContSeq (lista)
	(revContSeqRec lista '())
)

;d) Sa se construiasca o functie care intoarce maximul atomilor numerici dintr-o lista, de la nivelul superficial.
(defun maxim (a b)
	(cond
		((equal a nil) b)
		((equal b nil) a)
		(t (if (or (= a b) (> a b)) a b))
	)
)

(defun maxList (lista)
	(cond
		((null lista) nil)
		((not (numberp (car lista))) (maxList (cdr lista)))
		(t (maxim (car lista) (maxList (cdr lista))))
	)
)