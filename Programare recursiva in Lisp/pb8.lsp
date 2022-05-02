;8.

;a) Sa se elimine elementul de pe pozitia a n-a a unei liste liniare.
(defun deleteN (lista N)
	(cond
		((null lista) lista) ; ((null lista) nil)
		((not (= N 1)) (cons (car lista) (deleteN (cdr lista) (- N 1))))
		(t (cdr lista))
	)
)

;b) Definiti o functie care determina succesorul unui numar reprezentat cifra cu cifra intr-o lista. De ex: (1 9 3 5 9 9) --> (1 9 3 6 0 0)
(defun succesorRecAux (lista cf col)
	(cond
		((and (null lista) (= cf 0)) col)
		((null lista) (cons 1 col))
		((and (= (car lista) 9) (= cf 1)) (succesorRecAux (cdr lista) 1 (cons 0 col)))
		((= (car lista) 9) (succesorRecAux (cdr lista) 0 (cons 9 col)))
		((= cf 0) (succesorRecAux (cdr lista) 0 (cons (car lista) col)))
		(t (succesorRecAux (cdr lista) 0 (cons (+ (car lista) 1) col)))
	)
)

(defun succesorRec (lista)
	(cond
		((null lista) 'nil)
		((= (car lista) 9) (succesorRecAux (cdr lista) 1 (list 0)))
		(t (succesorRecAux (cdr lista) 0 (list (+ (car lista) 1))))
	)
)

(defun successor (lista)
	(succesorRec (invertList lista))	
)

;c) Sa se construiasca multimea atomilor unei liste.Exemplu: (1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)
(defun invertListRec (lista col)
	(cond
		((null lista) col)
		(t (invertListRec (cdr lista) (cons (car lista) col)))
	)
)

(defun invertList (lista)
	(invertListRec lista (list))
)

(defun atomSetAux (lista col)
	(cond
		((null lista) col)
		((listp (car lista)) (atomSetAux (cdr lista) (atomSetAux (car lista) col)))
		((membership col (car lista)) (atomSetAux (cdr lista) col))
		(t (atomSetAux (cdr lista) (cons (car lista) col)))
	)
)

(defun atomSetAuxAlt (lista col)
	(cond
		((null lista) col)
		((listp (car lista)) (atomSetAuxAlt (cdr lista) (atomSetAuxAlt (car lista) col)))
		((membership col (car lista)) (atomSetAuxAlt (cdr lista) col))
		(t (atomSetAuxAlt (cdr lista) (append col (list (car lista)))))
	)
)

(defun atomSet (lista)
	;(invertList (atomSetAux lista nil))
	(atomSetAuxAlt lista nil)
)

;d) Sa se scrie o functie care testeaza daca o lista liniara este o multime.
(defun membership (lista elem)
	(cond
		((null lista) 'nil)
		((equal (car lista) elem) t)
		((not nil) (membership (cdr lista) elem))
	)
)

(defun testSet (lista)
	(cond
		((null lista) 't)
		((membership (cdr lista) (car lista)) nil)
		(t (testSet (cdr lista)))
	)
)