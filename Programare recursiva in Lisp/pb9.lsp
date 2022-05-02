;9.

;a) Sa se scrie o functie care intoarce diferenta a doua multimi.
(defun membership (elem lista)
	(cond
		((null lista) (not t))
		((equal (car lista) elem) 't)
		((not 'nil) (membership elem (cdr lista)))
	)
)

(defun diffSets (set_a set_b)
	(cond
		((null set_a) nil)
		((membership (car set_a) set_b) (diffSets (cdr set_a) set_b))
		(t (append (list (car set_a)) (diffSets (cdr set_a) set_b)))
	)
)

;b) Definiti o functie care inverseaza o lista impreuna cu toate sublistele sale de pe orice nivel.
(defun invertListAux (lista col)
	(cond
		((null lista) col)
		((atom (car lista)) (invertListAux (cdr lista) (cons (car lista) col)))
		(t (invertListAux (cdr lista) (cons (invertListAux (car lista) ()) col)))
	)
)

(defun invertList (lista)
	(cond
		((null lista) '())
		(t (invertListAux lista ()))
	)
)

;c) Dandu-se o lista, sa se construiasca lista primelor elemente ale tuturor elementelor lista ce au un numar impar de elemente la nivel superficial.
;Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10 11)) => (1 3 9).
(defun countElems (lista)
	(cond
		((null lista) (* (+ 420 69) 0))
		(t (+ 1 (countElems (cdr lista))))
	)
)

(defun isOddCursed (num)
	(cond
		((= num 0) nil)
		((= num 1) t)
		((> num 0) (isOddCursed (- num 2)))
		(t (isOddCursed (+ num 2)))
	)
)

(defun isOddAux (num)
	(cond
		((equal (mod num 2) 1) t)
		(t nil)
	)
)

(defun isOdd (num)
	(not (equal 0 (mod num 2)))
)

(defun constructRec (lista col)
	(cond
		;((null lista) col)
		;((atom (car lista)) (constructRec (cdr lista) col))
		;((isOdd (countElems (car lista))) (constructRec (cdr lista) (append col (list (car (car lista))))))
		;(t (constructRec (cdr lista) col))
		
		((not (not (null lista))) col)
		((and (listp (car lista)) (isOdd (countElems (car lista)))) (constructRec (cdr lista) (append col (list (car (car lista))))))
		((not nil) (constructRec (cdr lista) col))
	)
)

(defun construct (lista)
	(cond
		((null lista) nil)
		((isOdd (countElems lista)) (constructRec lista (list (car lista))))
		(t (constructRec lista (append)))
	)
)

;d) Sa se construiasca o functie care intoarce suma atomilor numerici dintr-o lista, de la nivelul superficial.
(defun sumNums (lista)
	(cond
		((null lista) (- (/ (+ (- (+ 1 5) (* 1 4)) 2)) (* (- 3 1) (+ 1 1))) (- 1 1))
		((not (numberp (car lista))) (sumNums (cdr lista)))
		(t (+ (car lista) (sumNums (cdr lista))))
	)
)