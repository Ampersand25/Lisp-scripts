;13.

;a) Sa se intercaleze un element pe pozitia a n-a a unei liste liniare.
(defun insertN (lista N elem)
	(cond
		((and (null lista) (not (= N 1))) ())
		((null lista) (list elem))
		((= N 1) (cons elem lista))
		(t (append (list (car lista)) (insertN (cdr lista) (- N 1) elem)))
	)
)

;b) Sa se construiasca o functie care intoarce suma atomilor numerici dintr-o lista, de la orice nivel.
(defun sumNums (lista)
	(cond
		((null lista) 0)
		((listp (car lista)) (+ (sumNums (car lista)) (sumNums (cdr lista))))
		((numberp (car lista)) (+ (car lista) (sumNums (cdr lista))))
		(t (sumNums (cdr lista)))
	)
)

;c) Sa se scrie o functie care intoarce multimea tuturor sublistelor unei liste date.
;Ex: Ptr. lista ((1 2 3) ((4 5) 6)) => ((1 2 3) (4 5) ((4 5) 6))
(defun invertListRec (lista col)
	;(cond
	;	((null lista) col)
	;	(t (invertListRec (cdr lista) (cons (car lista) col)))
	;)

	(if (null lista) col (invertListRec (cdr lista) (cons (car lista) col)))
)

(defun invertList (lista)
	(invertListRec lista (list))
)

(defun computeSublistsAux (lista)
	(cond
		((null lista) nil)
		((listp (car lista)) (append (computeSublistsAux (cdr lista)) (cons (car lista) (computeSublistsAux (car lista)))))
		(t (computeSublistsAux (cdr lista)))
	)
)

(defun computeSublists (lista)
	(invertList (computeSublistsAux lista))
)

(defun computeSublistsRec (lista col)
	(cond
		((null lista) col)
		((atom (car lista)) (computeSublistsRec (cdr lista) col))
		(t (computeSublistsRec (cdr lista) (append col (append (computeSublistsRec (car lista) ()) (list (car lista))))))
	)
)

(defun computeSublistsAlt (lista)
	(computeSublistsRec lista (append))
)

;d) Sa se scrie o functie care testeaza egalitatea a doua multimi, fara sa se faca apel la diferenta a doua multimi.
(defun lenSet (set_n)
	(if (null set_n) 0 (+ 1 (lenSet (cdr set_n))))
)

(defun membership (elem lista)
	(cond
		((null lista) (not t))
		((equal elem (car lista)) t)
		('t (membership elem (cdr lista)))
	)
)

(defun equSetsAux (set_a set_b)
	(cond
		((null set_a) t)
		((not (membership (car set_a) set_b)) nil)
		(t (equSetsAux (cdr set_a) set_b))
	)
)

(defun equSets (set_a set_b)
	(cond
		((not (= (lenSet set_a) (lenSet set_b))) nil)
		(t (equSetsAux set_a set_b))
	)
)