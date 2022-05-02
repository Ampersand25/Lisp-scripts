;2.

;a) Definiti o functie care selecteaza al n-lea element al unei liste, sau NIL, daca nu exista.
(defun selectNRec (lista N idx)
	(cond
		((null lista) 'nil)
		((= idx N) (car lista))
		(t (selectNRec (cdr lista) N (+ 1 idx)))
	)
)

(defun selectN (lista N)
	(cond
		((or (null lista) (equal N 0)) (list))
		((not '()) (selectNRec lista N 1))
	)
)

;b) Sa se construiasca o functie care verifica daca un atom e membru al unei liste nu neaparat liniara.
(defun membership (lista a)
	(cond
		((null lista) ())
		((listp (car lista)) (or (membership (cdr lista) a) (membership (car lista) a)))
		((equal (car lista) a) t)
		(t (membership (cdr lista) a))
	)
)

;c) Sa se construiasca lista tuturor sublistelor unei liste. Prin sublista se intelege fie lista insasi, fie un element de pe orice nivel, care este lista.
;Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) => ( (1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10) ).
(defun computeListsRec (lista col)
	(cond
		((null lista) col)
		((not (listp (car lista))) (computeListsRec (cdr lista) col))
		(t (computeListsRec (cdr lista) (append col (computeListsRec (car lista) (list (car lista))))))
	)
)

(defun computeLists (lista)
	(cond
		((null lista) (list nil))
		(t (computeListsRec lista (list lista)))
	)
)

;d) Sa se scrie o functie care transforma o lista liniara intr-o multime.
(defun toSet (lista)
	(cond
		((null lista) nil)
		((membership (cdr lista) (car lista)) (toSet (cdr lista)))
		(t (cons (car lista) (toSet (cdr lista))))
	)
)