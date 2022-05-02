;11.

;a) Sa se determine cel mai mic multiplu comun al valorilor numerice dintr-o lista neliniara.
(defun cmmdc (a b)
	(if (= b 0) a (cmmdc b (mod a b)))
)

(defun cmmmc (a b)
	(/ (* a b) (cmmdc a b))
)

(defun cmmmcListRec (lista col)
	(cond
		((null lista) col)
		((listp (car lista)) (cmmmcListRec (cdr lista) (cmmmcListRec (car lista) col)))
		((numberp (car lista)) (cmmmcListRec (cdr lista) (cmmmc (car lista) col)))
		(t (cmmmcListRec (cdr lista) col))
	)
)

(defun cmmmcList (lista)
	(cond
		((null lista) 0)
		(t (cmmmcListRec lista 1))
	)
)

;b) Sa se scrie o functie care sa testeze daca o lista liniara formata din numere intregi are aspect de "munte"(o secvență se spune ca are aspect de "munte" daca elementele cresc pana la un moment dat, apoi descresc.
;De ex. 10 18 29 17 11 10).
(defun eMunteRec (lista status)
	(cond
		;((or (null lista) (null (cdr lista))) status)
		;((= (car lista) (car (cdr lista))) nil)
		;((> (car lista) (car (cdr lista))) (eMunteRec (cdr lista) t))
		;((equal status t) nil)
		;(t (eMunteRec (cdr lista) status))

		; Refactorizare
		((or (null lista) (null (cdr lista))) status)
		((> (car lista) (car (cdr lista))) (eMunteRec (cdr lista) t))
		((or (equal status t) (= (car lista) (car (cdr lista)))) nil)
		(t (eMunteRec (cdr lista) nil))
	)
)

(defun eMunte (lista)
	(cond
		;((null lista) nil)
		;((null (cdr lista)) nil)
		;((null (cdr (cdr lista))) nil)
		;((or (= (car lista) (car (cdr lista))) (> (car lista) (car (cdr lista)))) nil)
		;((= (car (cdr lista)) (car (cdr (cdr lista)))) nil)
		;(t (eMunteRec (cdr lista) nil))

		; Refactorizare
		((or (or (or (null lista) (null (cdr lista))) (null (cdr (cdr lista)))) (not (< (car lista) (car (cdr lista))))) nil)
		(t (eMunteRec (cdr lista) nil))
	)
)

;c) Sa se elimine toate aparitiile elementului numeric maxim dintr-o lista neliniara.
(defun firstNum (lista)
	(cond
		((null lista) nil)
		((numberp (car lista)) (car lista))
		((atom (car lista)) (firstNum (cdr lista)))
		(t (or (firstNum (car lista)) (firstNum (cdr lista))))
	)
)

(defun maxListRec (lista maxCurr)
	(cond
		;((null lista) maxCurr)
		;((and (not (numberp (car lista))) (atom (car lista))) (maxListRec (cdr lista) maxCurr))
		;((listp (car lista)) (maxListRec (cdr lista) (maxListRec (car lista) maxCurr)))
		;((> (car lista) maxCurr) (maxListRec (cdr lista) (car lista)))
		;(t (maxListRec (cdr lista) maxCurr))

		; Refactorizare
		((null lista) maxCurr)
		((listp (car lista)) (maxListRec (cdr lista) (maxListRec (car lista) maxCurr)))
		((and (numberp (car lista)) (> (car lista) maxCurr)) (maxListRec (cdr lista) (car lista)))
		(t (maxListRec (cdr lista) maxCurr))
	)
)

(defun maxList (lista)
	(if (null lista) nil (maxListRec lista (firstNum lista)))
)

(defun deleteElemFromList (lista elem)
	(cond
		;((null lista) (list))
		;((listp (car lista)) (cons (deleteElemFromList (car lista) elem) (deleteElemFromList (cdr lista) elem)))
		;((not (numberp (car lista))) (cons (car lista) (deleteElemFromList (cdr lista) elem)))
		;((= (car lista) elem) (deleteElemFromList (cdr lista) elem))
		;(t (cons (car lista) (deleteElemFromList (cdr lista) elem)))
		
		; Refactorizare
		((null lista) (append))
		((listp (car lista)) (cons (deleteElemFromList (car lista) elem) (deleteElemFromList (cdr lista) elem)))
		((equal elem (car lista)) (deleteElemFromList (cdr lista) elem))
		(t (cons (car lista) (deleteElemFromList (cdr lista) elem)))
	)
)

(defun deleteMax (lista)
	(deleteElemFromList lista (maxList lista))
)

;d) Sa se construiasca o functie care intoarce produsul atomilor numerici pari dintr-o lista, de la orice nivel.
(defun prodEvenList (lista)
	(cond
		((null lista) 1)
		((listp (car lista)) (* (prodEvenList (car lista)) (prodEvenList (cdr lista))))
		((and (numberp (car lista)) (equal (mod (car lista) 2) 0)) (* (car lista) (prodEvenList (cdr lista))))
		(t (prodEvenList (cdr lista)))
	)
)