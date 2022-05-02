;15.

;a) Sa se scrie o functie care intoarce reuniunea a doua multimi.
(defun membership (elem lista)
	(cond
		((null lista) nil)
		((equal (car lista) elem) 't)
		((not 'nil) (membership elem (cdr lista)))
	)
)

(defun intersectSets (set_a set_b)
	(cond
		((null set_a) '())
		((membership (car set_a) set_b) (cons (car set_a) (intersectSets (cdr set_a) set_b)))
		(t (intersectSets (cdr set_a) set_b))
	)
)

(defun reunionSets (set_a set_b)
	(cond
		((null set_a) set_b)
		((null set_b) set_a)
		((not (membership (car set_a) set_b)) (cons (car set_a) (reunionSets (cdr set_a) set_b)))
		('t (reunionSets (cdr set_a) set_b))
	)
)

;b) Sa se construiasca o functie care intoarce produsul atomilor numerici dintr-o lista, de la orice nivel.
(defun prodNums (lista)
	(cond
		((null lista) 1)
		((listp (car lista)) (* (prodNums (car lista)) (prodNums (cdr lista))))
		((numberp (car lista)) (* (car lista) (prodNums (cdr lista))))
		(t (prodNums (cdr lista)))
	)
)

;c) Definiti o functie care sorteaza cu pastrarea dublurilor o lista liniara.
(defun partitionRec (lista pivot less greater)
	(cond
		((null lista) (list less (list pivot) greater))
		((< (car lista) pivot) (partitionRec (cdr lista) pivot (append less (list (car lista))) greater))
		(t (partitionRec (cdr lista) pivot less (append greater (list (car lista)))))
	)
)

(defun partition (lista pivot)
	(partitionRec lista pivot (list) (append))
)

(defun lessRec (lista pivot col)
	(cond
		((null lista) col)
		((< (car lista) pivot) (lessRec (cdr lista) pivot (cons (car lista) col)))
		(t (lessRec (cdr lista) pivot col))
	)
)

(defun less (lista pivot)
	(lessRec lista pivot ())
)

(defun greaterRec (lista pivot col)
	(cond
		((null lista) col)
		((< (car lista) pivot) (greaterRec (cdr lista) pivot col))
		(t (greaterRec (cdr lista) pivot (cons (car lista) col)))
	)
)

(defun greater (lista pivot)
	(greaterRec lista pivot '())
)

(defun qSort (lista)
	;(cond
	;	((null lista) nil)
	;	(t (append (qSort (less (cdr lista) (car lista))) (list (car lista)) (qSort (greater (cdr lista) (car lista)))))
	;)

	(if (null lista) (not t) (append (qSort (less (cdr lista) (car lista))) (list (car lista)) (qSort (greater (cdr lista) (car lista)))))
)

;d) Definiti o functie care construiește o listă cu pozițiile elementului minim dintr-o listă liniară numerică.
(defun getMinListRec (lista minCurr)
	(cond
		((null lista) minCurr)
		((< (car lista) minCurr) (getMinListRec (cdr lista) (car lista)))
		(t (getMinListRec (cdr lista) minCurr))
	)
)

(defun getMinList (lista)
	(if (null lista) nil (getMinListRec (cdr lista) (car lista)))
)

(defun computeListAltRec (lista pos minList col)
	(cond
		((null lista) col)
		((= (car lista) minList) (computeListAltRec (cdr lista) (+ 1 pos) minList (cons pos col)))
		(t (computeListAltRec (cdr lista) (+ pos 1) minList col))
	)
)

(defun computeListAlt (lista)
	(computeListAltRec lista 1 (getMinList lista) nil)
)

(defun computeListAux (lista idx minim col)
	(cond
		((null lista) col)
		((= (car lista) minim) (computeListAux (cdr lista) (+ 1 idx) minim (append col (list idx))))
		((< (car lista) minim) (computeListAux (cdr lista) (+ idx 1) (car lista) (list idx)))
		(t (computeListAux (cdr lista) (- idx -1) minim col))
	)
)

(defun computeList (lista)
	(cond
		((null lista) nil)
		((null (cdr lista)) (list 1))
		(t (computeListAux (cdr lista) 2 (car lista) (list 1)))
	)
)