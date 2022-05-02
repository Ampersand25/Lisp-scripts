;1.

;a) Sa se insereze intr-o lista liniara un atom a dat dupa al 2-lea, al 4-lea, al 6-lea,....element.
(defun insertListRec (lista a idx)
	(cond
		((null lista) nil)
		((= idx 1) (cons (car lista) (insertListRec (cdr lista) a 2)))
		(t (cons (car lista) (cons a (insertListRec (cdr lista) a 1))))
	)
)

(defun insertList (lista a)
	(cond
		((null lista) nil)
		(t (insertListRec lista a 1))
	)
)

;b) Definiti o functie care obtine dintr-o lista data lista tuturor atomilor care apar, pe orice nivel, dar in ordine inversa.
;De exemplu: (((A B) C) (D E)) --> (E D C B A)
(defun getAtomsRec (lista col)
	(cond
		((null lista) col)
		((atom (car lista)) (getAtomsRec (cdr lista) (cons (car lista) col)))
		(t (getAtomsRec (cdr lista) (getAtomsRec (car lista) col)))
	)
)

(defun getAtoms (lista)
	(getAtomsRec lista nil)
)

;c) Definiti o functie care intoarce cel mai mare divizor comun al numerelor dintr-o lista neliniara.
(defun gcdNums (a b)
	(cond
		((= b 0) a)
		(t (gcdNums b (mod a b)))
	)
)

(defun gcdNumsAlt (a b)
	(cond
		((= a 0) b)
		((= b 0) a)
		((= a b) a) ; ((= a b) b)
		((< a b) (gcdNumsAlt a (- b a)))
		(t (gcdNumsAlt (- a b) b))
	)
)

(defun gcdListRec (lista gcdCol)
	(cond
		((null lista) gcdCol)
		((numberp (car lista)) (gcdListRec (cdr lista) (gcdNums (car lista) gcdCol)))
		((listp (car lista)) (gcdListRec (cdr lista) (gcdListRec (car lista) gcdCol)))
		(t (gcdListRec (cdr lista) gcdCol))
	)
)

(defun gcdList (lista)
	(cond
		((null lista) nil)
		(t (gcdListRec lista 0))
	)
)

;d) Sa se scrie o functie care determina numarul de aparitii ale unui atom dat intr-o lista neliniara.
(defun countAtom (lista atm)
	(cond
		((null lista) 0)
		((listp (car lista)) (+ (countAtom (car lista) atm) (countAtom (cdr lista) atm)))
		((not (equal atm (car lista))) (countAtom (cdr lista) atm))
		(t (+ (countAtom (cdr lista) atm) 1))
	)
)