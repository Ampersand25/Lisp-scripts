;6.

;a) Sa se scrie de doua ori elementul de pe pozitia a n-a a unei liste liniare.
;De exemplu, pentru (10 20 30 40 50) si n=3 se va produce (10 20 30 30 40 50).
(defun duplicateN (lista N)
	(cond
		((null lista) nil)
		((not (= N 1)) (cons (car lista) (duplicateN (cdr lista) (- N 1))))
		(t (cons (car lista) (cons (car lista) (duplicateN (cdr lista) 0))))
	)
)

(defun duplicateNAuxRec (lista N idx)
	(cond
		((null lista) ())
		((equal N idx) (append (list (car lista) (car lista)) (cdr lista)))
		(t (cons (car lista) (duplicateNAuxRec (cdr lista) N (+ idx 1))))
	)
)

(defun duplicateNAux (lista N)
	(cond
		((= N 0) nil)
		(t (duplicateNAuxRec lista N 1))
	)
)

;b) Sa se scrie o functie care realizeaza o lista de asociere cu cele doua liste pe care le primeste.
;De ex: (A B C) (X Y Z) --> ((A.X) (B.Y) (C.Z)).
(defun associateLists (lista_1 lista_2)
	(cond
		((or (null lista_1) (null lista_2)) (list (list)))
		((or (null (cdr lista_1)) (null (cdr lista_2))) (list (cons (car lista_1) (car lista_2))))
		(t (cons (cons (car lista_1) (car lista_2)) (associateLists (cdr lista_1) (cdr lista_2))))
	)
)

;c) Sa se determine numarul tuturor sublistelor unei liste date, pe orice nivel. Prin sublista se intelege fie lista insasi, fie un element de pe orice nivel, care este lista.
;Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 5 (lista insasi, (3 ...), (4 5), (6 7), (9 10)).
(defun countSublists (lista)
	(cond
		((null lista) 1)
		((listp (car lista)) (+ (countSublists (car lista)) (countSublists (cdr lista))))
		(t (countSublists (cdr lista)))
	)
)

;d) Sa se construiasca o functie care intoarce numarul atomilor dintr-o lista, de la nivel superficial.
(defun countAtoms (lista)
	(cond
		((null lista) 0)
		((atom (car lista)) (+ (countAtoms (cdr lista)) 1))
		(t (countAtoms (cdr lista)))
	)
)