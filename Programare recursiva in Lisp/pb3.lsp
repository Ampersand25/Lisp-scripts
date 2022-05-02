;3.

;a) Definiti o functie care intoarce produsul a doi vectori.
(defun lenVec (vec)
	(if (null vec) 0 (+ (lenVec (cdr vec)) 1))
)

(defun dotProduct (vec_a vec_b)
	(if (not (= (lenVec vec_a) (lenVec vec_b))) (format nil "[!]Vectori de lungimi diferite!")
		(if (null vec_a) 0 (+ (* (car vec_a) (car vec_b)) (dotProduct (cdr vec_a) (cdr vec_b))))
	)
)

(defun getK (lista k)
	(cond
		((null lista) 'nil)
		((= k 1) (car lista))
		('t (getK (cdr lista) (- k 1)))
	)
)

(defun calcProd (vec_a vec_b i j)
	(* (getK vec_a i) (getK vec_b j))
)

(defun computeCompCP (vec_a vec_b i j)
	;(- (* (getK vec_a i) (getK vec_b j)) (* (getK vec_a j) (getK vec_b i)))
	(- (calcProd vec_a vec_b i j) (calcProd vec_a vec_b j i))
)

(defun crossProduct (vec_a vec_b)
	(cond
		((not (equal (lenVec vec_a) (lenVec vec_b))) (error "Vectori de lungimi distincte!"))
		((not (= 3 (lenVec vec_a))) (error "Lungime invalida vectori!"))
		;(t (list (- (* (getK vec_a 2) (getK vec_b 3)) (* (getK vec_a 3) (getK vec_b 2))) (- (* (getK vec_a 3) (getK vec_b 1)) (* (getK vec_a 1) (getK vec_b 3))) (- (* (getK vec_a 1) (getK vec_b 2)) (* (getK vec_a 2) (getK vec_b 1)))))
		(t (list (computeCompCP vec_a vec_b 2 3) (computeCompCP vec_a vec_b 3 1) (computeCompCP vec_a vec_b 1 2)))
	)
)

;b) Sa se construiasca o functie care intoarce adancimea unei liste.
(defun maxim (a b)
	(if (< a b) b a)
)

(defun maximRec (a b)
	(cond
		((< a b) (maximRec b a))
		(t a)
	)
)

(defun depthList (lista)
	(cond
		((null lista) 1)
		((atom (car lista)) (depthList (cdr lista)))
		(t (maximRec (+ 1 (depthList (car lista))) (depthList (cdr lista))))
	)
)

;c) Definiti o functie care sorteaza fara pastrarea dublurilor o lista liniara.
(defun partition (lista pivot col fn)
	(cond
		((null lista) col)
		((funcall fn (car lista) pivot) (partition (cdr lista) pivot (append col (list (car lista))) fn))
		(t (partition (cdr lista) pivot col fn))
	)
)

;(defun less (a b)
;	(if (< a b) t nil)
;)

(defun lessPart (lista)
	;(partition (cdr lista) (car lista) (list) #'less)
	(partition (cdr lista) (car lista) (list) #'<)
)

;(defun greater (a b)
;	(if (> a b) t nil)
;)

(defun greaterPart (lista)
	;(partition (cdr lista) (car lista) (append) #'greater)
	(partition (cdr lista) (car lista) (append) #'>)
)

(defun qSort (lista)
	(cond
		((null lista) nil)
		(t (append (qSort (lessPart lista)) (list (car lista)) (qSort (greaterPart lista))))
	)
)

;d) Sa se scrie o functie care intoarce intersectia a doua multimi.
(defun membership (elem lista)
	(cond
		((null lista) nil)
		((equal elem (car lista)) t)
		((not nil) (membership elem (cdr lista)))
	)
)

(defun intersect (set_a set_b)
	(cond
		((null set_a) '())
		((membership (car set_a) set_b) (cons (car set_a) (intersect (cdr set_a) set_b)))
		('t (intersect (cdr set_a) set_b))
	)
)