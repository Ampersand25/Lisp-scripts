;5.

;a) Definiti o functie care interclaseaza cu pastrarea dublurilor doua liste liniare sortate.
(defun mergeLists (lista_1 lista_2)
	(cond
		((null lista_1) lista_2)
		((null lista_2) lista_1)
		((< (car lista_1) (car lista_2)) (cons (car lista_1) (mergeLists (cdr lista_1) lista_2)))
		;((> (car lista_1) (car lista_2)) (cons (car lista_2) (mergeLists lista_1 (cdr lista_2))))
		((> (car lista_1) (car lista_2)) (mergeLists lista_2 lista_1))
		(t (cons (car lista_1) (mergeLists (cdr lista_1) (cdr lista_2))))
	)
)

;b) Definiti o functie care substituie un element E prin elementele unei liste L1 la toate nivelurile unei liste date L.
(defun replaceElem (E L1 L)
	(cond
		((null L) nil)
		((listp (car L)) (cons (replaceElem E L1 (car L)) (replaceElem E L1 (cdr L))))
		; cons   - inlocuim elementul E (atom simbolic sau numeric (numar)) cu lista L1
		; append - inlocuim elementul E (atom simbolic sau numeric (numar)) cu elementele listei L1 (ci nu cu lista L1) (append face concatenarea continutului unor liste date ca si argumente (parametrii actuali/efectivi) in cadrul apelului)
		((equal (car L) E) (append L1 (replaceElem E L1 (cdr L)))) ; ((equal (car L) E) (cons L1 (replaceElem E L1 (cdr L)))) - daca vrem sa il inlocuim pe E cu lista L1 (ci nu cu elementele ei)
		(t (cons (car L) (replaceElem E L1 (cdr L))))
	)
)

;c) Definiti o functie care determina suma a doua numere in reprezentare de lista si calculeaza numarul zecimal corespunzator sumei.
(defun convertListToNumberAux (lista col)
	(cond
		((null lista) col)
		((not nil) (convertListToNumberAux (cdr lista) (+ (* col 10) (car lista))))
	)
)

(defun convertListToNumber (lista)
	;(if (null lista) nil (convertListToNumberAux lista 0))
	(if (null lista) 'nil (convertListToNumberAux (cdr lista) (car lista)))
)

(defun invertListRec (lista col)
	(cond
		((not (null lista)) (invertListRec (cdr lista) (cons (car lista) col)))
		(t col)
	)
)

(defun invertList (lista)
	(invertListRec lista (list))
)

(defun op10 (n m cf fn)
	(funcall fn (+ (+ n m) cf) 10)
)

(defun sumListsRec (la lb cf lsum)
	(cond
		((and (null la) (null lb)) (if (= cf 0) lsum (cons 1 lsum)))
		((null la) (sumListsRec nil (cdr lb) (op10 0 (car lb) cf #'floor) (cons (op10 0 (car lb) cf #'mod) lsum)))
		((null lb) (sumListsRec (cdr la) nil (op10 (car la) 0 cf #'floor) (cons (op10 (car la) 0 cf #'mod) lsum)))
		(t (sumListsRec (cdr la) (cdr lb) (op10 (car la) (car lb) cf #'floor) (cons (op10 (car la) (car lb) cf #'mod) lsum)))
	)
)

(defun sumLists (la lb)
	(convertListToNumber (sumListsRec (invertList la) (invertList lb) 0 (append)))
)

;d) Definiti o functie care intoarce cel mai mare divizor comun al numerelor dintr-o lista liniara.
(defun gcdNum (a b)
	(if (= b 0) a (gcdNum b (mod a b)))
	;(if (not (= 0 b)) (gcdNum b (mod a b)) a)
)

(defun gcdList (lista)
	(cond
		((null lista) 0)
		((numberp (car lista)) (gcdNum (car lista) (gcdList (cdr lista))))
		(t (gcdList (cdr lista)))
	)
)