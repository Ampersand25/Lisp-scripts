;; 10. Se da un arbore de tipul (2). Sa se precizeze nivelul pe care apare un nod x in arbore. Nivelul radacii se considera a fi 0.

;; IMPLEMENTARE - VARIANTA I
(defun sum (a b)
	;(cond
	;	((or (not (numberp a)) (not (numberp b))) nil)
	;	(t (+ a b))
	;)

	(if (and (numberp a) (numberp b)) (+ a b) nil)
)

(defun getLevelOfNode (binary_tree x)
	(cond
		((endp binary_tree) 'nil)
		((equal (car binary_tree) x) 0)
		((or (sum (getLevelOfNode (cadr binary_tree) x) 1) (sum 1 (getLevelOfNode (caddr binary_tree) x))))
	)
)

(defun searchNode (binary_tree node)
	(cond
		((null binary_tree) nil)
		((equal (car binary_tree) node) t)
		('t (or (searchNode (car (cdr binary_tree)) node) (searchNode (car (cdr (cdr binary_tree))) node)))
	)
)

;; IMPLEMENTARE - VARIANTA II (CU VARIABILA COLECTOARE SI FUNCTIE WRAPPER)
(defun inc (arg)
	(if (numberp arg) (- arg -1) nil)
)

(defun dec (arg)
	(if (not (numberp arg)) nil (+ arg -1))
)

;; FUNCTIE AUXILIARA RECURSIVA/RECURENTA
(defun getLevelOfNodeAltRec (binary_tree x level)
	(cond
		((not binary_tree) nil)
		((equal x (first binary_tree)) level)
		((not (rest binary_tree)) nil)
		((not (rest (rest binary_tree))) (getLevelOfNodeAltRec (first (rest binary_tree)) x (inc level)))
		(t (or (getLevelOfNodeAltRec (first (rest binary_tree)) x (inc level)) (getLevelOfNodeAltRec (first (rest (rest binary_tree))) x (inc level))))
	)
)

;; FUNCTIE DE TIP WRAPPER
(defun getLevelOfNodeAlt (binary_tree x)
	(getLevelOfNodeAltRec binary_tree x 0)
)

;; FUNCTIE DE TIP UI (USER INTERFACE)
(defun getLevelOfNodeUI (binary_tree x)
	;; APEL - VARIANTA I CU COND
	;(cond
	;	((equal (searchNode binary_tree x) t) (format t "Nodul ~A se afla in arborele binar introdus pe nivelul ~A!" x (getLevelOfNode binary_tree x)))
	;	((format 't "Nodul ~A nu se afla in arborele binar introdus!" x))
	;)

	;; APEL - VARIANTA I CU IF
	;(if (not (getLevelOfNode binary_tree x)) (format nil "Nodul ~A nu se afla in arborele binar introdus!" x) (format nil "Nodul ~A se afla in arborele binar introdus pe nivelul ~A!" x (getLevelOfNode binary_tree x)))

	;; APEL - VARIANTA II
	;(if (getLevelOfNodeAlt binary_tree x) (format nil "Nodul ~A se afla in arborele binar introdus pe nivelul ~A!" x (getLevelOfNodeAlt binary_tree x)) (format nil "Nodul ~A nu se afla in arborele binar introdus!" x))

	;; APEL - VARIANTA III
	(let ((level (getLevelOfNode binary_tree x))) (if (not level) (format nil "Nodul ~A nu se afla in arborele binar ~A!" x binary_tree) (format nil "Nodul ~A se afla in arborele binar ~A pe nivelul ~A!" x binary_tree level)))
)