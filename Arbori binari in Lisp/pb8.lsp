;; 8. Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in inordine.

;; VARIANTA I
(defun inOrder (binary_tree)
	(cond
		((endp binary_tree) nil)
		((endp (cdr binary_tree)) (list (car binary_tree)))
		((endp (cddr binary_tree)) (append (inOrder (cadr binary_tree)) (list (car binary_tree))))
		(t (append (inOrder (cadr binary_tree)) (list (car binary_tree)) (inOrder (caddr binary_tree))))
	)
)

;; VARIANTA II
(defun inOrderAlt (binary_tree)
	(if (null binary_tree) () (append (inOrderAlt (first (rest binary_tree))) (list (first binary_tree)) (inOrderAlt (first (rest (rest binary_tree))))))
)

;; VARIANTA III
(defun inOrderColRec (binary_tree col)
	(cond
		((not binary_tree) (append (list (car col)) (cdr col)))
		((not (cdr binary_tree)) (append col (list (car binary_tree))))
		((not (cddr binary_tree)) (append  (inOrderColRec (cadr binary_tree) col) (list (car binary_tree))))
		((append (inOrderColRec (caddr binary_tree) (append (inOrderColRec (cadr binary_tree) col) (list (car binary_tree))))))
	)
)

(defun inOrderCol (binary_tree)
	(inOrderColRec binary_tree (append))
)