;; 14. Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in postordine.

;; VARIANTA I
(defun postOrder (binary_tree)
	(cond
		((endp binary_tree) nil)
		('t (append (postOrder (cadr binary_tree)) (postOrder (caddr binary_tree)) (list (car binary_tree))))
	)
)

;; VARIANTA II
(defun postOrderAlt (binary_tree)
	(if (null binary_tree) () (append (postOrderAlt (first (rest binary_tree))) (postOrderAlt (first (rest (rest binary_tree)))) (list (first binary_tree))))
)

;; VARIANTA III
(defun postOrderColRec (binary_tree col)
	(cond
		((not binary_tree) (cons (car col) (cdr col)))
		((not (cdr binary_tree)) (append col (list (car binary_tree))))
		((not (cddr binary_tree)) (append  (postOrderColRec (cadr binary_tree) col) (list (car binary_tree))))
		((not 'nil) (append (postOrderColRec (caddr binary_tree) (postOrderColRec (cadr binary_tree) col)) (list (car binary_tree))))
	)
)

(defun postOrderCol (binary_tree)
	(postOrderColRec binary_tree (list))
)