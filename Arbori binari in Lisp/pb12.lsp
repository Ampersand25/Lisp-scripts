;; 12. Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in preordine.

;; VARIANTA I
(defun preOrder (binary_tree)
	(cond
		((endp binary_tree) 'nil)
		(t (cons (car binary_tree) (append (preOrder (cadr binary_tree)) (preOrder (caddr binary_tree)))))
	)
)

;; VARIANTA II
(defun preOrderAlt (binary_tree)
	(if (not binary_tree) '() (append (list (first binary_tree)) (preOrderAlt (first (rest binary_tree))) (preOrderAlt (first (rest (rest binary_tree))))))
)

;; VARIANTA III
(defun preOrderColRec (binary_tree col)
	(cond
		((null binary_tree) col)
		((null (cdr binary_tree)) (append col (list (car binary_tree))))
		((null (cddr binary_tree)) (preOrderColRec (cadr binary_tree) (append col (list (car binary_tree)))))
		((preOrderColRec (caddr binary_tree) (preOrderColRec (cadr binary_tree) (append col (list (car binary_tree))))))
	)
)

(defun preOrderCol (binary_tree)
	(preOrderColRec binary_tree (append))
)