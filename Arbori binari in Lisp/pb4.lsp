;; 4. Sa se converteasca un arbore de tipul (2) la un arbore de tipul (1).

(defun convertBT (binary_tree)
	(cond
		((endp binary_tree) ())
		((null (cdr binary_tree)) (list (car binary_tree) 0))
		((not (cddr binary_tree)) (append (list (car binary_tree) 1) (convertBT (cadr binary_tree))))
		(t (append (list (car binary_tree) 2) (convertBT (cadr binary_tree)) (convertBT (caddr binary_tree))))
	)
)