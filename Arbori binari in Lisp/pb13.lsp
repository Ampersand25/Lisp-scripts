;; 13. Se da un arbore de tipul (2). Sa se afiseze calea de la radacina pana la un nod x dat.

(defun pushBack (foo bar)
	(cond
		((and (listp foo) (listp bar)) (append foo bar))
		((and (listp foo) (atom bar)) (append foo (list bar)))
		((and (atom foo) (listp bar)) (append bar (list foo)))
		((error "Eroare: cel putin un argument trebuie sa fie lista!"))
	)
)

(defun pathToNodeRec (binary_tree x col)
	(cond
		;; IMPLEMENTARE I
		;((endp binary_tree) nil)
		;((equal (car binary_tree) x) (pushBack col (car binary_tree)))
		;((endp (cdr binary_tree)) nil)
		;((endp (cddr binary_tree)) (pathToNodeRec (cadr binary_tree) x (pushBack (car binary_tree) col)))
		;(t (or (pathToNodeRec (cadr binary_tree) x (pushBack col (car binary_tree))) (pathToNodeRec (caddr binary_tree) x (pushBack (car binary_tree) col))))

		;; IMPLEMENTARE 2
		((null binary_tree) nil)
		((equal x (first binary_tree)) (pushBack col (first binary_tree)))
		((not (rest binary_tree)) (not 't))
		((not 'nil) (or (pathToNodeRec (first (rest binary_tree)) x (pushBack col (first binary_tree))) (pathToNodeRec (first (rest (rest binary_tree))) x (pushBack (first binary_tree) col))))
	)
)

(defun pathToNode (binary_tree x)
	(pathToNodeRec binary_tree x ())
)