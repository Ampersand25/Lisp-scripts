;; 11. Se da un arbore de tipul (2). Sa se afiseze nivelul (si lista corespunzatoare a nodurilor) avand numar maxim de noduri. Nivelul rad. se considera 0.

(defun countNodesLevelAux (binary_tree level curr)
	(cond
		((endp binary_tree) 0)
		((null (cdr binary_tree)) (if (= level curr) 1 0))
		((not (cddr binary_tree)) (if (= curr level) (- (countNodesLevelAux (cadr binary_tree) level (+ curr 1)) -1) (countNodesLevelAux (cadr binary_tree) level (+ 1 curr))))
		(t (if (not (equal level curr)) (+ (countNodesLevelAux (cadr binary_tree) level (+ curr 1)) (countNodesLevelAux (caddr binary_tree) level (+ 1 curr))) (+ 1 (countNodesLevelAux (cadr binary_tree) level (+ curr 1)) (countNodesLevelAux (caddr binary_tree) level (+ 1 curr)))))
	)
)

(defun countNodesLevel (binary_tree level)
	(if (and (listp binary_tree) (and (numberp level) (not (< level 0)))) (countNodesLevelAux binary_tree level 0) (error "Argument(e) invalid(e)!"))
)

(defun maxim (a b)
	(cond
		((or (not (numberp a)) (not (numberp b))) nil)
		((> a b) a)
		((not nil) b)
	)
)

(defun depthBT (binary_tree)
	(cond
		((endp binary_tree) -1)
		((endp (cdr binary_tree)) 0)
		((+ 1 (maxim (depthBT (cadr binary_tree)) (depthBT (caddr binary_tree)))))
	)
)

(defun finalLevelBT (binary_tree)
	(if (listp binary_tree) (depthBT binary_tree) (error "Argument invalid: parametrul de intrare trebuie sa fie o lista!"))
)

(defun getNodesLevelRec (binary_tree lvl currLvl)
	(cond
		((or (endp binary_tree) (> currLvl lvl)) ())
		((= currLvl lvl) (list (first binary_tree)))
		((not 'nil) (append (getNodesLevelRec (first (rest binary_tree)) lvl (+ currLvl 1)) (getNodesLevelRec (first (rest (rest binary_tree))) lvl (+ 1 currLvl))))
	)
)

(defun getNodesLevel (binary_tree lvl)
	(cond
		((and (not (listp binary_tree)) (or (not (numberp lvl)) (< lvl 0))) (error "Primele doua argumente sunt invalide!"))
		((not (listp binary_tree)) (error "Primul argument este invalid!"))
		((or (not (numberp lvl)) (> 0 lvl)) (error "Al doilea argument este invalid!"))
		('t (getNodesLevelRec binary_tree lvl 0))
	)
)

(defun levelMaxNodesRec (binary_tree lwrLvl uprLvl lvl maxNodeslvl nodesLvl)
	(cond
		((> lwrLvl uprLvl) (list lvl maxNodeslvl nodesLvl))
		((> (countNodesLevel binary_tree lwrLvl) maxNodeslvl) (levelMaxNodesRec binary_tree (+ 1 lwrLvl) uprLvl lwrLvl (countNodesLevel binary_tree lwrLvl) (getNodesLevel binary_tree lwrLvl)))
		((levelMaxNodesRec binary_tree (+ lwrLvl 1) uprLvl lvl maxNodeslvl nodesLvl))
	)
)

(defun levelMaxNodes (binary_tree lowerLevel upperLevel)
	(if (> lowerLevel upperLevel) (error "Argumente invalide: al doilea parametru de intrare trebuie sa fie mai mic sau egal decat al treilea!") (levelMaxNodesRec binary_tree (+ lowerLevel 1) upperLevel lowerLevel (countNodesLevel binary_tree lowerLevel) (getNodesLevel binary_tree lowerLevel)))
)

(defun levelMaxNodesUI (binary_tree)
	(if (not (listp binary_tree)) (error "Argument invalid: arborele binar trebuie sa fie reprezentat ca si o lista (neliniara)!") (if (endp binary_tree) (format nil "Arborele binar introdus este vid!") (let ((rez (levelMaxNodes binary_tree 0 (finalLevelBT binary_tree)))) (format nil "Nivelul cu numar maxim de noduri din arbore este nivelul ~A cu ~A noduri: ~A!" (car rez) (cadr rez) (caddr rez)))))
)