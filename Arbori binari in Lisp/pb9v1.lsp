;; 9. Sa se converteasca un arbore de tipul (1) la un arbore de tipul (2).
;; Exemple de test:
;; 0. (A 2 B 0 C 2 D 0 E 0)                                                                         => (A (B) (C (D) (E)))
;; 1. (A 2 B 1 C 0 D 2 E 0 F 1 H 0)                                                                 => (A (B (C)) (D (E) (F (H))))
;; 2. (A 1 B 2 C 2 D 1 E 1 F 0 G 1 H 0 I 2 J 0 K 1 L 0)                                             => (A (B (C (D (E (F))) (G (H))) (I (J) (K (L)))))
;; 3. (A 2 B 2 C 0 D 1 E 0 F 1 G 2 H 1 J 0 K 2 L 0 M 2 N 1 O 0 P 0)                                 => (A (B (C) (D (E))) (F (G (H (J)) (K (L) (M (N (O)) (P))))))
;; 4. (A 2 B 2 C 2 E 1 G 1 H 0 F 1 K 0 D 1 L 1 M 2 N 0 P 0 Q 2 R 0 S 2 T 0 U 2 V 2 X 0 Y 0 W 1 Z 0) => (A (B (C (E (G (H))) (F (K))) (D (L (M (N) (P))))) (Q (R) (S (T) (U (V (X) (Y)) (W (Z))))))
;; 5. (A 2 B 1 C 1 D 1 E 2 F 0 G 0 I 2 J 1 K 0 L 2 M 2 N 0 O 1 P 2 Q 1 R 0 S 0 T 2 U 0 V 0)         => (A (B (C (D (E (F) (G))))) (I (J (K)) (L (M (N) (O (P (Q (R)) (S)))) (T (U) (V)))))

(defun getLeftRightSubtrees (binary_tree no_of_nodes no_of_edges col)
	;; Intr-un arbore binar avem urmatoarea relatie intre noduri/varfuri si muchii:
	;; numarul de muchii = numarul de noduri - 1 =>
	;; numarul de noduri = numarul de muchii + 1
	(if
		(or (= no_of_nodes (+ 1 no_of_edges)) (endp binary_tree))
		(list col binary_tree)
		(getLeftRightSubtrees (cddr binary_tree) (+ no_of_nodes 1) (+ no_of_edges (cadr binary_tree)) (append col (list (car binary_tree)) (list (cadr binary_tree))))
	)
)

(defun getSubtrees (binary_tree)
	(getLeftRightSubtrees (cddr binary_tree) 0 0 ()) ; <=> (getLeftRightSubtrees (cdr (cdr binary_tree)) 0 0 nil)
)

(defun getLeftSubtree (binary_tree)
	(car (getSubtrees binary_tree))
)

(defun getRightSubtree (binary_tree)
	(cadr (getSubtrees binary_tree)) ; <=> (car (cdr (getSubtrees binary_tree)))
)

(defun convertBinaryTreeRec (binary_tree)
	(cond
		((not binary_tree) (append)) ; <=> ((null binary_tree) (list))
		;((null (cddr binary_tree)) (list (car binary_tree)))
		(t (cons (car binary_tree) (list (convertBinaryTreeRec (getLeftSubtree binary_tree)) (convertBinaryTreeRec (getRightSubtree binary_tree)))))
	)
)

(defun removeNILsFromList (lista)
	(cond
		((null lista) 'nil)
		((equal (car lista) '()) (removeNILsFromList (cdr lista)))
		((listp (car lista)) (cons (removeNILsFromList (car lista)) (removeNILsFromList (cdr lista))))
		((not nil) (cons (car lista) (removeNILsFromList (cdr lista))))
	)
)

(defun convertBinaryTree (binary_tree)
	(removeNILsFromList (convertBinaryTreeRec binary_tree))
)