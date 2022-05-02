;; 16. Sa se decida daca un arbore de tipul (2) este echilibrat (diferenta dintre adancimile celor 2 subarbori nu este mai mare decat 1).

;; Model matematic pentru functia maxim:
;; maxim(a, b) = { false (nil), daca a nu este numar si b nu este numar
;;               { b          , daca a nu este numar si b este numar
;;               { a          , daca b nu este numar si a este numar
;;               { a          , daca a este numar, b este numar si a > b
;;               { b          , altfel (adica daca a este numar, b este numar si a <= b)
(defun maxim (a b)
	"Functie utilizator/user cu nume de tip operand (rezultat) care calculeaza maximul dintre doua argumente date: <a> si <b>. 
	Daca <a> si <b> nu sunt de tip numeric atunci functia returneaza nil (valoarea logica de adevar (booleana) false/fals), 
	iar daca doar unul dintre cei doi parametrii de intrare este un atom numeric (numar) atunci subprogramul va returna valoarea acestui argument"
	(cond
		((and (not (numberp a)) (not (numberp b))) nil)
		((not (numberp a)) b)
		((not (numberp b)) a)
		((> a b) a)
		(b) ; <=> (t b) <=> ('t b) <=> ((not nil) b) <=> ((not 'nil) b)
	)
)

;; Model matematic pentru functia heightBTRec:
;; heightBTRec(binary_tree) =
;; heightBTRec(l1l2l3)      = { -1                                             , daca binary_tree (l1l2l3) este lista vida, adica binary_tree = l1l2l3 = (l1, l2, l3) = ()
;;                            { 0                                              , daca l2l3 este lista vida, adica l2l3 = (l2, l3) = () (ceea ce inseamna ca binary_tree = l1 = (l1))
;;                            { 1 + heightBTRec(l3)                            , daca l2 este lista vida, adica l2 = (l2) = () (ceea ce inseamna ca binary_tree = l1l3 = (l1, l3))
;;                            { 1 + heightBTRec(l2)                            , daca l3 este lista vida, adica l3 = (l3) = () (ceea ce inseamna ca binary_tree = l1l2 = (l1, l2))
;;                            { maxim(1 + heightBTRec(l2), 1 + heightBTRec(l3)), altfel (adica binary_tree are ambii subarbori nevizi)
(defun heightBTRec (binary_tree)
	"Functie utilizator/user cu nume de tip operand (rezultat) care intoarce inaltimea unui arbore binar (adica maximul inaltimilor subarborilor arborelui) <binary_tree>. 
	Arborele vid are prin conventie inaltimea egala cu 0 (inaltimea se defineste ca fiind lungimea celui mai scurt lant elementar (nu se repeta varfuri) de la un varf si pana la radacina arborelui)"
	; (cadr foobar)  <=> (car (cdr foobar))
	; (cddr foobar)  <=> (cdr (cdr foobar))
	; (caddr foobar) <=> (car (cdr (cdr foobar)))
	(cond
		((endp binary_tree) -1)
		((endp (cdr binary_tree)) 0)
		((endp (cadr binary_tree)) (+ 1 (heightBTRec (caddr binary_tree))))
		((endp (cddr binary_tree)) (+ 1 (heightBTRec (cadr binary_tree))))
		((maxim (+ 1 (heightBTRec (cadr binary_tree))) (+ 1 (heightBTRec (caddr binary_tree)))))
	)
)

(defun raiseValidErr ()
	(error "Argument invalid: arborele binar trebuie sa fie reprezentat sub forma unei liste (neliniare)!")
)

;; Model matematic pentru functia heightBT:
;; heightBT(binary_tree) = { heightBTRec(binary_tree), daca binary_tree este lista (neliniara)
;;                         { raiseValidErr()         , altfel (adica daca binary_tree nu este lista (este atom))
(defun heightBT (binary_tree)
	;(if (listp binary_tree) (heightBTRec binary_tree) (error "Argument invalid: arborele binar trebuie sa fie reprezentat sub forma unei liste (neliniare)!"))
	(if (listp binary_tree) (heightBTRec binary_tree) (raiseValidErr))
)

(defun heightBTUI (binary_tree)
	(format nil "Inaltimea arborelui binar ~A este: ~A" binary_tree (heightBT binary_tree))
)

;; Model matematic pentru functia modul:
;; modul(num) = { false (nil)                   , daca num nu este numar (atom numeric)
;;              { -num (num * (-1) = (-1) * num), daca num este numar (atom numeric) si num < 0
;;              { num                           , altfel (adica daca num este numar (atom numeric) si num >= 0)
(defun modul (num)
	"Functie utilizator/user cu nume de tip operand (rezultat) care intoarce/returneaza valoarea absoluta (modulul) 
	unui argument (parametru de intrare) <num> daca acesta este un atom numeric (numar) sau nil (false = fals) in caz contrar"
	(if (not (numberp num)) nil (if (< num 0) (modul (* -1 num)) num))
)

;; Model matematic pentru functia isBalanced:
;; isBalanced(binary_tree) =
;; isBalanced(l1l2l3)      = { true (adevarat)                 , daca binary_tree este lista vida (binary_tree = ())
;;                           { false (fals)                    , daca binary_tree nu este lista vida (binary_tree <> ()) si modul(heightBT(l2) - heightBT(l3)) > 1 (>= 2)
;;                           { isBalanced(l2) si isBalanced(l3), altfel, adica daca binary_tree nu este lista vida (binary_tree != ()) si modul(heightBT(l2) - heightBT(l3)) <= 1 (< 2)
(defun isBalanced (binary_tree)
	"Functie utilizator/user cu nume de tip predicat (predicate) care testeaza/verifica daca un arbore binar <binary_tree> este sau nu echilibrat (balanced)"
	(cond
		((null binary_tree) t)
		((> (modul (- (heightBT (cadr binary_tree)) (heightBT (caddr binary_tree)))) 1) nil)
		(t (and (isBalanced (cadr binary_tree)) (isBalanced (caddr binary_tree))))
	)
)

(defun isBalancedUI (binary_tree)
	;; VARIANTA I
	;(if (not (isBalanced binary_tree)) (format nil "Arborele binar introdus NU ESTE balansat (balanced) pe inaltime!") (format nil "Arborele binar introdus ESTE balansat (balanced) pe inaltime!"))
	
	;; VARIANTA II
	(cond
		;((not (listp binary_tree)) (error "Argument invalid: arborele binar trebuie sa fie reprezentat sub forma unei liste (neliniare)!"))
		((not (listp binary_tree)) (raiseValidErr))
		('t (format nil "Arborele binar ~A ~A balansat (balanced) pe inaltime!" binary_tree (if (isBalanced binary_tree) "ESTE" "NU ESTE")))
	)
)