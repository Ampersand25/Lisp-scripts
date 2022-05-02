;7.

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;a) Sa se scrie o functie care testeaza daca o lista este liniara.

; Rezolvare subpunctul a)
; Conventii/Notatii pseudocod:
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara cu n elemente/componente (membrii)
; Vom simboliza prin () - lista vida (lista cu 0 elemente)
; Avem urmatorul model matematic (formula recursiva/recurenta) pentru functia user/utilizator testLinear
;(functie de tip predicat/predicate (returneaza o valoare booleana (valoare logica de adevar, adica true/adevarat (t) sau false/fals (nil)))):
; testLinear(L)         =
; testLinear(l1l2...ln) = { t   = true (adevarat), daca n = 0 (adica L = ())
;                         { nil = false (fals)   , daca n <> 0 (L <> ()) si l1 este lista
;	                      { testLinear(l2...ln)  , daca n <> 0 (L <> ()) si l1 nu este lista (este atom (numeric (numar) sau nenumeric (adica simbolic)))
(defun testLinear (lista)
	"Functie booleana de tip operand (rezultat) care testeaza (verifica) daca o lista (neliniara) <lista> este sau nu liniara"
	(cond
		((null lista) 't)            ; <=> ((endp lista) t)
		((listp (car lista)) 'nil)   ; <=> ((not (atom (car lista))) nil)
		(t (testLinear (cdr lista))) ; <=> ((testLinear (cdr lista)))
	)
)

(defun isLinear (lista)
	"Functie apelanta de tip interfata cu utilizatorul (UI = User Interface) care apeleaza functia <testLinear> (functie apelata) 
	si afiseaza pe ecran (in consola/terminal) un mesaj corespunzator liniaritatii listei <lista>"
	(if (testLinear lista) (format nil "Lista ~A ESTE liniara!" lista) (format 'nil "Lista ~A NU ESTE liniara!" lista))
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;b) Definiti o functie care substituie prima aparitie a unui element intr-o lista data.

; Rezolvare subpunctul b)
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara cu n elemente/componente (membrii)
; Conventie pseudocod: vom reprezenta lista vida (lista fara niciun element) prin notatia ()
; elem   - element (posibil membru al listei)
; (elem) - lista cu un singur membru formata doar din elementul/componenta elem
; De asemenea, vom utiliza notatia (+) pentru operatia de concatenare a continutului a doua liste
;(rezultatul va fi tot o lista care va contine elementele primei liste urmate de elementele celei de a doua liste)
; Prin simbolul + vom reprezenta operatia de adaugare (append) a unui element la inceputul sau finalul/sfarsitul unei liste neliniare
; Descriem functia user/utilizator replaceFirstApList (functie determinista (are un singur rezultat (adica o singura solutie))) prin urmatorul model matematic:
; replaceFirstApList(a, b, L)                 =
; replaceFirstApList(a, b, (l1, l2, ..., ln)) = { ()                                                  , daca n = 0 (adica L = ())
;                                               { (l1) (+) replaceFirstApList(a, b, (l2, l3, ..., ln)), daca n <> 0 (n != 0) si l1 <> a (l1 != a)
;                                               { b + (l2, l3, ..., ln)                               , daca n <> 0 (n != 0) si l1 = a
(defun replaceFirstApList (a b lista)
	"Functie de tip operand (rezultat) care inlocuieste/substituie prima aparitie a elementului <a> cu elementul <b> in lista <lista> sau 
	intoarce lista neschimbata/nemodificata in cazul in care elementul <a> nu este un membru al listei <lista>"
	(cond
		((endp lista) ())                                                                     ; <=> ((null lista) '())
		((not (equal (car lista) a)) (cons (car lista) (replaceFirstApList a b (cdr lista))))
		((cons b (cdr lista)))                                                                ; <=> (t (cons b (cdr lista)))
	)
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;c) Sa se inlocuiasca fiecare sublista a unei liste cu ultimul ei element. Prin sublista se intelege element de pe primul nivel, care este lista.
;Exemplu: (a (b c) (d (e (f)))) ==> (a c (e (f))) ==> (a c (f)) ==> (a c f)
;(a (b c) (d ((e) f))) ==> (a c ((e) f)) ==> (a c f)

; Rezolvare subpunctul c)
; Functie utilizator auxiliara (vom apela aceasta functie in functia principala/main (apelanta) transformList care va fi definita ulterior)
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara (in care elementele pot sa fie la randul lor liste (subliste)) cu n elemente/componente (membrii)
; Consideram urmatoarele conventii in descrierea comportamentului functiei (algoritmului):
; () - lista vida (lista cu 0 elemente)
; +  - operator pentru operatia binara de inserare/adaugare a unui element intr-o lista
; Exemplu utilizare operator "+":
; a + L = a + l1l2...ln = al1l2...ln - adaugare la inceput (inserare pe prima pozitie <=> inainte de primul element din lista L (adica l1))
; L + a = l1l2...ln + a = l1l2...lna - adaugare la final/sfarsit (inserare pe ultima pozitie <=> dupa ultimul element din lista L (adica ln))
; Modelul matematic (formula recursiva/recurenta) al functiei user (utilizator) cu numele lastElementFromList este urmatorul:
; lastElementFromList(L)         =
; lastElementFromList(l1l2...ln) = { ()                          , daca n = 0 (L = ())
;                                  { l1                          , daca n = 1 si l1 nu este lista (este atom sau numar) (l1 este singurul element ramas in lista, adica ultimul element din lista L = l1...ln (elementul ln))
;                                  { lastElementFromList(l1)     , daca n = 1 si l1 este lista (nu este nici atom si nici numar) (l1 este singurul element ramas in lista, adica ultimul element din lista L = l1...ln (elementul ln))
;                                  { lastElementFromList(l2...ln), altfel (adica daca n > 1 <=> n >= 2)
(defun lastElementFromList (lista)
	"Functie de tip operand (rezultat) care returneaza/intoarce ultimul element (care va fi un atom) dintr-o lista neliniara data <lista>"
	(cond
		((null lista) (list))                                            ; <=> ((endp lista) (append))
		((and (null (cdr lista)) (not (listp (car lista)))) (car lista)) ; <=> ((and (endp (cdr lista)) (atom (car lista))) (car lista))
		((null (cdr lista)) (lastElementFromList (car lista)))           ; <=> ((and (endp (cdr lista)) (not (atom (car lista)))) (lastElementFromList (car lista)))
		(t (lastElementFromList (cdr lista)))                            ; <=> ((lastElementFromList (cdr lista)))
	)
)

; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara (poate sa contina liste/subliste) cu n elemente/componente (membrii)
; Consideram urmatoarele notatii pseudocod:
; () - lista vida (lista de aritate 0 (fara vreun element))
; +  - operator pentru operatia binara de inserare/adaugare a unui element intr-o lista
; Exemplu utilizare operator "+":
; a + L = a + (l1, l2, ..., ln) = (a, l1, l2, ..., ln) - adaugare la inceput (inserare pe prima pozitie <=> inainte de primul element din lista L (adica l1))
; L + a = (l1, l2, ..., ln) + a = (l1, l2, ..., ln, a) - adaugare la final/sfarsit (inserare pe ultima pozitie <=> dupa ultimul element din lista L (adica ln))
; Modelul matematic (formula recursiva/recurenta) al functiei user (utilizator) cu numele transformList este urmatorul:
; transformList(L)         =
; transformList(l1l2...ln) = { ()                                              , daca n = 0 (L = ())
;                            { l1 + transformList(l2...ln)                     , daca n <> 0 (L <> ()) si l1 nu este lista (este atom (simbolic) sau numar)
;                            { lastElementFromList(l1) + transformList(l2...ln), daca n != 0 (L != ()) si l1 este lista (nu este atom (simbolic) si nici numar)
(defun transformList (lista)
	"Functie de tip operand/rezultat care inlocuieste fiecare sublista (element de tip lista) a unei liste neliniare 
	(componentele ei pot sa fie la randul lor liste) <lista> cu ultimul ei element (membru) care este atom (numeric (numar) sau nenumeric/simbolic (simbol))"
	(cond
		((null lista) '())
		((atom (car lista)) (cons (car lista) (transformList (cdr lista))))
		(t (cons (lastElementFromList (car lista)) (transformList (cdr lista))))
	)
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;d) Definiti o functie care interclaseaza fara pastrarea dublurilor doua liste liniare sortate.

; Rezolvare subpunctul d)
; Fie A = a1a2...an = (a1, a2, ..., an) - o lista liniara cu n elemente/componente (membrii)
;     B = b1b2...bm = (b1, b2, ..., bm) - o lista liniara cu m elemente/componente (membrii)
; Conventii:
; () - lista vida (lista cu 0 elemente)
; Modelul matematic al functiei utilizator mergeLists este urmatorul:
; mergeLists(A, B)                 =
; mergeLists(a1a2...an, b1b2...bm) = { ()                          , daca n = 0 (A = ()) si m = 0 (B = ())
;                                    { B (b1b2...bm)               , daca n = 0 (A = ()) si m <> 0 (B <> ())
;                                    { A (a1a2...an)               , daca m = 0 (B = ()) si n <> 0 (A <> ())
;                                    { a1 + mergeLists(a2...an, B) , daca n != 0 (A != ()), m != 0 (B != ()) si a1 < b1 (b1 > a1)
;                                    { b1 + mergeLists(A, b2...bm) , daca n != 0 (A != ()), m != 0 (B != ()) si a1 > b1 (b1 < a1)
;                                    { mergeLists(a2...an, b2...bm), altfel (adica daca n <> 0 (A <> ()), m <> 0 (B <> ()) si a1 = b1 (b1 = a1))
(defun mergeLists (lista_1 lista_2)
	"Functie de tip operand/rezultat care intoarce lista rezultata in urma interclasarii (merging (operatie de merge)) listelor 
	liniare (elementele/membrii sunt atom, ci nu liste) <lista_1> si <lista_2> care trebuie sa fie sortate/ordonate in ordine crescatoare"
	(cond
		; 'nil <=> nil <=> () <=> '() <=> (list) <=> (append)
		((and (null lista_1) (null lista_2)) 'nil)
		((null lista_1) lista_2)
		((null lista_2) lista_1)
		((< (car lista_1) (car lista_2)) (cons (car lista_1) (mergeLists (cdr lista_1) lista_2)))
		((> (car lista_1) (car lista_2)) (cons (car lista_2) (mergeLists lista_1 (cdr lista_2))))
		(t (mergeLists (cdr lista_1) (cdr lista_2)))
	)
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------