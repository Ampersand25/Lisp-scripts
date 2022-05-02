;7.

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;a) Sa se scrie o functie care testeaza daca o lista este liniara.

; Rezolvare subpunctul a)
; Conventii/Notatii pseudocod:
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara cu n elemente/componente (membrii)
; Vom simboliza prin () - lista vida (lista cu 0 elemente)
; Avem urmatorul model matematic (formula recursiva/recurenta) pentru functia user/utilizator <testLinear> 
; (functie de tip predicat/predicate (returneaza o valoare booleana (valoare logica de adevar, adica true/adevarat (t) sau false/fals (nil)))):
; testLinear(L)         =
; testLinear(l1l2...ln) = { t   = true (adevarat), daca n = 0 (adica L = ())
;                         { nil = false (fals)   , daca n <> 0 (L <> ()) si l1 este lista (l1: list)
;	                      { testLinear(l2...ln)  , daca n <> 0 (L <> ()) si l1 nu este lista (este atom (numeric (numar) sau nenumeric (adica simbolic (simbol))))
(defun testLinear (lista)
	"[!]Documentatie pentru functia utilizator (user) <testLinear>
	- Descriere succinta: functie booleana de tip operand (rezultat) care testeaza (verifica) daca o lista (neliniara) <lista> este sau nu liniara
	- Input (parametrii de intrare): <lista> - lista (neliniara) de atomi (simbolici/nenumerici sau numerici) si liste (subliste (inner lists) ale listei initiale (outer list))
	- Preconditii (restrictii impuse asupra datelor de intrare): -
	- Output (parametrii de iesire): boolean - valoare logica de adevar (adica t sau nil)
	- Postconditii (descrierea rezultatelor): t (true/adevarat) - daca argumentul <lista> este o lista liniara (adica contine doar atomi)
	                                          nil (false/fals)  - in caz contrar, adica daca argumentul <lista> contine si/numai liste (subliste), adica are cel putin un membru/element de tip list (lista)"
	(cond
		((null lista) 't)             ; <=> ((endp lista) t)
		((listp (car lista)) 'nil)    ; <=> ((not (atom (car lista))) nil)
		('t (testLinear (cdr lista))) ; <=> ((testLinear (cdr lista)))
	)
)

(defun isLinear (lista)
	"Functie apelanta de tip interfata cu utilizatorul (UI = User Interface) de tip consola care apeleaza functia <testLinear> (functie apelata) 
	si afiseaza pe ecran (in consola/terminal) un mesaj corespunzator proprietatii de liniaritate a listei <lista> data ca si argument (parametru de intrare pentru functia curenta)"
	(if (testLinear lista) (format nil "Lista ~A ESTE liniara!" lista) (format 'nil "Lista ~A NU ESTE liniara!" lista))
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;b) Definiti o functie care substituie prima aparitie a unui element intr-o lista data.

; Rezolvare subpunctul b)
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara cu n elemente/componente (membrii)
; Conventie pseudocod: vom reprezenta lista vida (lista fara niciun element) prin notatia ()
; Descriem functia user/utilizator <membership> (functie cu nume care verifica daca un element se afla intr-o lista neliniara pe orice nivel (nu doar la nivelul superficial al acesteia)) prin urmatorul model recursiv (model/formula de recurenta):
; membership(elem, L)         =
; membership(elem, l1l2...ln) = { false (fals)                                        , daca n = 0 (L vida <=> L = ())
;                               { true (adevarat)                                     , daca n <> 0 (n != 0 <=> L nevida <=> L <> ()) si l1 = elem
;                               { membership(elem, l1) SAU membership(elem, l2l3...ln), daca n <> 0, l1 <> elem si l1 este lista (neliniara)
;                               { membership(elem, l2l3...ln)                         , altfel (adica daca n <> 0, l1 <> elem si l1 nu este lista)
(defun membership (elem lista)
	"[!]Documentatie pentru functia utilizator (user) <membership>
	- Descriere succinta: functie de tip predicat (predicate) care verifica/testeaza daca un element <elem> dat este sau nu membru al unei liste <lista> date (<lista> nu este neaparat o lista liniara (poate sa fie si neliniara pentru a nu restrange generalitatea solutiei/rezolvarii (algoritmului de calcul)))
	- Input (parametrii de intrare): <elem>  - s-expresie (atom, lista, pereche cu punct)
	                                 <lista> - lista (neliniara) de elemente de tipul lui <elem>
	- Preconditii (restrictii impuse asupra datelor de intrare): <lista> nu poate sa fie un atom (trebuie sa fie neaparat o lista (liniara sau neliniara))
	- Output (parametrii de iesire): boolean (valoare logica de adevar, adica valoare booleana: true/adevarat (t in terminologia Lisp) sau false/fals (nil in terminologia Lisp))
	- Postconditii (descrierea rezultatelor): membership(elem, lista) = true (adevarat), daca elementul <elem> se afla (exista) in lista <lista> (este un membru al acestei liste)
	                                                                  = false (fals)   , in caz contrar (adica daca <elem> nu se afla pe niciun nivel in lista neliniara <lista>)"
	(cond
		((not lista) nil)
		((equal elem (car lista)) t)
		((listp (car lista)) (or (membership elem (car lista)) (membership elem (cdr lista))))
		(t (membership elem (cdr lista)))
	)
)

; Conventie pseudocod: vom reprezenta lista vida (lista fara niciun element) prin notatia () (lista cu 0 elemente/membrii)
; elem   - element (posibil membru al listei date)
; (elem) - lista cu un singur membru formata doar din elementul/componenta elem
; De asemenea, vom utiliza notatia (+) pentru operatia de concatenare a continutului a doua liste 
; (in urma aplicarii acestui operator binar pe doua liste va rezulta o a treia lista (lista rezultat (rezultatul operatiei)) care va contine prima oara toate elementele din prima lista (primul operand, adica operandul stang) urmate de toate elementele celei de a doua liste (al doilea operand, adica operandul drept))
; Prin simbolul + vom reprezenta operatia de adaugare (append) a unui element la inceputul sau finalul/sfarsitul unei liste neliniare (in functie de cum se realizeaza apelul, adica in functie de ordinea operanzilor)
; Descriem functia user/utilizator <replaceFirstApList> (functie determinista (are un singur rezultat (adica o singura solutie))) prin urmatorul model matematic (model recursiv):
; replaceFirstApList(a, b, L)                 =
; replaceFirstApList(a, b, (l1, l2, ..., ln)) = { ()                                                  , daca n = 0 (adica L = ())
;                                               { b + (l2, l3, ..., ln)                               , daca n <> 0 (n != 0) si l1 = a
;                                               { replaceFirstApList(a, b, l1) + (l2, l3, ..., ln)    , daca n <> 0 (n != 0), l1 <> a (l1 != a), l1 este lista (l1: list) si membership(a, l1) = t = true/adevarat (dovada ca elementul <a> apare in lista <l1> (pe orice nivel in cadrul acesteia))
;                                               { (l1) (+) replaceFirstApList(a, b, (l2, l3, ..., ln)), altfel (adica daca n <> 0 (n != 0), l1 <> a (l1 != a) si l1 nu este lista (este atom (numeric sau simbolic sau sir de caractere)) sau este lista (neliniara) si nu contine elementul a ca si membru (membership(a, l1) = nil = false/fals))
(defun replaceFirstApList (a b lista)
	"[!]Documentatie pentru functia utilizator (user) <replaceFirstApList>
	- Descriere succinta: functie de tip operand (rezultat) care inlocuieste/substituie prima aparitie a elementului <a> cu elementul 
	<b> in lista <lista> sau intoarce lista neschimbata/nemodificata in cazul in care elementul <a> nu este un membru al listei <lista>
	- Input (parametrii de intrare): <a>     - s-expresie (atom, pereche cu punct sau lista (neliniara))
	                                 <b>     - s-expresie (atom, pereche cu punct sau lista (neliniara))
	                                 <lista> - lista (nu neaparat liniara)
	- Preconditii (restrictii impuse asupra datelor de intrare): <lista> nu poate sa fie un atom (trebuie sa fie neaparat o lista)
	- Output (parametrii de iesire): list (lista neliniara)
	- Postconditii (descrierea rezultatelor): functia intoarce o lista neliniara obtinuta din lista <lista> in urma inlocuirii/substituirii membrului <a> cu elementul <b>
	                                          in cazul in care argumentul (parametrul de intrare) <a> nu este un membru in lista <lista> atunci functia intoarce chiar argumentul <lista> (nemodificat)"
	(cond
		; (endp lista) <=> (not lista) <=> (null lista)
		((endp lista) ())
		((equal (car lista) a) (cons b (cdr lista)))
		((and (listp (car lista)) (membership a (car lista))) (cons (replaceFirstApList a b (car lista)) (cdr lista)))
		((cons (car lista) (replaceFirstApList a b (cdr lista))))
	)
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;c) Sa se inlocuiasca fiecare sublista a unei liste cu ultimul ei element. Prin sublista se intelege element de pe primul nivel, care este lista.
;Exemplu: (a (b c) (d (e (f)))) ==> (a c (e (f))) ==> (a c (f)) ==> (a c f)
;(a (b c) (d ((e) f))) ==> (a c ((e) f)) ==> (a c f)

; Rezolvare subpunctul c)
; Functie utilizator auxiliara (vom apela aceasta functie in functia principala/main (apelanta) <transformList> care va fi definita ulterior)
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara (in care elementele pot sa fie la randul lor liste (subliste)) cu n elemente/componente (membrii)
; Consideram urmatoarele conventii in descrierea comportamentului functiei (specificarea algoritmului):
; () - lista vida (lista cu 0 elemente)
; +  - operator pentru operatia binara de inserare/adaugare a unui element intr-o lista (neliniara)
; Exemplu utilizare operator "+":
; a + L = a + l1l2...ln = al1l2...ln - adaugare la inceput (inserare pe prima pozitie <=> inainte de primul element din lista L (adica inainte de membrul l1 al listei L))
; L + a = l1l2...ln + a = l1l2...lna - adaugare la final/sfarsit (inserare pe ultima pozitie <=> dupa ultimul element din lista L (adica dupa membrul ln al listei L))
; Modelul matematic (formula recursiva/recurenta) pentru functiei user (utilizator) cu numele <lastElementFromList> este urmatorul:
; lastElementFromList(L)         =
; lastElementFromList(l1l2...ln) = { ()                          , daca n = 0 (L = ())
;                                  { l1                          , daca n = 1 si l1 nu este lista (este atom (numeric, simbolic sau sir de caractere)) (l1 este singurul element ramas in lista, adica ultimul element din lista initiala L = l1...ln (elementul ln))
;                                  { lastElementFromList(l1)     , daca n = 1 si l1 este lista (nu este atom (nici numeric si nici nenumeric)) (l1 este singurul element ramas in lista, adica ultimul element din lista initiala L = l1...ln (elementul ln))
;                                  { lastElementFromList(l2...ln), altfel (adica daca n > 1 <=> n >= 2)
(defun lastElementFromList (lista)
	"[!]Documentatie pentru functia utilizator (user) <lastElementFromList>
	- Descriere succinta: functie de tip operand (rezultat) care returneaza/intoarce ultimul element (care va fi un atom) dintr-o lista neliniara data <lista>
	- Input (parametrii de intrare): <lista> - list (lista neliniara (lista de s-expresii (atomi, perechi cu punct si liste)))
	- Preconditii (restrictii impuse asupra datelor de intrare): argumentul (parametrul de intrare) <lista> nu poate sa fie atom (nici numeric, dar nici nenumeric/simbolic)
	- Output (parametrii de iesire): atom (atom numeric (numar), sir de caractere sau atom sibolic (simbol))
	- Postconditii (descrierea rezultatelor): functia intoarce atomul care reprezinta ultimul membru al listei <lista> data ca si parametru de intrare functiei
	                                          lastElementFromList(L) = lastElementFromList(l1l2...ln) = rez, unde rez este ultimul atom din lista L"
	(cond
		((null lista) (list))                                            ; <=> ((endp lista) (append))
		((and (null (cdr lista)) (not (listp (car lista)))) (car lista)) ; <=> ((and (not (rest lista)) (atom (first lista))) (first lista))
		((null (cdr lista)) (lastElementFromList (car lista)))           ; <=> ((and (not (rest lista)) (not (atom (first lista)))) (lastElementFromList (first lista)))
		(t (lastElementFromList (cdr lista)))                            ; <=> ((lastElementFromList (rest lista)))
	)
)

; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista neliniara (poate sa contina liste/subliste) cu n elemente/componente (membrii)
; Consideram urmatoarele notatii pseudocod:
; () - lista vida (lista de aritate 0 (fara vreun element))
; +  - operator pentru operatia binara de inserare/adaugare a unui element intr-o lista
; Exemplu utilizare operator "+":
; a + L = a + (l1, l2, ..., ln) = (a, l1, l2, ..., ln) - adaugare la inceputul listei L (inserare pe prima pozitie <=> inaintea primului element din lista L (adica inaintea lui l1))
; L + a = (l1, l2, ..., ln) + a = (l1, l2, ..., ln, a) - adaugare la finalul/sfarsitul listei L (inserare pe ultima pozitie <=> dupa ultimul element din lista L (adica dupa ln))
; Modelul matematic (formula recursiva/recurenta) al functiei user (utilizator) cu numele <transformList> este urmatorul:
; transformList(L)         =
; transformList(l1l2...ln) = { ()                                              , daca n = 0 (L = ())
;                            { l1 + transformList(l2...ln)                     , daca n <> 0 (L <> ()) si l1 nu este lista (este atom (de tip atomic))
;                            { lastElementFromList(l1) + transformList(l2...ln), daca n != 0 (L != ()) si l1 este lista (nu este atom)
(defun transformList (lista)
	"[!]Documentatie pentru functia utilizator (user) <transformList>
	- Descriere succinta: functie de tip operand/rezultat care inlocuieste fiecare sublista (element de tip lista) a unei liste neliniare 
	(componentele ei pot sa fie la randul lor liste) <lista> cu ultimul ei element (membru) care este atom (numeric (numar) sau nenumeric: atom simbolic (simbol) sau sir de caractere)
	- Input (parametrii de intrare): <lista> - lista neliniara (poate sa contina atat liste (subliste) cat si atomi sau perechi cu punct (pp))
	- Preconditii (restrictii impuse asupra datelor de intrare): argumentul (parametrul de intrare) <lista> nu poate sa fie atom (nici numeric, dar nici nenumeric (simbolic sau sir de caractere))
	- Output (parametrii de iesire): lista liniara care contine doar atomi (nu si liste sau perechi cu punct)
	- Postconditii (descrierea rezultatelor): functia intoarce o lista liniara obtinuta din argumentul <lista> de tip lista prin inlocuirea fiecarei subliste cu ultimul ei element/membru (restul elementelor de tip atomic vor ramane nealterate (nu vor fi sterse din lista sau modificate))"
	(cond
		((not lista) '())                                                                ; <=> ((null lista) ())
		((atom (car lista)) (cons (car lista) (transformList (cdr lista))))              ; <=> ((not (listp (first lista))) (cons (first lista) (transformList (rest lista))))
		((not nil) (cons (lastElementFromList (car lista)) (transformList (cdr lista)))) ; <=> ('t (cons (lastElementFromList (first lista)) (transformList (rest lista))))
	)
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;d) Definiti o functie care interclaseaza fara pastrarea dublurilor doua liste liniare sortate.

; Rezolvare subpunctul d)
; Fie L = l1l2...ln = (l1, l2, ..., ln) - o lista liniara (nu contine subliste) cu n elemente/componente (membrii) care sunt atomi
; Conventii pseudocod:
; Consideram + ca fiind operatia binara de adaugare/inserare a unui element intr-o lista (la inceputul sau finalul/sfarsitul acesteia in functie de ordinea parametrilor)
; elem - s-expresie (atom, lista (neliniara) sau pereche cu punct)
; L    - lista (neliniara)
; elem + L => adaugam elementul <elem> la inceputul listei <L>
; L + elem => adaugam elementul <elem> la finalul/sfarsitul listei <L>
; Modelul recursiv/recurent al functiei utilizator cu nume <removeDuplicates> este urmatorul:
; removeDuplicates(L)         =
; removeDuplicates(l1l2...ln) = { L                             , daca n <= 1 (n = 0 (L = ()) sau n = 1)
;                               { removeDuplicates(l2...ln)     , daca n > 1 (n >= 2) si l1 = l2
;                               { l1 + removeDuplicates(l2...ln), altfel (daca n > 1 (n >= 2) si l1 <> l2 (l1 != l2))
(defun removeDuplicates (lista)
	"[!]Documentatie pentru functia utilizator (user) <mergeListsWrapper>
	- Descriere succinta: functie de tip operand (rezultat) care intoarce o lista obtinuta din lista <lista> (data ca si argument la apelul functiei) prin eliminarea dublurilor, 
	cu alte cuvinte, functia \"transforma\" out-of-place (not-in-place (not-in-space), folosind memorie aditionala/suplimentara) parametrul <lista> de tip lista (liniara) intr-o multime (set) 
	(asta daca dublurile din lista initiala se afla pe pozitii consecutive/succesive, altfel (in caz contrar) lista finala/rezultat va contine in continuare duplicate care nu vor fi vecine (deci lista rezultata nu va mai fi o multime/set))
	- Input (parametrii de intrare): <lista> - lista liniara
	- Preconditii (restrictii impuse asupra datelor/parametrilor de intrare): lista <lista> trebuie sa contina elemente duplicate (dubluri) pe pozitii succesive/consecutive (adica daca doua sau mai multe elemente au aceeasi valoare atunci vor fi vecine in cadrul listei (se vor afla pe pozitii alaturate))
	- Output (parametrii de iesire sau rezultate): lista liniara (nu neaparat numerica) 
	- Postconditii (descrierea rezultatelor (datelor/parametrilor de iesire)): subprogramul returneaza/intoarce/furnizeaza lista obtinuta prin transformarea listei <lista> intr-o multime (stergand/eliminand toate dublurile din aceasta (se pastreaza un singur element si se renunta la duplicatele acestuia))"
	(cond
		((or (endp lista) (endp (cdr lista))) lista)
		((equal (car lista) (cadr lista)) (removeDuplicates (cdr lista)))
		((cons (car lista) (removeDuplicates (cdr lista))))
	)
)

; Fie A = a1a2...an = (a1, a2, ..., an) - o lista liniara cu n elemente/componente (membrii)
;     B = b1b2...bm = (b1, b2, ..., bm) - o lista liniara cu m elemente/componente (membrii)
; Conventii:
; () - lista vida (lista cu 0 elemente)
; Modelul matematic al functiei utilizator <mergeLists> este urmatorul:
; mergeLists(A, B)                 =
; mergeLists(a1a2...an, b1b2...bm) = { ()                               , daca n = 0 (A = ()) si m = 0 (B = ())
;                                    { B (b1b2...bm)                    , daca n = 0 (A = ()) si m <> 0 (B <> ())
;                                    { A (a1a2...an)                    , daca m = 0 (B = ()) si n <> 0 (A <> ())
;                                    { a1 + mergeLists(a2...an, B)      , daca n != 0 (A != ()), m != 0 (B != ()) si a1 < b1 (b1 > a1)
;                                    { b1 + mergeLists(A, b2...bm)      , daca n != 0 (A != ()), m != 0 (B != ()) si a1 > b1 (b1 < a1)
;                                    { a1 + mergeLists(a2...an, b2...bm), altfel (adica daca n <> 0 (A <> ()), m <> 0 (B <> ()) si a1 = b1 (b1 = a1))
;                                      sau
;                                    { b1 + mergeLists(a2...an, b2...bm), altfel (adica daca n <> 0 (A <> ()), m <> 0 (B <> ()) si a1 = b1 (b1 = a1))
(defun mergeLists (lista_1 lista_2)
	"[!]Documentatie pentru functia utilizator (user) <mergeLists>
	- Descriere succinta: functie de tip operand/rezultat care intoarce lista rezultata in urma interclasarii (merging (operatie de merge)) listelor liniare (elementele/membrii sunt atomi, ci nu liste sau perechi cu punct) 
	<lista_1> si <lista_2> care trebuie sa fie sortate/ordonate in ordine crescatoare (nu neaparat strict crescator deoarece pot exista dubluri in cele doua liste date ca si argumente pentru functia descrisa)
	- Input (parametrii de intrare): <lista_1> - lista liniara (nu contine liste/subliste sau pp (perechi cu punct))
	                                 <lista_2> - lista liniara (nu contine liste/subliste sau pp (perechi cu punct))
	- Preconditii (restrictii impuse asupra datelor de intrare): <lista_1> si <lista_2> nu pot sa aiba membrii de tip lista (trebuie sa fie liste liniara), perechi cu punct (pp) sau atomi nenumerici (simbolici)
	                                                             <lista_1> si <lista_2> trebuie sa aiba elementele (atomi numerici (numere)) sortate/ordonate crescator (adica daca L = l1l2...ln, avem ca l1 <= l2 <= ... <= ln <=> li <= lj, oricare ar fi i < j (i <= j) si i, j <- 1, n (i si j apartin multimii Nn de indici))
	- Output (parametrii de iesire): list (lista liniara care contine doar atomi numerici (adica numere)), aceasta lista poate sa contina elemente duplicate (dar doar pe pozitii consecutive/succesive)
	- Postconditii (descrierea rezultatelor): functia intoarce lista obtinuta prin interclasarea celor doua liste liniare si sortate <lista_1> si <lista_2> fara pastrarea dublurilor 
											  (adica daca un element este membru atat al listei <lista_1> cat si al listei <lista_2> atunci acesta va aparea o singura data in lista rezultat, ci nu de doua sau mai multe ori (exceptie daca elementul apare de mai multe ori in una dintre cele doua liste)). 
	                                          Lista rezultata este la randul ei o lista liniara (cu elemente avand tip numeric (atomi numerici)) si sortata/ordonata in mod crescator (nu strict crescator deoarece pot exista elemente duplicate (adica dubluri))"
	(cond
		; 'nil <=> nil <=> () <=> '() <=> (list) <=> (append)
		((and (null lista_1) (null lista_2)) 'nil)
		((not lista_1) lista_2)
		((endp lista_2) lista_1)
		((< (car lista_1) (car lista_2)) (cons (car lista_1) (mergeLists (cdr lista_1) lista_2)))
		((> (car lista_1) (car lista_2)) (cons (car lista_2) (mergeLists lista_1 (cdr lista_2))))
		(t (cons (car lista_1) (mergeLists (cdr lista_1) (cdr lista_2))))
	)
)

; Modelul matematic al functiei utilizator <mergeListsWrapper> cu nume este urmatorul:
; mergeListsWrapper(L1, L2) = removeDuplicates(mergeLists(L1, L2)), unde L1 si L2 sunt doua liste liniare si numerice (alcatuite/formate doar din numere (atomi numerici))
(defun mergeListsWrapper (lista_1 lista_2)
	"[!]Documentatie pentru functia utilizator (user) <mergeListsWrapper>
	- Descriere succinta: functie de tip wrapper care interclaseaza doua liste liniare si sortate (sortate/ordonate crescator) fara pastrarea dublurilor (lista finala nu va contine duplicate)
	- Input (parametrii de intrare): <lista_1>, <lista_2> - liste liniare (nu contin liste/subliste sau pp (perechi cu punct))
	- Preconditii (restrictii impuse asupra datelor/parametrilor de intrare): cele doua argumente de tip lista: <lista_1> si <lista_2> trebuie sa fie liniare, numerice (sa contina doar atomi numerici) si sa aiba elementele in ordine crescatoare (adica sa fie sortate crescator)
	- Output (parametrii de iesire sau rezultate): lista liniara si numerica (contine doar atomi numerici, ci nu si atomi nenumerici: atomi simbolici (simboluri) si siruri de caractere))
	- Postconditii (descrierea rezultatelor (datelor/parametrilor de iesire)): lista rezultat va fi sortata/ordonata in mod strict crescator (si implicit nu va contine elemente duplicate (dubluri), adica fiecare membru al listei va aparea o singura data in aceasta)"
	(removeDuplicates (mergeLists lista_1 lista_2))
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------