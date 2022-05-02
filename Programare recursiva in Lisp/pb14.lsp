;14.

; Conventii psudocod:
; Fie lista = l1l2...lm = (l1, l2, ..., lm) - o lista liniara (elementele nu sunt la randul lor liste) de m atomi (atomi simbolici (simboluri) sau numerici (numere))
; Fie () - lista vida (lista cu 0 elemente (adica fara niciun element))
; Notam prin simbolul "+" operatia de adaugare/inserare a unui element intr-o lista (la inceputul sau la finalul/sfarsitul acesteia)
; Exemple: elem + l1l2...lm = elem + (l1, l2, ..., lm) = (elem, l1, l2, ..., lm) - inserare inaintea primului element din lista (adaugare la inceput)
;          l1l2...lm + elem = (l1, l2, ..., lm) + elem = (l1, l2, ..., lm, elem) - inserare dupa ultimul element din lista (adaugare la final/sfarsit)
; Simbolizam prin (+) operatia de concatenare a doua liste (nu neaparat liniare, pot sa fie si neliniare)
; Exemple: avem A = a1a2...an = (a1, a2, ..., an) si B = b1b2...bm = (b1, b2, ..., bm), doua liste neliniare de lungime n (avem n elemente/membrii in lista A) si respectiv m (avem m elemente/membrii in lista B)
;          a1a2...an (+) b1b2...bm = (a1, a2, ..., an) (+) (b1, b2, ..., bm) = (a1, a2, ..., an, b1, b2, ..., bm) = a1a2...anb1b2...bm
;          b1b2...bm (+) a1a2...an = (b1, b2, ..., bm) (+) (a1, a2, ..., an) = (b1, b2, ..., bm, a1, a2, ..., an) = b1b2...bma1a2...an
;          (elem) (+) A = (elem) (+) a1a2...an = (elem) (+) (a1, a2, ..., an) = (elem, a1, a2, ..., an)
;          B (+) (elem) = b1b2...bm (+) (elem) = (b1, b2, ..., bm) (+) (elem) = (b1, b2, ..., bm, elem)

;a) Dandu-se o lista liniare, se cere sa se elimine elementele din N in N.

; Avem urmatorul domeniu al problemei:
; element = atom
; list    = element*

; Signatura functie (descriere antet subprogram):
; deleteNAux(lista: list, N: integer (intreg), idx: integer (intreg)): list

; Modelul matematic (formula recursiva/recurenta) pentru functia deleteNAux:
; deleteNAux(lista, N, idx)     =
; deleteNAux(l1l2...lm, N, idx) = { ()                                  , daca m = 0 (<=> lista = ())
;                                 { deleteNAux(l2...lm, N, 1)           , daca m <> 0 (m != 0) si idx = N
;                                 { l1 + deleteNAux(l2...lm, N, idx + 1), altfel (adica daca m <> 0 (lista <> ()) si idx <> N)

(defun deleteNAux (lista N idx)
	(cond
		((null lista) nil)
		((= idx N) (deleteNAux (cdr lista) N 1))
		(t (cons (car lista) (deleteNAux (cdr lista) N (+ idx 1))))
	)
)

; Signatura functie (descriere antet subprogram):
; deleteN(lista: list, N: integer (intreg)): list

; Modelul matematic (formula recursiva/recurenta) pentru functia deleteN:
; deleteN(lista, N) = deleteNAux(lista, N, 1)

(defun deleteN (lista N)
	(deleteNAux lista N 1)
)

; Signatura functie (descriere antet subprogram):
; deleteNAuxAlt(lista: list, N: integer (intreg), idx: integer (intreg)): list

; Modelul matematic (formula recursiva/recurenta) pentru functia deleteNAuxAlt:
; deleteNAuxAlt(lista, N, idx)     =
; deleteNAuxAlt(l1l2...lm, N, idx) = { ()                                     , daca m = 0 (<=> lista = ())
;                                    { deleteNAuxAlt(l2...lm, N, 1)           , daca m <> 0 (m != 0) si idx = 1
;                                    { l1 + deleteNAuxAlt(l2...lm, N, idx - 1), altfel (adica daca m <> 0 si idx <> N)

(defun deleteNAuxAlt (lista N idx)
	(cond
		((null lista) '())
		((= idx 1) (deleteNAuxAlt (cdr lista) N N))
		(t (cons (car lista) (deleteNAuxAlt (cdr lista) N (- idx 1)))
	)
)

; Signatura functie (descriere antet subprogram):
; deleteNAlt(lista: list, N: integer (intreg)): list

; Modelul matematic (formula recursiva/recurenta) pentru functia deleteNAlt:
; deleteNAlt(lista, N) = deleteNAuxAlt(lista, N, N)

(defun deleteNAlt (lista N)
	(deleteNAuxAlt lista N N)
)

;b) Sa se scrie o functie care sa testeze daca o lista liniara formata din numere intregi are aspect de "vale" (o secvență se spune ca are aspect de "vale" daca elementele descresc pana la un moment dat, apoi cresc.
;De ex. 10 8 6 17 19 20).

; Avem urmatorul domeniu al problemei:
; element = integer (intreg cu semn (signed) = atom numeric intreg)
; list    = element*

; Signatura functie (descriere antet subprogram):
; vale(lista: list, indicator: boolean (true (adevarat) sau false (fals))): boolean (valoare logica de adevar)

; Modelul matematic (formula recursiva/recurenta) pentru functia vale:
; vale(lista, indicator)     =
; vale(l1l2...ln, indicator) = { indicator                 , daca n = 1
;                              { false (fals)              , daca n > 1 (n >= 2) si l1 = l2
;                              { vale(l2l3...ln, true)     , daca n > 1 (n >= 2) si l1 < l2
;                              { false (fals)              , daca n > 1 (n >= 2), l1 > l2 si indicator = true (adevarat)
;                              { vale(l2l3...ln, indicator), altfel (adica daca n > 1 (n >= 2), l1 > l2 si indicator <> true/adevarat (adica indicator = false/fals))

(defun vale (lista indicator)
	(cond
		((null (cdr lista)) indicator)
		((= (car lista) (car (cdr lista))) nil)
		((< (car lista) (car (cdr lista))) (vale (cdr lista) t))
		((equal indicator t) nil)
		(t (vale (cdr lista) indicator))
	)
)

; Signatura functie (descriere antet subprogram):
; valeWrapper(lista: list): boolean (valoare logica de adevar)

; Modelul matematic (formula recursiva/recurenta) pentru functia valeWrapper:

; valeWrapper(lista)     = 
; valeWrapper(l1l2...ln) = { false (fals)      , daca n < 3 (n <= 2) sau l1 <= l2
;                          { vale(lista, false), altfel (adica daca n >= 3 (n > 2) si l1 > l2)

; valeWrapper(lista)     = 
; valeWrapper(l1l2...ln) = { false (fals)      , daca n = 0 (lista = ())
;                          { false (fals)      , daca n = 1 (lista = l1 = (l1))
;                          { false (fals)      , daca n = 2 (lista = l1l2 = (l1, l2))
;                          { false (fals)      , daca n > 2 (n >= 3) si l1 <= l2
;                          { vale(lista, false), altfel

(defun valeWrapper (lista)
	(cond
		((null lista) nil)
		((null (cdr lista)) nil)
		((null (cdr (cdr lista))) nil)
		((or (< (car lista) (car (cdr lista))) (= (car lista) (car (cdr lista)))) nil)
		(t (vale (cdr lista) nil))
	)
)

;c) Sa se construiasca o functie care intoarce minimul atomilor numerici dintr-o lista, de la orice nivel.

; Avem urmatorul domeniu al problemei:
; elementAtom = atom
; listAtom    = elementAtom*
; element     = {atom | listAtom}
; list        = element*

; Signatura functie (descriere antet subprogram):
; calculateMinListRec(lista: list, minim: number): number (atom numeric)

; Modelul matematic (formula recursiva/recurenta) pentru functia calculateMinListRec:
; calculateMinListRec(lista, minim)     =
; calculateMinListRec(l1l2...ln, minim) = { minim                                                         , daca n = 0 (lista = ())
;                                         { calculateMinListRec(l2l3...ln, minim)                         , daca n <> 0 (lista <> ()) si l1 nu este numar si este atom (adica este atom simbolic (simbol), ci nu atom numeric (numar))
;                                         { calculateMinListRec(l2l3...ln, calculateMinListRec(l1, minim)), daca n <> 0 (lista <> ()) si l1 este lista (l1: list)
;                                         { calculateMinListRec(l2l3...ln, l1)                            , daca n <> 0 (lista <> ()), l1 este numar (atom numeric) si 'minim = false (fals) sau l1 < minim'
;                                         { calculateMinListRec(l2l3...ln, minim)                         , altfel (adica daca n <> 0 (lista <> ()), l1 este numar (atom numeric), minim <> false/fals (minim != false) si l1 >= minim)

(defun calculateMinListRec (lista minim)
	(cond
		((null lista) minim)
		((and (not (numberp (car lista))) (atom (car lista))) (calculateMinListRec (cdr lista) minim))
		((listp (car lista)) (calculateMinListRec (cdr lista) (calculateMinListRec (car lista) minim)))
		((or (null minim) (< (car lista) minim)) (calculateMinListRec (cdr lista) (car lista)))
		(t (calculateMinListRec (cdr lista) minim))
	)
)

; Signatura functie (descriere antet subprogram):
; calculateMinList(lista: list): number (numar)

; Modelul matematic (formula recursiva/recurenta) pentru functia calculateMinList:
; calculateMinList(lista) = calculateMinListRec(lista, false)

(defun calculateMinList (lista)
	(calculateMinListRec lista nil)
)

;d) Sa se scrie o functie care sterge dintr-o lista liniara toate aparitiile elementului maxim numeric.

; Avem urmatorul domeniu al problemei:
; element = atom
; list    = element*

; Signatura functie (descriere antet subprogram):
; computeMaxListRec(lista: list, maxim: number): number (numar (atom numeric))

; Modelul matematic (formula recursiva/recurenta) pentru functia computeMaxListRec:
; computeMaxListRec(lista, maxim)     =
; computeMaxListRec(l1l2...ln, maxim) = { maxim                            , daca n = 0 (lista = ())
;                                       { computeMaxListRec(l2...ln, maxim), daca n <> 0 (lista != ()) si l1 nu este de tip number (adica nu este numar (este atom nenumeric sau lista))
;                                       { computeMaxListRec(l2...ln, l1)   , daca n <> 0 (lista != ()), l1 este numar (l1: number) si l1 > maxim
;                                       { computeMaxListRec(l2...ln, maxim), altfel (adica daca n <> 0 (lista != ()), l1 este numar (l1: number) si l1 <= maxim)

(defun computeMaxListRec (lista maxim)
	(cond
		((null lista) maxim)
		((not (numberp (car lista))) (computeMaxListRec (cdr lista) maxim))
		((> (car lista) maxim) (computeMaxListRec (cdr lista) (car lista)))
		(t (computeMaxListRec (cdr lista) maxim))
	)
)

; Signatura functie (descriere antet subprogram):
; computeMaxList(lista: list): number (numar (atom numeric))

; Modelul matematic (formula recursiva/recurenta) pentru functia computeMaxList:
; computeMaxList(lista)     =
; computeMaxList(l1l2...ln) = { false                         , daca n = 0 (lista = ())
;                             { l1                            , daca n = 1 (lista = l1 = (l1))
;                             { computeMaxListRec(l2...ln, l1), altfel (adica daca n > 1 (n >= 2) <=> n <> 0 (n != 0) si n <> 1 (n != 1))

(defun computeMaxList (lista)
	(cond
		((null lista) (not t))
		((null (cdr lista)) (car lista))
		((not nil) (computeMaxListRec (cdr lista) (car lista)))
	)
)

; Signatura functie (descriere antet subprogram):
; removeElement(lista: list, elem: {atom | list}): list

; Modelul matematic (formula recursiva/recurenta) pentru functia removeElement:
; removeElement(lista, elem)     =
; removeElement(l1l2...ln, elem) = { ()                                   , daca n = 0 (lista = ())
;                                  { removeElement(l2...ln, elem)         , daca n <> 0 (lista <> ()) si l1 = elem
;                                  { (l1) (+) removeElement(l2...ln, elem), altfel (adica daca n <> 0 (lista <> ()) si l1 <> elem (l1 != elem))
;                                    <=> l1 + removeElement(l2...ln, elem)

(defun removeElement (lista elem)
	(cond
		((null lista) ())
		((equal (car lista) elem) (removeElement (cdr lista) elem))
		(t (cons (car lista) (removeElement (cdr lista) elem)))
	)
)

; Signatura functie (descriere antet subprogram):
; removeMaxim(lista: list): list

; Modelul matematic (formula recursiva/recurenta) pentru functia removeMaxim:
; removeMaxim(lista) = removeElement(lista, computeMaxList(lista))

(defun removeMaxim (lista)
	(removeElement lista (computeMaxList lista))
)