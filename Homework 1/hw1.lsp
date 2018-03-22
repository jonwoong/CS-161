;;;;;;;;;; JONATHAN WOONG
;;;;;;;;;; 804205763
;;;;;;;;;; CS 161 - FALL 2017
;;;;;;;;;; HW1

;;;;;;;;;; TREE-CONTAINS ;;;;;;;;;;
; INPUTS: 
;	N = number to search for
;	TREE = tree to traverse
; OUTPUT:
;	T if TREE contains N
; 	NIL otherwise
; BEHAVIOR:
;	Check for whether TREE is currently an atom, if it is, check whether it equals N
; 	If TREE is not an atom, check recurse on the head and tail
(defun TREE-CONTAINS (N TREE)
	(if (atom TREE) ; if TREE contains single number
		(equal TREE N) ; then return T if N==TREE
		(or (TREE-CONTAINS N (car TREE)) ; else search for N in car
			(TREE-CONTAINS N (cdr TREE))))) ; else search for N in cdr

;;;;;;;;;; TREE-MAX ;;;;;;;;;;
; INPUTS: 
; 	TREE = tree to traverse
; OUTPUT:
; 	Max number in tree
; BEHAVIOR:
; 	Since TREE is an ordered list, keep traversing the right side until reaching the max valued leaf
(defun TREE-MAX (TREE)
    (cond 
        ((listp TREE) (TREE-MAX (third TREE))) ; keep traversing right side of tree, return last leaf
        (T TREE))) 

;;;;;;;;;; TREE-ORDER ;;;;;;;;;;
; INPUTS:
;	TREE = tree to traverse
; OUTPUT:
; 	In-ordered list of numbers in TREE
; BEHAVIOR:
; 	Append the head of L to the list containing m and R by recursing on L and R
(defun TREE-ORDER (TREE)
	(cond
		((atom TREE) (list TREE)) ; if single number, return list containing only that number
		(T (append (TREE-ORDER (car TREE)) (cons (second TREE) (TREE-ORDER (third TREE))))))) ; build list by recursing on L and R

;;;;;;;;;; SUB-LIST ;;;;;;;;;;
; INPUTS:
;	L = list to derive sublist from
;	START = position in L to start building sublist
;	LEN = length of sublist to build
; OUTPUTS:
;	A sublist of L that starts at position START and is length L
; BEHAVIOR:
; 	Traverse the list, decrementing START for each elemented visited
; 	Once START is 0, continue traversing list, decrementing LEN while building a new list
; 	Exit when LEN has been decremented to 0
(defun SUB-LIST (L START LEN) 
	(cond 
		((equal 0 LEN) NIL) ; exit case
		((null L) NIL) ; return nil if length of list is 0
		((> START 0) (SUB-LIST (cdr L) (- START 1) LEN)) ; traverse list until reaching start position
		(T (cons (car L) (SUB-LIST (cdr L) START (- LEN 1)))))) ; build list while decrementing LEN

;;;;;;;;;; SPLIT-LIST ;;;;;;;;;;
; INPUT: 
;	L = list to split
; OUTPUT:
; 	A list containing lists L1 and L2, where the length of L2 differs from L1 by 0 or 1
; BEHAVIOR:
; 	If the length of L is even, calculate the MEDIAN on the length of L
;	If the length of L is odd, calculate calculate the MEDIAN on the length of L-1
;	Use sublist to build L1 from position 0 to MEDIAN 
; 	Use sublist to build L2 from position MEDIAN to end
(defun SPLIT-LIST (L) 
	(cond
		((evenp (length L)) ; if the length of L is even
			(let ((MEDIAN (/ (length L) 2))) ; then MEDIAN = L/2
				(list 
					(SUB-LIST L 0 MEDIAN) ; build sublist from 0 to MEDIAN position
					(SUB-LIST L MEDIAN MEDIAN))))  ; build sublist from MEDIAN to end
		(T (let ((MEDIAN (/ (- (length L) 1) 2))) ; if the length of L is odd, MEDIAN = (L-1)/2
				(list
					(SUB-LIST L 0 MEDIAN) ; build sublist from 0 to MEDIAN position
					(SUB-LIST L MEDIAN (+ MEDIAN 1))))))) ; build sublist from MEDIAN to end

;;;;;;;;; BTREE-HEIGHT ;;;;;;;;;;
; INPUT:
;	TREE = binary tree to calculate height of
; OUTPUT:
; 	Length of longest path from root to farthest leaf
; BEHAVIOR:
; 	Traverse the left and right branches using DFS, incremementing LEFT or RIGHT at each level
;	If a left leaf has a higher value than right leaf, the left leaf must be deeper
; 	If a right leaf has a higher value than left leaf, the right leaf must be deeper
(defun BTREE-HEIGHT (TREE)
	(cond 
		((atom TREE) 0) ; base case
		(T (let ((LEFT (BTREE-HEIGHT (first TREE))) (RIGHT (BTREE-HEIGHT (second TREE)))) ; perform DFS, LEFT=left half of tree, RIGHT=right half of tree
			(cond 
				((< RIGHT LEFT) (+ LEFT 1)) ; if number in LEFT > number in RIGHT, the number in LEFT is deeper
				(T (+ RIGHT 1))))))) ; if number in LEFT < number in RIGHT, the number in RIGHT is deeper

;;;;;;;;;; LIST2BTREE ;;;;;;;;;;
; INPUT: 
; 	LEAVES = list of leaves to convert to a tree
; OUTPUT:
;	A binary tree made from LEAVES where the number of leaves in right and left branch differ by 0 or 1
; BEHAVIOR:
; 	Call SPLIT-LIST build list by splitting LEAVES into two halves
(defun LIST2BTREE (LEAVES)
	(cond
		((equal (length LEAVES) 1) (first LEAVES)) ; exit case
		((equal (length LEAVES) 2) LEAVES) ; exit case
    (T (list ; build list
    	(LIST2BTREE (first (SPLIT-LIST LEAVES))) ; split LEAVES in half
    	(LIST2BTREE (second (SPLIT-LIST LEAVES))))))) ; split LEAVES in half

;;;;;;;;;; BTREE2LIST ;;;;;;;;;;
; INPUT:
; 	TREE = binary tree to convert into list
; OUTPUT:
; 	An ordered list containing elements of TREE
; BEHAVIOR:
;	Recursively append the left and right sides of TREE
(defun BTREE2LIST (TREE)
	(cond
		((null TREE) NIL) ; exit case
		((atom TREE) (list TREE)) ; if single number, return it as list
		(T (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))))) ; build list by appending left and right branches

;;;;;;;;;;; IS-SAME ;;;;;;;;;;
; INPUTS:
;	E1 = a lisp expression
; 	E2 = a lisp expression
; OUTPUT:
;	T if E1 == E2
; 	NIL if E1 != E2
; BEHAVIOR:
;	Check for atom equality using =
;	Recurse on E1 and E2 to check equality at atom level 
(defun IS-SAME (E1 E2)
	(cond
		((null E1) (null E2)) 
		((atom E1) (and (atom E2) (= E1 E2))) ; compare 2 atoms
		(T (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))))) ; recursively compare heads and tails of E1 and E2