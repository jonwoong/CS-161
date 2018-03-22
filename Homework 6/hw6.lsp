; JONATHAN WOONG
; 804205763
; CS 161 - FALL 2017
; DIS 1A
; HOMEWORK 6

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; EXERCISE: Fill this function.
;;;;;;;;;;;;;;;;;;;;
;;;;; node2var ;;;;;
;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inputs:
;;;   1. n = node index
;;;   2. c = color index
;;;   3. k = max color index
;;;
;;; Output: the index of the variable
;;;   that corresponds to the fact that 
;;;   "node n gets color c" (when there are k possible colors).
;;;
;;; Behavior: implement the function variable_index(n) = (n-1)*k + c
;;; 
(defun node2var (n c k)
  (+ (* (- n 1) k) c)) ; variable_index(n) = (n-1)*k + c

; EXERCISE: Fill this function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; at-least-one-color ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inputs:
;;;   1. n = node index
;;;   2. c = color index
;;;   3. k = max color index
;;;
;;; Output: *a clause* for the constraint:
;;;     "node n gets at least one color from the set {c,c+1,...,k}."
;;;
;;; Behavior:
;;;   Check if k is invalid or if we have ran out of colors
;;;   Call node2var on n to return the variable index where n gets the color c
;;;   Recurse through the rest of the colors
;;;   Append all of the returned variable indices
;;;   Return the list 
;;; 
(defun at-least-one-color (n c k)
  (cond
    ((< k 1) NIL) ; no colors
    ((> c k) NIL) ; out of colors
    (T (append (list (node2var n c k)) ; a variable index for n getting color c
              (at-least-one-color n (+ c 1) k))))) ; variable indices for n getting the rest of the colors

; EXERCISE: Fill this function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; at-most-one-color ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inputs:
;;;   1. n = node index
;;;   2. c = color index
;;;   3. k = max color index
;;;
;;; Output: *a list of clauses* for the constraint:
;;;   "node n gets at most one color from the set {c,c+1,...,k}."
;;;
;;; Behavior:
;;;   For the case (c==k) or only one color, return clause (variable_index | ~ variable_index)
;;;     because n can either be assigned that color or not
;;;   Call recursive-only-one on the the clause that represents n taking any color
;;;     to build a list of clauses where only one color of n is assignable
;;;   Return that list
;;;
(defun at-most-one-color (n c k)
  (cond
    ((= c k) (list (list (* n k) (- (* n k))))) ; one assignment: (variable_index | ~variable_index)
    (T (recursive-only-one (at-least-one-color n c k))))) ; a list of (~head|~element) for all possible colors for n

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; number-of-tuples ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input: l = a list of pairs
;;;
;;; Output: the number of pairs in l
;;;
;;; Behavior:
;;;   Increment the total by 1 for each head in l
;;;   Recurse on the tail of l
;;;   Return total
;;;
(defun number-of-pairs (l)
  (cond
    ((null l) 0) ; exit case
    (T (+ 1 (number-of-pairs (cdr l)))))) ; +1 for every head in l

;;;;;;;;;;;;;;;;;;;;
;;;;; only-one ;;;;;
;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Inputs:
;;;   1. n = a number
;;;   2. l = a list of numbers
;;;
;;; Output: a list of pairs (~n | ~element) for each element in l
;;;
;;; Behavior: 
;;;   Make a pair where ~n is the head and the ~(first element of l) is the tail
;;;   Recurse on the tail of l
;;;   Append all pairs
;;;   Return list of pairs
;;;
(defun only-one (n l)
  (cond
    ((null l) NIL) ; exit case
    (T (append (list (list (- n) (- (car l)))) ; (~n | ~head)
              (only-one n (cdr l)))))) ; (~n | ~element) for each element in l

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; recursive-only-one ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input: l = a list of possible color assignments
;;; 
;;; Output: a list of pairs that represent (~head | ~element) for each head and element in l
;;;
;;; Behavior:
;;;   Use only-one(~head, tail) to return a list of pairs (~head | ~element) for each element in tail
;;;   Recurse on the tail of l 
;;;   Append each (~head | ~element) from the recursion
;;;   Return the list
;;;
(defun recursive-only-one (l)
  (cond
    ((< (number-of-pairs l) 2) NIL) ; exit case
    (T (append (only-one (car l) (cdr l)) ; (~head | ~element) for every element in l
                (recursive-only-one (cdr l)))))) ; (~next_head | ~element) for every element in l

; EXERCISE: Fill this function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; generate-node-clauses ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inputs:
;;;   1. n = node index
;;;   2. k = maximum color index
;;;
;;; Output: *a list of clauses* to ensure that
;;;   "node n gets exactly one color from the set {1,2,...,k}."
;;;
;;; Behavior: 
;;;   Call at-least-one-color to return the clause where n can take any color
;;;   Call at-most-one-color to return the clauses where n can only take 1 color
;;;   Append the clauses
;;;   Return the list
;;; 
(defun generate-node-clauses (n k)
  (append (list (at-least-one-color n 1 k)) ; clause where n can get any color
          (at-most-one-color n 1 k))) ; clause where n can get at most 1 color

; EXERCISE: Fill this function
; returns 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; generate-edge-clauses ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inputs:
;;;   1. e = (x,y) pair of node indices
;;;   2. k = max color index
;;;
;;; Output: *a list of clauses* to ensure that
;;;   "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;;;
;;; Behavior:
;;;   Call negate-same-colored-pairs to build a list of clauses where x and y are not the same color
;;;   Return that list
;;;
(defun generate-edge-clauses (e k)
  (negate-same-colored-pairs e 1 k)) ; starts at color 1 but increments recursively through all colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; negate-same-colored-pairs ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inputs:
;;;   1. edge = (x,y) pair of node indices
;;;   2. color = color index
;;;   3. k = max color index
;;;
;;; Outputs: a list of clauses where x and y are not the same color
;;;   
;;; Behavior:
;;;   Check if we have run out of colors
;;;   Let x = the first node in the edge
;;;   Let y = the second node in the edge
;;;   Let xIndex = ~x_variable_index (~variable_index when x gets the current color)
;;;   Let yIndex = ~y_variable_index (~variable_index when y gets the same color)
;;;   Let negatedPair = (~x_variable_index | ~y_variable_index) for when x and y are the same color
;;;   Recurse on the rest of the colors 
;;;   Append all negated pairs
;;;   Return that list
;;;
(defun negate-same-colored-pairs (edge color k)
  (cond
    ((> color k) NIL) ; out of colors
    (T 
      (let* ((x (first edge)) ; first node
              (y (second edge)) ; second node
              (xIndex (- (node2var x color k))) ; ~first_variable_index
              (yIndex (- (node2var y color k))) ; ~second_variable_index
              (negatedPair (list xIndex yIndex))) ; (~first_variable_index | ~second_variable_index)
        (append (list negatedPair) (negate-same-colored-pairs edge (+ 1 color) k)))))) ; negated pairs between node x and node y for all colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun