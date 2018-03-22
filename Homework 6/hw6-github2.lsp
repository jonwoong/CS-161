;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; given node index(n), color index(c), and max color index(k)
; return index of propositional variable that represents the
; constraint: "node n receives color c"
;	return --> (n-1) * k + c
(defun node2var (n c k)
	(+ 
		c 
		(* 
			k 
			(- n 1)
		)
	)
)

; given n, c, k
; return a CLAUSE that represents: "node n must be colored w/ at
;	LEAST one color whose index comes from {c, c+1, ... k}
(defun at-least-one-color (n c k)
	(cond
		((< k 1) NIL)	; max # colors shouldn't be less than 1
		((< k c) NIL)	; color index shouldn't be < max
		((= k c) (list (node2var n k k))) ; return last var
		(t		; append curr var / recurse on rest
			(append
				(list (node2var n c k))
				(at-least-one-color n (+ 1 c) k)
			)
		)
	)
)

; given n, c, k
; return a LIST OF CLAUSES that represent: "node n must be colored w/
;	at MOST one color whose index comes from set {c, c+1, ... k}
(defun at-most-one-color (n c k)
	(cond	; only a single literal
		(	; return literal and its negative
			(= c k)	; ^ this is a valid clause
			(list
				(list 
					(* n k)
					(- (* n k))
				)
			)
		)
		(t	; otherwise must recurse to get the clause	
			(at-most-one-helper (at-least-one-color n c k))
		)
	)
)

; helper function -- return length of array
(defun arrlen (l)
	(cond
		((NULL l) 0)
		(t (+ 1 (arrlen (cdr l))))
	)
)

; helper function to return all permutations of x with negation of
;	all elements in l
(defun permutations (x l)
	(cond
		((NULL l) NIL) ; empty list
		(t
			(append
				(list (list x (- (car l)))) ; list of pairs
				(permutations x (cdr l)) ;of negated literal in l w/
							; x
			)
		)
	)
)

; helper function that appends permutations together
(defun at-most-one-helper (l)
	(cond
		((< (arrlen l) 2) NIL)	; base case -- single element
		(t
			(append	; append permutations of -(first) w/ rest
				(permutations (- (car l)) (cdr l))
				(at-most-one-helper (cdr l)); to rest of permutations
			)
		)
	)
)

; given n, k
; return LIST OF CLAUSES that constrain n to be colored w/ exactly
;	one color in set {1, ... k}
; -- utilize the two other functions I wrote...
(defun generate-node-clauses (n k) ;append at least 1 and at most 1
	(append
		(list (at-least-one-color n 1 k))
		(at-most-one-color n 1 k)
	)
)

; Given edge (x y) and a max color index (k), return list of clauses
;	that prohibit x and y from having same color.
(defun generate-edge-clauses (e k)
	(edge-clauses-helper e 1 k);start w/ c = 1
)

(defun edge-clauses-helper (e c k); for c < k
	(cond
		((> c k) NIL);exceeded colormax
		(t
			(append
				(list ;get clause where colors occurs for both
					(list 
						(- (node2var (car e) c k))
						(- (node2var (second e) c k))
					)
				)
				(edge-clauses-helper e (+ 1 c) k);recurse on rest
			)
		)
	)	
)

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