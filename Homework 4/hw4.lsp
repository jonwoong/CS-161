; JONATHAN WOONG
; 804205763
; CS 161 - FALL 17
; HW 4

(defun reload ()
	(load "hw4.lsp")
	(load "parse_cnf.lsp"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SAT ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	Input: 
;;;		1. n = number of variables in CNF
;;; 	2. delta = CNF equation
;;;
;;; Output: the list solution to the CNF with variable assignments
;;;
;;; Behavior:
;;;		Get a list of all variable names
;;; 	Generate binary combination (0=false,1=true) for all possible combinations of variables
;;; 	Test each binary combination on each clause, return the combination that is true for all clauses
;;; 	
(defun sat? (n delta)
	(findSolution n delta))

;;;;;;;;;; getLength ;;;;;;;;;;
;;;
;;; Input: l = improperlist
;;;
;;; Output: length of l
;;;
(defun getLength (l)
	(cond
		((null l) 0)
		((atom l) 1)
		((listp l) (+ 1 (getLength (cdr l))))
		(T 0)))


;;;;;;;;;; zeroFill ;;;;;;;;;;
;;;
;;; Input: n = length of list to generate
;;;
;;; Output: a list (0 0 0 0 ... 0) of length n to be used as the inital assignment to variables
;;;
(defun zeroFill (n)
	(cond
		((> n 0) (append (list 0) (zeroFill (- n 1))))
		(T NIL)))

;;;;;;;;;; isAllOnes ;;;;;;;;;;
;;;
;;; Input: l = a list of 0's and 1's
;;;
;;; Output: T if the list contains all 1's, NIL otherwise
;;;
(defun isAllOnes (l)
	(cond
		((null l) T)
		((atom l)
			(if (= l 1)
				T
				NIL))
		((listp l)
			(and (isAllOnes (car l)) (isAllOnes (cdr l))))
		(T NIL)))

;;;;;;;;;; increment ;;;;;;;;;;
;;;
;;; Input: l = a list of 0's and 1's
;;;
;;; Output: a list that increments l by 1 (as if its a binary number)
;;;
(defun increment (l)
	(cond
		((isAllOnes l) NIL)
		((null l) NIL)
		((atom l)
			(if (= l 0)
				1
				0))
		((listp l)
			(cond
				((= 0 (car l))
					(append (list 1) (cdr l)))
				((= 1 (car l))
					(append (list 0) (increment (cdr l))))
				(T NIL)))
		(T NIL)))

;;;;;;;;;; addHead ;;;;;;;;;;
;;;
;;; Input:
;;; 	1. value = 0 or 1
;;; 	2. l = list to add head to
;;; 
;;; Output: a list where value is added as the head of each element of l
;;;
(defun addHead (value l)
	(cond
		((null l) (cons value l))
		((listp l) (cons (cons value (car l)) (addHead value (cdr l))))
		(T NIL)))

;;;;;;;;;; generateCombinationsUnfiltered ;;;;;;;;;;
;;; 
;;; Input: n = length of each combination
;;;
;;; Output: a list of all binary combinations of length n to be used as variable assignemnts
;;;
(defun generateCombinationsUnfiltered (n)
	(cond
		((> n 0) (append (addHead 0 (generateCombinationsUnfiltered (- n 1))) (addHead 1 (generateCombinationsUnfiltered (- n 1)))))
		(T NIL)))


;;;;;;;;;; filterUnwanteds ;;;;;;;;;;
;;;
;;; Input: the output from gerneateCombinationsUnfiltered
;;;
;;; Output: the list of all binary combinations of length n (with errors taken out)
;;;
(defun filterUnwanteds (l n)
	(cond
		((null l) NIL)
		((atom l)
			(if (= (length l) n)
				l
				NIL))
		((listp l)
			(if (= (getLength (car l)) n)
				(append (list (car l)) (filterUnwanteds (cdr l) n))
				(filterUnwanteds (cdr l) n)))
		(T NIL)))

;;;;;;;;;; convertImproperToList
;;;
;;; Input: l = an improper list
;;;
;;; Output: a list that derives from an improperlist
;;;
(defun convertImproperToList (l)
	(cond
		((null l) NIL)
		((atom l) (list l))
		((listp l)
			(append (convertImproperToList (car l)) (convertImproperToList (cdr l))))
		(T NIL)))

;;;;;;;;;; generateCombinationsFiltered ;;;;;;;;;;
;;;
;;; Input: the result from generateCombinationsUnfiltered
;;;
;;; Output: the list of all binary combinations of length n (with errors taken out)
;;;
(defun generateCombinationsFiltered (l)
	(cond
		((null l) NIL)
		((listp l)
			(append (list (convertImproperToList (car l))) (generateCombinationsFiltered (cdr l))))
		(T NIL)))

;;;;;;;;;; isUnitClause ;;;;;;;;;; 
;;;
;;; Input: anything
;;;
;;; Output: T if the input is a variable clause, NIL otherwise
;;;
(defun isUnitClause (variable)
	(cond
		((null variable) NIL)
		((atom variable) T)
		((listp variable)
			(if (= 1 (length variable))
				T
				NIL))
		(T NIL)))

;;;;;;;;;; isNegative ;;;;;;;;;; 
;;;
;;; Input: anything
;;;
;;; Output: T if the input is negative, NIL otherwise
;;;
(defun isNegative (variable)
	(cond
		((null variable) NIL)
		((atom variable)
			(if (< variable 0)
				T
				NIL))
		((listp variable)
			(if (< (car variable) 0)
				T
				NIL))
		(T NIL)))

;;;;;;;;;; negate ;;;;;;;;;; 
;;;
;;; Input: a number or unit clause
;;;
;;; Ouptput: the input negated
;;;
(defun negate (variable)
	(cond
		((atom variable)
			(if (> variable 0)
				(- 0 variable)
				(- 0 variable)))
		((listp variable)
			(if (> (car variable) 0)
				(list (- 0 (car variable)))
				(list (- 0 (car variable)))))
		(T NIL)))

;;;;;;;;;; getNecessaryAssignment ;;;;;;;;;; 
;;;
;;; Input: a variable name
;;;
;;; Output: the necessary assignment for that variable to be evaluated as true
;;;
(defun getNecessaryAssignment (variable)
	(if (< variable 0)
		(- 0 variable)
		variable))

;;;;;;;;;; getTruthValue ;;;;;;;;;; 
;;;
;;; Input:
;;;		1. variable = a variable name
;;; 	2. assignment = 0=false, 1=true
;;;
;;; Output: the evaluated T or NIL value after assigning that variable
;;;
(defun getTruthValue (variable assignment)
	(cond
		((= assignment 0)
			(if (isNegative variable)
				T
				NIL))
		((= assignment 1)
			(if (isNegative variable)
				NIL
				T))
		(T NIL)))

;;;;;;;;;; testClauseAssignment ;;;;;;;;;; 
;;;
;;; Input:
;;; 	1. clause = a clause containing 1 or more variables
;;; 	2. assignment = a list of 0's and 1's to assign to each variable
;;;
;;; Output: T if assigning variables evaluates the clause to T, NIL otherwise
;;;
(defun testClauseAssignment (clause assignment)
	(let* ((clauseLength (length clause)))
		(cond
			((null clause) NIL)
			((isUnitClause clause)
				(cond
					((null assignment) NIL)
					((atom assignment) (getTruthValue clause assignment))
					((listp assignment) NIL)
					(T NIL)))
			((listp clause)
				(cond
					((null assignment) NIL)
					((atom assignment) NIL)
					((listp assignment)
						(let* ((truthResult 
							(or (getTruthValue (car clause) (car assignment)) 
							(testClauseAssignment (cdr clause) (cdr assignment)))))
						(if (null truthResult)
							NIL
							T)))
					(T NIL)))
			(T NIL))))

;;;;;;;;;; convertBinaryToBool ;;;;;;;;;; 
;;;
;;; Input: num = a single number 0 or 1, or a list of 0's and 1's
;;;
;;; Output: a list containing T and NILs depending on 0's and 1's
;;;
(defun convertBinaryToBool (num)
	(cond
		((atom num)
			(if (= num 0)
				NIL
				T))
		((listp num)
			(if (= (car num) 0)
				NIL
				T))
		(T NIL)))

;;;;;;;;;; absolute ;;;;;;;;;; 
;;;
;;; Input: num = a single number or list of numbers
;;;
;;; Output: a list of absolute values of nums
;;;
(defun absolute(num)
	(cond
		((null num) NIL)
		((atom num)
			(if (> num 0)
				num
				(- 0 num)))
		((listp num)
			(append (list (absolute (car num))) (absolute (cdr num))))
		(T NIL)))

;;;;;;;;;; listContainsNum ;;;;;;;;;; 
;;;
;;; Input:
;;; 	1. l = a list
;;; 	2. num = a number
;;;
;;; Output: T if l contains num, NIL otherwise
;;;
(defun listContainsNum (l num)
	(cond
		((null l) NIL)
		((atom l)
			(cond
				((null num) NIL)
				((atom num)
					(if (= l num)
						T
						NIL))
				((listp num)
					(if (= l (car num))
						T
						NIL))
				(T NIL)))
		((listp l)
			(cond
				((null num) NIL)
				((atom num)
					(or (listContainsNum (car l) num)
						(listContainsNum (cdr l) num)))
				((listp num)
					(or (listContainsNum (car l) (car num))
						(listContainsNum (cdr l) (car num))))
				(T NIL)))
		(T NIL)))

;;;;;;;;;; flattenList ;;;;;;;;;; 
;;; 
;;; Input: l = a list of lists
;;;
;;; Output: a single list containing all elements in l
;;;
(defun flattenList (l)
	(cond
		((null l) NIL)
		((atom l) (list l))
		((listp l)
			(append (car l) (flattenList (cdr l))))
		(T NIL)))

;;;;;;;;;; collectUniqueValues ;;;;;;;;;; 
;;;
;;; Input: l = a list
;;;
;;; Output: a list containing all unique values in l
;;;
(defun collectUniqueValues (l)
	(cond
		((null l) NIL)
		((atom l) (list l))
		((listp l)
			(if (listContainsNum (cdr l) (car l))
				(collectUniqueValues (cdr l))
				(append (list (car l)) (collectUniqueValues (cdr l)))))
		(T NIL)))

;;;;;;;;;; collectVariableList ;;;;;;;;;; 
;;;
;;; Input: l = a sentence
;;;
;;; Output: a list containing all variables in l
;;;
(defun collectVariableList (l)
	(cond
		((null l) NIL)
		((atom l) (list l))
		((listp l)
			(collectUniqueValues (absolute (flattenList l))))
		(T NIL)))

;;;;;;;;;; assignVariables ;;;;;;;;;; 
;;;
;;; Input: 
;;; 	1. variables = variable names
;;; 	2. assignment = a list of 0's and 1's to assign to each variable
;;;
;;; Output: a list containing (VARIABLE NAME T) or (VARIABLENAME NIL)
;;;
(defun assignVariables (variables assignment)
	(cond
		((null variables) NIL)
		((atom variables)
			(cond 
				((null assignment) NIL)
				((atom assignment) (list variables (convertBinaryToBool assignment)))
				(T NIL)))
		((listp variables) 
			(cond 
				((null assignment) NIL)
				((atom assignment) NIL)
				(T (append (list (assignVariables (car variables) (car assignment)))
					(assignVariables (cdr variables) (cdr assignment))))))
		(T NIL)))

; tests if a pair is (VARIABLENAME T) or (VARIABLENAME NIL)
(defun isAssignmentPair (pair)
	(cond
		((null pair) NIL)
		((atom pair) NIL)
		((listp pair)
			(if (and 
					(= 2 (length pair)) 
					(numberp (car pair)))
				T
				NIL))
		(T NIL)))

; maps all (VARIABLE T) and (VARIABLE NIL) to clause elements
(defun mapVariableAssignmentToClause (clause variableAssignment)
	(cond
		((null clause) NIL)
		((atom clause)
			(cond
				((null variableAssignment) NIL)
				((isAssignmentPair variableAssignment)
					(cond
						((= clause (first variableAssignment)) (second variableAssignment))
						((= clause (negate (first variableAssignment))) (not (second variableAssignment)))
						(T NIL)))
				((listp variableAssignment)
					(or (mapVariableAssignmentToClause clause (car variableAssignment))
					(mapVariableAssignmentToClause clause (cdr variableAssignment))))
				(T NIL)))
		((listp clause)
			(append (list (mapVariableAssignmentToClause (car clause) variableAssignment))
				(mapVariableAssignmentToClause (cdr clause) variableAssignment)))
		(T NIL)))

; evaluates all (VARIABLE T) and (VARIABLE NIL) assignments
(defun evaluateBools (l)
	(cond
		((null l) NIL)
		((atom l) l)
		((listp l) (or (car l) (evaluateBools (cdr l))))
		(T NIL)))

; converts (VARIABLE T) and (VARIABLE NIL) to just (VARIABLE)
(defun convertAssignmentToAnswer (variableAssignment)
	(cond
		((null variableAssignment) NIL)
		((isAssignmentPair variableAssignment)
			(let* ((variableName (first variableAssignment))
				(boolValue (second variableAssignment)))
			(if (null boolValue)
				(list (negate variableName))
				(list variableName))))
		((listp variableAssignment)
			(append (convertAssignmentToAnswer (car variableAssignment))
				(convertAssignmentToAnswer (cdr variableAssignment))))
		(T NIL)))

; assign values to variables in clause and test whether clause evaluates to true
(defun findSolutionsForClause (clause binaryAssignment variables)
	(cond
		((null clause) NIL)
		((listp clause)
				(cond
					((null possibleVariableAssignment) NIL)
					((listp possibleVariableAssignment)
						(let* ((assignment (assignVariables variables binaryAssignment))
							(truthResult (evaluateBools (mapVariableAssignmentToClause clause assignment))))
						(if (not (null truthResult))
							(append (list assignment) (findSolutionsForClause clause (increment binaryAssignment)))
							(findSolutionsForClause clause (increment binaryAssignment)))))
					(T NIL)))
		(T NIL)))

; finds first possible assignment to clause that evaluates to true
(defun findFirstClauseSolution (clause binaryAssignment variables)
	(cond
		((null clause) NIL)
		((listp clause)
				(cond
					((null binaryAssignment) NIL)
					((listp binaryAssignment)
						(let* ((assignment (assignVariables variables binaryAssignment))
							(truthResult (evaluateBools (mapVariableAssignmentToClause clause assignment))))
						(if (not (null truthResult))
							binaryAssignment
							(findFirstClauseSolution clause (increment binaryAssignment) variables))))
					(T NIL)))
		(T NIL)))

; finds next possible clause assignemnt that evaluates to true
(defun findNextClauseSolution (clause previousBinarySolution variables)
	(cond
		((null clause) NIL)
		((listp clause)
			(let* ((n (length variables)))
				(cond
					((null previousBinarySolution) (findFirstClauseSolution clause (zerofill n) variables))
					((listp previousBinarySolution) 
						(findFirstClauseSolution clause (increment previousBinarySolution) variables))
					(T NIL))))
		(T NIL)))

; tests a variable assignment on all clauses in delta
(defun testSolutionOnAllClauses (delta solution variables)
	(cond
		((null delta) NIL)
		((listp delta)
			(cond
				((null (cdr delta)) (evaluateBools (mapVariableAssignmentToClause (car delta) (assignVariables variables solution))))
				((not (null (cdr delta)))
					(let* ((truthResult (evaluateBools (mapVariableAssignmentToClause (car delta) (assignVariables variables solution)))))
						(if (not (null truthResult)) 
							(and truthResult (testSolutionOnAllClauses (cdr delta) solution variables))
							NIL)))
				(T NIL)))
		(T NIL)))

; solves all clauses in delta, returns multiple solutions
(defun solveDelta (delta possibleSolution variables)
	(cond
		((null delta) possibleVariableAssignment)
		((listp delta)
			(let* ((n (length variables))
				(clause (car delta)))
				(cond
					((null possibleSolution) NIL)
					((listp possibleSolution)
						(if (not (null (testSolutionOnAllClauses delta possibleSolution variables)))
							(assignVariables variables possibleSolution)
							(solveDelta delta (findNextClauseSolution clause possibleSolution variables) variables)))
					(T NIL))))
		(T NIL)))

; get 1 solution from solvedelta
(defun findSolution (n delta)
	(let* ((variableList (collectVariableList delta))
		(possibleSolution (findFirstClauseSolution (car delta) (zerofill n) variableList))
		(solution (solveDelta delta possibleSolution variableList)))
	(cond
		((null solution) NIL)
		((listp solution) (convertAssignmentToAnswer solution))
		(T NIL))))
