; JONATHAN WOONG
; 804205763
; CS 161 - FALL 17
;
; CS161 Hw3: Sokoban
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; UTILITY FUNCTIONS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload()
  (load "hw3.lsp")
  )

(defun load-a-star()
  (load "a-star.lsp"))

(defun reload-all()
  (reload)
  (load-a-star)
  )

(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SOKOBAN CODE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;;;

(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HELPFER FUNCTIONS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isBlank (v)
  (= v blank))

(defun isWall (v)
  (= v wall))

(defun isBox (v)
  (= v box))

(defun isKeeper (v)
  (= v keeper))

(defun isStar (v)
  (= v star))

(defun isBoxStar (v)
  (= v boxstar))

(defun isKeeperStar (v)
  (= v keeperstar))

(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))))))

(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1)))))))

(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; GOAL-TEST ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input: s = the grid representing objects in the game
;;;
;;; Output: T if no boxes exist in the grid, NIL otherwise
;;; 
;;; Behavior: Recurse through a state and check whether the head (rows) contain boxes
;;;
(defun goal-test (s)
  (cond
  	((null s) NIL) ; exit case
  	((listp s) 
  		(and
  			(noBoxInRow (car s))
  			(noBoxInRow (cdr s))))
  	(T NIL)));end defun

;;;;;;;;;; noBoxInRow ;;;;;;;;;;
;;;
;;; Input: row = a list element of the state
;;;
;;; Output: T if no boxes exist in the row, NIL otherwise
;;;
;;; Behavior: Iterates through a row (list) and checks whether the row contains a box
;;;
(defun noBoxInRow (row)
	(cond
		((null row) T) ; exit case
		((atom row) 
			(if (isBox row) ; singleton case
				NIL 
				T))
		((listp row) ; list case
			(and (noBoxInRow (car row))
				(noBoxInRow (cdr row))))
		(T NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; NEXT-STATES ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input: s = the grid representing objects in the game
;;;
;;; Ouptut: result = a list of possible transition states
;;;
;;; Behavior: Followed the suggestion pretty much exactly,
;;; 	Stores 4 possible states in result with NILs removed by cleanUpList
;;;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 (result (list (try-move 1 pos s) (try-move 2 pos s) (try-move 3 pos s) (try-move 4 pos s))))
    (cleanUpList result);end
    );end let
  );

;;;;;;;;;; getNumberOfRows ;;;;;;;;;;
;;;
;;; Input: state = the grid representing objects in the game
;;;
;;; Output: the number of rows in a state
;;;
;;; Behavior: Counts the number of rows in a state by incrementing 1 
;;; 	for each head (row) visited
;;;
(defun getNumberOfRows (state)
	(cond
		((null state) 0) ; exit case
		((listp state) (+ 1 (getNumberOfRows (cdr state)))))) ; increment +1 per head (row)

;;;;;;;;;; getNthFromRow ;;;;;;;;;;
;;;
;;; Input: 
;;;		1. n = the index of the item to return
;;; 	2. row = the list to search on
;;; 
;;; Output: the nth atom of row
;;;  
;;; Behavior: Returns the nth element from a list (by index)
;;;
(defun getNthFromRow (n row)
	(cond
		((> n 0) (getNthFromRow (- n 1) (cdr row))) ; iterate through the list, decrementing n by 1 for each element visited
		(T (car row)))) ; return the head when n=0

;;;;;;;;;; getNthRowFromState ;;;;;;;;;;
;;;
;;; Input: 
;;;		1. n = the index of the row to return
;;;		2. state = the grid representing objects in the game
;;;
;;; Output: the nth row (list) from state 
;;;
;;; Behavior: Returns the nth row from a state (by index)
;;;
(defun getNthRowFromState (n state)
	(cond
		((> n 0) (getNthRowFromState (- n 1) (cdr state))) ; iterate through rows, decrementing n by 1 for each row visited
		(T (car state)))) ; return the head when n=0

;;;;;;;;;; getObjectAtPos ;;;;;;;;;;
;;;
;;; Input:
;;;
;;;
;;; Output:
;;;
;;; Behavior:
;;;
(defun getObjectAtPos (pos state)
	(let* ((x (car pos))
	 	(y (cadr pos)))
		(getNthFromRow x (getNthRowFromState y state))))

;;;;;;;;;; setNthInRow ;;;;;;;;;;
;;;
;;; Input:
;;; 	1. n = the index of the list value to set
;;; 	2. row = the row to change
;;; 	3. value = the value to set the nth element of row to
;;;
;;; Output: the modified row
;;;
;;; Behavior: Sets the nth element in a row to a value (by index)
;;;
(defun setNthInRow (n row value)
	(cond 
		((null row) NIL) ; exit case
		((> n 0) (cons (car row) (setNthInRow (- n 1) (cdr row) value))) ; iterate through a row while retaining the head, decrement n by 1 for each element visited
		(T (cons value (cdr row))))) ; return the row with value changed

;;;;;;;;;; setNthRowInState ;;;;;;;;;;
;;;
;;; Input:
;;; 	1. n = the index of the row to modify
;;; 	2. state = the grid representing objects in the game
;;; 	3. value = the value that the nth row will be overwritten to
;;;
;;; Output: state with updated nth row
;;;
;;; Behavior: Sets the nth row in a state to a row value (by index)
;;;
(defun setNthRowInState (n state value)
	(cond
		((> n 0) (cons (car state) (setNthRowInState (- n 1) (cdr state) value))) ; iterate through rows while retaining the head, decrement n by 1 for eah row visited
		(T (cons value (cdr state))))) ; return the state with row changed

;;;;;;;;;; setPosToN ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. pos = (c, r) position of keeper
;;; 	2. n = the value to change (c, r) to
;;; 	3. state = the grid representing objects in the game
;;;
;;; Output: the updated state
;;;
;;; Behavior: Sets specific square (c,r) in a state to a value n
;;; 	Relies heavily on the functions above to get/set specific squares in the state
;;;
(defun setPosToN (pos n state)
	(let* ((x (car pos))
		(y (cadr pos)))
	(setNthRowInState y state (setNthInRow x (getNthRowFromState y state) n)))) ; simple use of above helper functions to choose row and element

;;;;;;;;;; withinBounds ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. pos = (c, r) position of keeper
;;; 	1. state = the grid representing objects in the game
;;;
;;; Output: T if a position is valid, NIL otherwise
;;;
;;; Behavior: Checks whether (c,r) coordinate is within bounds of state
;;;
(defun withinBounds (pos state)
	(let* ((x (car pos))
	 	(y (cadr pos))
	 	(rowLength (length (car state)))
	 	(numberOfRows (getNumberOfRows state)))
	(cond
		((null x) NIL) ; invalid x
		((null y) NIL) ; invalid y
		((and (< x rowLength) (>= x 0)) T)
		((and (< y numberOfRows) (>= y 0)) T)
		(T NIL))))

;;;;;;;;;; directionValid ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. direction = (1=NORTH, 2=WEST, 3=EAST, 4=SOUTH)
;;; 	2. pos = (c, r) position of keeper
;;; 	3. state = the grid representing objects in the game
;;;
;;; Output: T if a move in that direction is valid, NIL otherwise
;;;
;;; Behavior: Checks whether (c,r) coordinate is within bounds of state
;;;
(defun directionValid (direction pos state)
	(let* ((x (car pos))
	 	(y (cadr pos))
	 	(rowLength (length (car state)))
	 	(numberOfRows (getNumberOfRows state)))
	(cond
		((null x) NIL) ; invalid x
		((null y) NIL) ; invalid y
		((= direction 1) (> y 0)) ; y lower bound
		((= direction 2) (> x 0)) ; x lower bound
		((= direction 3) (< x rowLength)) ; x upper bound
		((= direction 4) (< y numberOfRows)) ; y uppder bound
		(T NIL))))

;;;;;;;;;; getNeighbor ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. direction = (1=NORTH, 2=WEST, 3=EAST, 4=SOUTH)
;;; 	2. pos = (c, r) position of keeper
;;; 	3. state = the grid representing objects in the game
;;;
;;; Output: the value at (c, r) of the neighbor of keeper chosen by setting direction
;;;
;;; Behavior:
;;; 	Returns the value of the square that is the neighbor of (c,r) 
;;;
(defun getNeighbor (direction pos state)
	(cond ((directionValid direction pos state) ; bounds checking
		(let* ((x (car pos))
		 	(y (cadr pos)))
		(cond
			((= direction 1) (getNthFromRow x (getNthRowFromState (- y 1) state))) ; return 1 square north
			((= direction 2) (getNthFromRow (- x 1) (getNthRowFromState y state))) ; return 1 square west
			((= direction 3) (getNthFromRow (+ x 1) (getNthRowFromState y state))) ; return 1 square east
			((= direction 4) (getNthFromRow x (getNthRowFromState (+ y 1) state))) ; return 1 square south
			(T NIL))))))

;;;;;;;;;; getNeighborPos ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. direction = (1=NORTH, 2=WEST, 3=EAST, 4=SOUTH)
;;; 	2. pos = (c, r) position of keeper
;;; 	3. state = the grid representing objects in the game
;;;
;;; Output: the (c, r) coordinate of the keeper's neighbor chosen by setting direction
;;;
;;; Behavior: Returns the coordinates of the neighbor of (c,r)
;;;
(defun getNeighborPos (direction pos state)
	(cond ((directionValid direction pos state) ; bounds checking
		(let* ((x (car pos))
		 	(y (cadr pos)))
		(cond
			((= direction 1) (list x (- y 1))) ; north = (x, y-1)
			((= direction 2) (list (- x 1) y)) ; west = (x-1, y)
			((= direction 3) (list (+ x 1) y)) ; east = (x+1, y)
			((= direction 4) (list x (+ y 1))) ; south = (x, y+1)
			(T NIL))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TRY-MOVE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input:  
;;; 	1. direction = (1=NORTH, 2=WEST, 3=EAST, 4=SOUTH)
;;; 	2. pos = (c, r) position of keeper
;;; 	3. state = the grid representing objects in the game
;;;
;;; Output: an updated state if the move is valid, NIL otherwise
;;;
;;;	Behavior:
;;; 	Get the (c,r) of the target square that keeper wants to move into
;;;		If the target square is not null, 
;;;			Get the value of the square that the keeper is on (keeper or keeperstar)
;;;			Get the value and (c,r) of the target square's neighbor
;;;			Update squares based on what the current keepersquare, targetsquare, and neighborsquares are
;;; 
(defun try-move (direction pos state)
	(let* ((target (getNeighbor direction pos state)) ; target = square to move to
			(targetPos (getNeighborPos direction pos state)))
		(cond
			((or (null target) (not (withinBounds targetPos state)) (not (withinBounds targetPos state))) NIL) ; null target or out of bounds
			(T (let* ((keeperSquare (getObjectAtPos pos state)) ; keeperSquare = square that keeper is on
					(neighborOfTarget (getNeighbor direction targetPos state)) ; neighborOfTarget = 1 square next to target in same direction as keeper movement
					(neighborOfTargetPos (getNeighborPos direction targetPos state))) ; neighborOfTargetPos = (c, r) of target
				(cond 
					((isKeeper keeperSquare) ; keeper on blank
							(cond
								((null neighborOfTarget) ; null target's neighbor
									(cond
										((isStar target) (setPosToN (getNeighborPos direction pos state) keeperstar (setPosToN pos blank state))) ; move onto star
										((isBlank target) (setPosToN (getNeighborPos direction pos state) keeper (setPosToN pos blank state))) ; move into space
										(T NIL))) ; end isKeeper))
								((isBoxStar target) ; target = boxstar
									(cond
										((isBlank neighborOfTarget) (setPosToN neighborOfTargetPos box (setPosToN targetPos keeperstar (setPosToN pos blank state))))
										((isStar neighborOfTarget) (setPosToN neighborOfTargetPos boxstar (setPosToN targetPos keeperstar (setPosToN pos blank state))))
										(T NIL)))
								((isBox target) ; target = box
									(cond
										((isBlank neighborOfTarget) (setPosToN neighborOfTargetPos box (setPosToN targetPos keeper (setPosToN pos blank state)))) ; push the box
										((isStar neighborOfTarget) (setPosToN neighborOfTargetPos boxstar (setPosToN targetPos keeper (setPosToN pos blank state)))) ; push box onto star
										(T NIL))) ; etc
								((isStar target) (setPosToN (getNeighborPos direction pos state) keeperstar (setPosToN pos blank state))) ; move onto star
								((isBlank target) (setPosToN (getNeighborPos direction pos state) keeper (setPosToN pos blank state))) ; move into space
								(T NIL))) ; end isKeeper
					((isKeeperStar keeperSquare) ; keeper on star
							(cond
								((null neighborOfTarget) ; null target's neighbor
									(cond
										((isStar target) (setPosToN (getNeighborPos direction pos state) keeperstar (setPosToN pos star state))) ; move onto star
										((isBlank target) (setPosToN (getNeighborPos direction pos state) keeper (setPosToN pos star state))) ; move into space
										(T NIL))) ; end isKeeper)))
								((isBoxStar target) ; target = boxstar
									(cond
										((isBlank neighborOfTarget) (setPosToN neighborOfTargetPos box (setPosToN targetPos keeperstar (setPosToN pos star state))))
										((isStar neighborOfTarget) (setPosToN neighborOfTargetPos boxstar (setPosToN targetPos keeperstar (setPosToN pos star state))))
										(T NIL)))
								((isBox target) ; target = box
									(cond
										((isBlank neighborOfTarget) (setPosToN neighborOfTargetPos box (setPosToN targetPos keeper (setPosToN pos star state)))) ; push the box
										((isStar neighborOfTarget) (setPosToN neighborOfTargetPos boxstar (setPosToN targetPos keeper (setPosToN pos star state)))) ; push box onto star
										(T NIL))) ; etc
								((isStar target) (setPosToN (getNeighborPos direction pos state) keeperstar (setPosToN pos star state))) ; move onto star
								((isBlank target) (setPosToN (getNeighborPos direction pos state) keeper (setPosToN pos star state))) ; move into space
							(T NIL))) ; end isKeeper
					(T NIL)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; H0 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input: s = the grid representing objects in the game
;;;
;;; Output: 0
;;;
;;; Behavior: Just returns 0
;;;
(defun h0 (s)
  0)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; H0 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Admissible? 
;;; 	Yes, this is admissible, because 
;;; 
;;; Input: s = the grid representing objects in the gameJust returns 0
;;;
;;; Output: the number of boxes not yet at a star
;;;
;;; Behavior: Counts the number of boxes using labelBoxes()
;;;
(defun h1 (s)
  (count 1 (labelBoxes s))) 

;;;;;;;;;; labelBoxes ;;;;;;;;;;
;;;
;;; Input: s = the grid representing objects in the gameJust returns 0
;;;
;;; Output: a list of 0's and 1's marking the number of boxes
;;;
;;; Behavior: Iterates through every square of the state, building a list of 1's (for boxes) and 0's (for no boxes)
;;; 
(defun labelBoxes (s)
	(cond
		((null s) (list 0)) ; exit case
		((atom s) ; singleton case
			(if (isBox s) 
				(list 1) ; 1 = box
				(list 0))) ; 0 = no box
		((listp s) 
			(append (labelBoxes (car s)) ; iterate through head
				(labelBoxes (cdr s)))) ; iterate through tail
		(T (list 0)))) ; exit case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; H804205763 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input: s = the grid representing objects in the gameJust returns 0
;;;
;;; Output: a number representing the distances between boxes and stars
;;;
;;; Behavior:
;;; 	Calls distanceHeuristic(), which calculates the distance between all boxes and stars in a state
;;; 	DISTANCE = || (|box_col, box_row|) - (|star_col, star_row|) ||
;;;
(defun h804205763 (s) 
	(distanceHeuristic s))

;;;;;;;;;; getObjectColumns ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. r = row of state
;;; 	2. col = column of state
;;; 	3. type = (2 = BOX, 4 = STAR)
;;; 
;;; Output: a list of atoms indicating the columns where boxes were found in a given row
;;;
;;; Behavior: Iterate through all elements of the state and build list containing col values of boxes/stars
;;; 
;(defun getObjectColumns (r col type)
;  (cond 
;  	((null r) nil) ; exit case
;  	((atom r)
;  		(if (= type r)
;  			(list col)
;  			NIL))  
;  	((listp r) (append (getObjectColumns (car r) col type) (getObjectColumns (cdr r) (+ col 1) type))) ; recursively construct list
;  	(T NIL))) ; if this column doesn't contain either, go to next column

(defun getObjectColumns (r col type)
  (cond 
  	((null r) nil) ; exit case
  	(T (if (= type (car r))
  		(append (list col) (getObjectColumns (cdr r) (+ col 1) type))
  		(getObjectColumns (cdr r) (+ col 1) type)))))

;;;;;;;;;; getObjectsInRow ;;;;;;;;;;
;;;
;;; Input: 
;;;		1. objectColumns = a list of lists that contain column coordinates of boxes/squares in a state
;;;		2. row = a row from a state
;;;
;;; Output: The values of that squares with coordinate (objectColumn[i],row)
;;;		
;;; Behavior: Given a row, recursively builds a list of the objects in that row with column coordinates in objectColumns list
;;;
(defun getObjectsInRow (objectColumns row)
	(cond 
		((null objectColumns) NIL) ; exit case
		(T (let* ((columnToGet (car objectColumns)) ; columnToGet = index of a column that contains a box/star
				(object (getNthFromRow columnToGet row))) ; object = the actual value at the square we look at
			(cond
				((null objectColumns) NIL) ; exit case
				((atom objectColumns) (list object)) ; singleton case
				((listp objectColumns) (append (list object) (getObjectsInRow (cdr objectColumns) row))) ; build a list of the objects at column coordinate objectColumns
				(T NIL)))))) ; exit case

;;;;;;;;;;; checkObjects ;;;;;;;;;;
;;;
;;; Input:
;;; 	1. objects = the list built using getObjectsInRow() above
;;; 	2. type = (2 = BOX, 4 = STAR)
;;; 	3. objectColumns = a list of lists that contain column coordinates of boxes/squares in a state
;;; 	4. rowNum =  index of the row an object was found in
;;; 	
;;; Output: a list of (objectColumns, rowNum) for the objects we check that are boxes/stars
;;;
;;; Behavior: Iterates through objects in the objects list, checks their type, and builds a list containing (c, r) if they are boxes/stars
;;;
(defun checkObjects (objects type objectColumns rowNum)
	(cond 
		((null objectColumns) NIL) ; exit case
		((atom objectColumns) ; singleton case
			(cond
				((and (= type 2) (isBox objects)) (list objectColumns rowNum)) ; (c, r) of a box
				((and (= type 4) (isStar objects)) (list objectColumns rowNum)) ; (c, r) of a star
				(T NIL))) ; exit case
		((listp objectColumns) ; list case
			(append (list (checkObjects (car objects) type (car objectColumns) rowNum)) ; check the head first
				(checkObjects (cdr objects) type (cdr objectColumns) rowNum))) ; iterate through the tail
		(T NIL))) ; exit case			

;;;;;;;;;; getObjectPos ;;;;;;;;;;
;;;
;;; Input:
;;; 	1. state = the grid representing objects in the game
;;; 	2. type = (2 = BOX, 4 = STAR)
;;;		3. rowNum = index of the row an object was found in
;;; 
;;; Output: a list of pairs (c, r) which represent the locations of the boxes/stars in a state
;;;		
;;; Behavior: Build the objectColumns list, check every row for boxes/stars in those column coordinates, save the (r, c) of the boxes/stars
;;;
(defun getObjectPos (state type rowNum)
	(cond 
		((listp state) ; list case
			(let* ((row (car state))) ; row = a list element of a state
				(cond
					((null row) NIL) ; exit case
					(T (let* ((numberOfRows (getNumberOfRows state))) ; numberOfRows = number of rows in a state
						(cond ((> numberOfRows 0) ; if the current rowNum is within bounds (can iterate)
								(let* ((objectColumns (getObjectColumns row 0 type))) ; objectColumns = column coordinate of all boxes/stars in state
									(cond
										((null objectColumns) (getObjectPos (cdr state) type (+ 1 rowNum))) ; found no boxes/stars in this row, iterate
										((atom objectColumns) ; singleton case
											(let* ((object (getNthFromRow objectColumns row))) ; object = an object that has a column coordinate from objectColumns
												(cond 
													((and (= type 2) (isBox object)) (list objectColumns rowNum)) ; found box, return (c, r)
													((and (= type 4) (isStar object)) (list objectColumns rowNum)) ; found star, return (c, r)
													(T (getObjectPos (cdr state) type (+ rowNum 1)))))) ; did not find box/star, iterate
										(T (let* ((objects (getObjectsInRow objectColumns row))) ; objects = list of objects in this row with coordinates from objectColumns
												(cond
													((null objects) NIL) ; exit case
													(T (checkObjects objects type objectColumns rowNum)))))))) ; filter out the boxes/stars we're interested in
							(T NIL))))))) ; end (listp state)
			(T NIL))) ; end cond

;;;;;;;;;; absoluteSubtract ;;;;;;;;;;
;;;
;;; Input:
;;;		1. a = an atom or pair
;;;		2. b = an atom or pair
;;;
;;; Output: |a - b|
;;;
;;; Behavior: Simple if-else statements to ensure positive values
;;;
(defun absoluteSubtract (a b)
	(cond
		((and (atom a) (atom b)) ; atom subtraction
			(if (> a b)
				(- a b)
				(- b a)))
		((and (listp a) (listp b)) ; pair subtraction
			(let* (
				(head (if (> (first a) (first b))
					(- (first a) (first b))
					(- (first b) (first a))))
				(tail (if (> (second a) (second b))
					(- (second a) (second b))
					(- (second b) (second a)))))
			(list head tail))) ; returns as (head tail)
		(T 0))) ; exit case

;;;;;;;;;; generateDistanceSet ;;;;;;;;;;
;;;
;;; Input:
;;; 	1. key = a list of (c, r) pairs representing box/star coordinates 
;;;		2. data = a list of (c,r) pairs representing box/star coordinates
;;; 
;;; Output: a list of pairs (c_1-c_2, r_1-r_2) that represent the distance from each box to each star
;;;
;;; Behavior: Calculate the difference between each key (box/star) and each data point (star/box) and build a list containing them
;;;
;(defun generateDistanceSet (key data)
;	(cond
;		((null key) NIL) ; exit case
;		((listp key) ; pair case
;			(let* ((currentKey (first key))) ; take the distance of this key to every element in the data list
;				(cond
;					((null data) NIL) ; exit case
;					((listp data) ; list case
;						(append (list (absoluteSubtract currentKey (first data))) ; build list of distances
;							(generateDistanceSet key (cdr data)) ; iterate through data list
;							(generateDistanceSet (cdr key) data)))))))) ; iterate through keys

(defun generateDistanceList (key data)
	(cond
		((null key) NIL) ; exit case
		((listp key) ; pair case
			(cond
				((null data) NIL) ; exit case
				((listp data) ; list case
					(append (list (absoluteSubtract (first key) (first data))) ; build list of distances
						(generateDistanceList (cdr key) (cdr data)))))))) ; iterate through data list

;;;;;;;;;; moveFirstToLast ;;;;;;;;;;
;;;
;;; Input: pairs = a list of (c, r)
;;;
;;; Output: a list with the first pair moved to the last position
;;;
;;; Behavior: append the first pair to the last position
;;;
(defun moveFirstToLast (pairs)
	(append (cdr pairs) (list (first pairs))))

;;;;;;;;;; swapFirstAndSecond ;;;;;;;;;;
;;;
;;; Input: pairs = a list of (c, r)
;;;
;;; Output: a list with the first and second pair swapped
;;;
;;; Behavior: build new list by append first and second pairs with remainder of list
;;;
(defun swapFirstAndSecond (pairs)
	(append (list (second pairs)) (list (first pairs)) (cddr pairs)))

;;;;;;;;;; generateDistanceSet ;;;;;;;;;;
;;;
;;; Input: 
;;; 	1. boxPositions = (c,r) of all boxes in state
;;;		2. starPositions = (c,r) of all stars in state
;;; 	3. n = number of boxes/stars
;;;
;;; Output: a list of distances from all stars to all boxes
;;;
;;; Behavior: use generateDistanceList on all possible combinations of lists of distances from boxes to stars
;;;
(defun generateDistanceSet (boxPositions starPositions n)
	(cond
		((> n 0) (append (list (generateDistanceList boxPositions starPositions))
			(generateDistanceSet boxPositions (moveFirstToLast starPositions) (- n 1))
			(generateDistanceSet boxPositions (swapFirstAndSecond (moveFirstToLast starPositions)) (- n 1))))
		(T NIL)))

;;;;;;;;;; getMin ;;;;;;;;;;
;;;
;;; Input: numbers = a list of numbers
;;;
;;; Output: the minimum number
;;;
;;; Behavior: compare the first and second numbers, if the first is less than the second recurse on new list (append first to rest)
;;;
(defun getMin (numbers)
	(cond
		((null numbers) 0)
		((atom numbers) numbers)
		((listp numbers)
			(cond
				((= (length numbers) 1) (first numbers))
				(T (if (< (first numbers) (second numbers))
					(getMin (append (list (first numbers)) (cddr numbers)))
					(getMin (cdr numbers))))))))
			
;;;;;;;;;; sumDistanceSet ;;;;;;;;;;;
;;;
;;; Input: data = a list of pairs representing the distances from each box to each star
;;;
;;; Output: the sum of all of the distances
;;;
;;; Behavior: Given a list of pairs, just sum all of the heads and tails
;;;
(defun sumDistanceList (data)
	(cond
		((null data) 0) ; exit case
		((atom data) data) ; singleton case
		((listp data) (+ (sumDistanceList (car data)) (sumDistanceList (cdr data)))) ; sum the values of the pairs
		(T 0))) ; exit case

(defun sumDistanceSet (distanceLists)
	(cond
		((null distanceLists) NIL)
		(T (append (list (sumDistanceList (first distanceLists))) (sumDistanceSet (cdr distanceLists))))))

;;;;;;;;;; distanceHeuristic ;;;;;;;;;;
;;;
;;; Input: state = the grid representing objects in the game
;;;
;;; Output: the minimum distance when pairing all possible boxes to all possible stars
;;;
;;; Behavior: Obtain coordinates for all boxes and all stars, use them to generate a list of distances between each, and return the sum
;;;
(defun distanceHeuristic (state)
	(let* ((boxPosition (getObjectPos state 2 0)) ; boxPosition = list of (c, r) for all boxes in state
		(starPosition (getObjectPos state 4 0))
		(listLength (length boxPosition))) ; starPosition = list of (c, r) for all stars in state
		(cond
			((null boxPosition) 0) ; exit case
			((atom boxPosition) (sumDistanceSet (absoluteSubtract boxPosition starPosition))) ; only 1 box and 1 star, calculate the distance
			((listp boxPosition) ; list case
				(let* ((distanceSet (generateDistanceSet boxPosition starPosition listLength)) ; get all coordinates of all boxes and all stars, generate the distance pairs between them all
					(distanceSums (sumDistanceSet distanceSet)))
					(getMin distanceSums)))))) ; return the minimum distance

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; PREDEFINED PROBLEMS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; UTILITY FUNCTIONS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1))))
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT)))))

(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))));

(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))))

(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)));

(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%"))))

(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)));end defun
