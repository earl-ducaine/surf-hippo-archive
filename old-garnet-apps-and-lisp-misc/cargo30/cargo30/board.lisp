;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey Drake
;;;
;;; Board Handler
;;;


;;; PACKAGE

(in-package "CARGO")


;;; STRUCTURES

(defstruct (chain
	    (:conc-name ch-)
	    (:print-function print-chain))
  "A chain of adjacent points."
  (color NIL :type atom)
  (members NIL :type list)
  (member-count 1 :type fixnum)
  (liberties NIL :type list)
  (liberty-count 0 :type fixnum)
  (adjacent-enemies NIL :type list)
  (scored NIL :type atom))

(defstruct (point
	    (:conc-name pt-)
	    (:print-function print-point))
  "A point on the board."
  (location '(0 . 0) :type cons)
  (color NIL :type atom)
  (neighbors '(NIL NIL NIL NIL) :type list)
  (chain NIL :type (or NULL chain)))

(defun print-point (point stream depth)
  "Special print function to prevent bottomless recursion in printing point structures."
  (declare (ignore depth))
  (format stream "~a point at ~a,~a~&"
	  (pt-color point)
	  (car (pt-location point))
	  (cdr (pt-location point)))
  (format stream "Neighbors: ~a~&"
	  (mapcar #'(lambda (x) (if x (pt-color x)))
		  (pt-neighbors point)))
  (format stream "~a" (pt-chain point)))

(defun print-chain (chain stream depth)
  "Special print function to prevent bottomless recursion in printing chain structures."
  (declare (ignore depth))
  (format stream "===~a chain===~&" (ch-color chain))
  (format stream "~a member~:p: ~a~&"
	  (ch-member-count chain)
	  (mapcar #'pt-location (ch-members chain)))
  (format stream "~a libert~:@p: ~a~&"
	  (ch-liberty-count chain)
	  (mapcar #'pt-location (ch-liberties chain)))
  (format stream "Adjacent enemy chains:~&")
  (mapcar #'(lambda (x)
	      (format stream " ~a stone~:p including ~a~&"
		      (ch-member-count x)
		      (pt-location (car (ch-members x)))))
	  (ch-adjacent-enemies chain)))


;;; GLOBAL VARIABLES

(defvar *move-number* 0           "Number for next stone played.")
(defvar *current-player* :black  "Color of current player.")
(defvar *chains* NIL               "Master list of chains.")
(defvar *history* NIL              "Stack of undoing functions.")
(defvar *passes* 0                 "Number of consecutive passes so far.")
(defvar *ko-point* NIL             "Point which is illegal to play in due to ko.")
(defvar *game-over* NIL            "T iff the playing stage of the game is over.")
(defvar *scoring* NIL		   "T iff the game is in the removing-doomed-chains stage.")
(defvar *board* NIL                "Master array of points.")


;;; INTERNAL FUNCTIONS

(defun neighbor (direction col row)
  "Returns the point next to (COL . ROW) in DIRECTION, from *BOARD*.  Returns NIL if there is no such point.
DIRECTION should be :north, :south, :east, or :west."
  (let ((c (case direction
	     ((:north :south) col)
	     (:west (1- col))
	     (:east (1+ col))))
	(r (case direction
	     ((:west :east) row)
	     (:north (1+ row))
	     (:south (1- row))))
	(max (array-dimension *board* 0)))		    ; 1 more than the number of lines on the board
    (if (and (> r 0) (< r max)
	     (> c 0) (< c max))
	(aref *board* c r)
      NIL)))

(defun initialize-board (lines)
  "Sets up the initial values for *BOARD*."
  (cond
   ((typep *board*					    ; If *BOARD* is the right size, recycle it
	   `(simple-array point (,(1+ lines) ,(1+ lines))))
    (for (col 1 lines)					    ; Clear the color and chain fields of each point
	 (for (row 1 lines)
	      (let ((point (aref *board* col row)))
		(setf (pt-color point) NIL)
		(setf (pt-chain point) NIL)))))
   (T							    ; Otherwise, make a new array
    (setq *board*
	  (make-array `(,(1+ lines) ,(1+ lines))
		      :element-type 'point))
    (for (col 1 lines)					    ; Make a point structure for each point
	 (for (row 1 lines)
	      (setf (aref *board* col row) (make-point))))
    (for (col 1 lines)					    ; Link the points together
	 (for (row 1 lines)
	      (let ((point (aref *board* col row)))
		(setf (pt-location point) (cons col row))
		(setf (pt-neighbors point)
		      (list (neighbor :north col row)
			    (neighbor :south col row)
			    (neighbor :east col row)
			    (neighbor :west col row)))))))))

(defun undo-function-base ()
  "Do standard bookkeeping for undoing moves."
  (setq *current-player* (opposite *current-player*))
  (decf *move-number*)
  (cond
   ((= 0 *passes*)
    (gui:message "~a to play" *current-player*))
   (T
    (gui:message "~a makes ~:r pass~&~a to play"
		 (opposite *current-player*)
		 *passes*
		 *current-player*))))
  
(defmacro undo-function-new (point chain enemies passes)
  "An auxiliary macro for create-new-chain.  Returns a function for undoing the move."
  `#'(lambda ()
       (setf (pt-color ,point) NIL)
       (gui:set-stones (list (pt-location ,point)
			     NIL :previous))
       (pull ,chain *chains*)				    ; Remove the created chain
       (setf (pt-chain ,point) NIL)
       (dolist (enemy ,enemies)				    ; Update adjacent enemy chains
	 (pull ,chain (ch-adjacent-enemies enemy))
	 (push ,point (ch-liberties enemy))
	 (incf (ch-liberty-count enemy)))
       (setq *passes* ,passes)				    ; Update global variables
       (undo-function-base)))

(defmacro undo-function-join (point ally enemies
				    prev-liberties
				    prev-liberty-count
				    prev-adjacent-enemies
				    passes)
  "An auxiliary macro for join-friendly-chain.  Returns a function for undoing the move."
  `#'(lambda ()
       (setf (pt-color ,point) NIL)			    ; Remove the stone
       (gui:set-stones (list (pt-location ,point)
			     NIL :previous))
       (setf (pt-chain ,point) NIL)
       (pull ,point (ch-members ,ally))			    ; Restore the chain to its previous stats
       (decf (ch-member-count ,ally))
       (setf (ch-liberties ,ally) ,prev-liberties)
       (setf (ch-liberty-count ,ally) ,prev-liberty-count)
       (setf (ch-adjacent-enemies ,ally) ,prev-adjacent-enemies)
       (dolist (enemy ,enemies)				    ; Update adjacent enemy chains
	 (pull ally (ch-adjacent-enemies enemy))
	 (push ,point (ch-liberties enemy))
	 (incf (ch-liberty-count enemy)))
       (setq *passes* ,passes)				    ; Update global variables
       (undo-function-base)))
       
(defmacro undo-function-merge (point new-chain chains enemies passes)
  "An auxiliary macro for merge-friendly-chains.  Returns a function for undoing the move."
  `#'(lambda ()
       (setf (pt-color ,point) NIL)			    ; Remove the stone
       (gui:set-stones (list (pt-location ,point)
			     NIL :previous))
       (pull ,new-chain *chains*)			    ; Get rid of the merged chain
       (setf (pt-chain ,point) NIL)
       (setq *chains* (append ,chains *chains*))	    ; Restore the lost chains
       (dolist (chain ,chains)
	 (dolist (stone (ch-members chain))
	   (setf (pt-chain stone) chain))
	 (dolist (enemy (ch-adjacent-enemies chain))
	   (push chain (ch-adjacent-enemies enemy))))
       (dolist (enemy ,enemies)
	 (pull ,new-chain (ch-adjacent-enemies enemy))
	 (push ,point (ch-liberties enemy))
	 (incf (ch-liberty-count enemy)))
       (setq *passes* ,passes)				    ; Update global variables
       (undo-function-base)))

(defun create-new-chain (point liberties enemies)
  "An auxiliary function for update-chains.  Creates a new chain containing only POINT, and updates the chains
listed in ENEMIES."
  (let ((chain						    ; Create the chain
	 (make-chain
	  :color *current-player* :members (list point)
	  :member-count 1        :liberties liberties
	  :adjacent-enemies enemies))
	(prev-passes *passes*))
    (setf (ch-liberty-count chain)			    ; Count the liberties for the chain
	  (length (ch-liberties chain)))
    (setf (pt-chain point) chain)			    ; Attach the chain to POINT
    (push chain *chains*)				    ; Add the chain to the master list of chains
    (dolist (enemy enemies)				    ; Update adjacent enemy chains
      (push chain (ch-adjacent-enemies enemy))
      (pull point (ch-liberties enemy))
      (decf (ch-liberty-count enemy)))
    (push (undo-function-new point chain enemies prev-passes)
	    *history*)))

(defun join-friendly-chain (point liberties ally enemies)
  "An auxiliary function for update-chains.  Adds POINT to ALLY, and updates the chains in ENEMIES."
  (let ((prev-liberties (copy-list (ch-liberties ally)))
	(prev-liberty-count (ch-liberty-count ally))
	(prev-adjacent-enemies (copy-list (ch-adjacent-enemies ally)))
	(prev-passes *passes*))
  (push point (ch-members ally))			    ; Add POINT to ALLY
  (incf (ch-member-count ally))
  (setf (ch-liberties ally)				    ; Update ALLY's liberties
	(remove point
		(union (ch-liberties ally) liberties)))
  (setf (ch-liberty-count ally)
	(length (ch-liberties ally)))
  (setf (ch-adjacent-enemies ally)
	(union (ch-adjacent-enemies ally) enemies))
  (setf (pt-chain point) ally)				    ; Attach ALLY to POINT
  (dolist (enemy enemies)				    ; Update adjacent enemy chains
    (pushnew ally (ch-adjacent-enemies enemy))
    (pull point (ch-liberties enemy))
    (decf (ch-liberty-count enemy)))
  (push (undo-function-join point ally enemies prev-liberties prev-liberty-count prev-adjacent-enemies prev-passes)
	*history*)))

(defun merge-into (chain allies)
  "An auxiliary function for merge-friendly-chains.  Merges ALLIES into CHAIN."
  (dolist (ally allies)					    ; This is the cdr of allies in merge-friendly-chains
    (setf (ch-members chain)
	  (append (ch-members chain)
		  (copy-list (ch-members ally))))
    (incf (ch-member-count chain) (ch-member-count ally))
    (setf (ch-liberties chain)
	  (union (ch-liberties chain)
		 (copy-list (ch-liberties ally))))
    (setf (ch-adjacent-enemies chain)
	  (union (ch-adjacent-enemies chain)
		 (copy-list (ch-adjacent-enemies ally))))))
  
(defun merge-friendly-chains (point liberties allies enemies)
  "An auxiliary function for update-chains.  Adds POINT to one of ALLIES, merges the other ALLIES into that
chain, and updates the chains in ENEMIES."
  (let* ((ally1 (car allies))
	 (chain (make-chain				    ; Create a new chain, so the old ones can be retrieved
		:color *current-player*
		:members
		(cons point (copy-list (ch-members ally1)))
		:member-count (1+ (ch-member-count ally1))
		:liberties
		(union liberties
		       (copy-list (ch-liberties ally1)))
		:adjacent-enemies
		(append enemies
			(copy-list
			 (ch-adjacent-enemies ally1)))))
	 (prev-passes *passes*))
    (merge-into chain (cdr allies))			    ; Merge the other allies into CHAIN
    (dolist (ally allies)				    ; Detach the merged chains
      (pull ally *chains*)
      (dolist (member (ch-members ally))
	(setf (pt-chain member) chain)))
    (pull point (ch-liberties chain))			    ; Update a couple of statistics on the new chain
    (setf (ch-liberty-count chain)
	  (length (ch-liberties chain)))
    (push chain *chains*)
    (setf (pt-chain point) chain)			    ; Attach the chain to the point
    (dolist (enemy (ch-adjacent-enemies chain))		    ; Update adjacent enemy chains
      (pushnew chain (ch-adjacent-enemies enemy))
      (setf (ch-adjacent-enemies enemy)
	    (set-difference (ch-adjacent-enemies enemy)
			    allies))
      (pull point (ch-liberties enemy)))
    (dolist (enemy enemies)
      (decf (ch-liberty-count enemy)))
    (push (undo-function-merge
	   point chain allies enemies prev-passes)
	  *history*)))

(defun recompute-liberties (chain)
  "Computes the liberties and liberty-count slots of CHAIN from scratch."
  (setf (ch-liberties chain) NIL)
  (dolist (point (ch-members chain))			    ; Find 'em
    (dolist (neighbor (pt-neighbors point))
      (if (and neighbor (not (pt-color neighbor)))
	  (pushnew neighbor (ch-liberties chain)))))
  (setf (ch-liberty-count chain)			    ; Count 'em
	(length (ch-liberties chain))))

(defmacro undo-function-capture (previous-function chains point player prev-ko)
  "An auxiliary macro for capture.  Wraps a function undoing the capture of CHAINS around PREVIOUS-FUNCTION."
  `#'(lambda ()
       (let ((stones-to-set
	      (if (eq ,player (ch-color (car ,chains)))
		  (list `(,(pt-location ,point)
			  T
			  :previous)))))
	 (dolist (chain ,chains)			    ; Restore each chain
	   (dolist (point (ch-members chain))		    ; Restore each point
	     (push (list (pt-location point)
			 (ch-color chain)
			 :previous)
		   stones-to-set)
	     (setf (pt-color point) (ch-color chain))
	     (setf (pt-chain point) chain))
	   (push chain *chains*)
	   (dolist (enemy (ch-adjacent-enemies chain))
	     (push chain (ch-adjacent-enemies enemy))
	     (recompute-liberties enemy)))
	 (setq *ko-point* ,prev-ko)			    ; Rest the ko point
	 (apply #'gui:set-stones stones-to-set)		    ; Update graphics
	 (funcall ,previous-function))))		    ; Do all of the non-capture undoing work
+
(defun capture (chains point)
  "Captures CHAINS (due to a move at POINT) and updates the relevant data structures."
  (if chains
      (let ((stones-to-set NIL)
	    (player *current-player*)
	    (prev-ko *ko-point*))
	(dolist (chain chains)				    ; Capture each chain
	  (dolist (point (ch-members chain))		    ; Capture each stone
	    (push (list (pt-location point) NIL NIL)
		  stones-to-set)
	    (setf (pt-color point) NIL)
	    (setf (pt-chain point) NIL))
	  (pull chain *chains*)
	  (dolist (enemy (ch-adjacent-enemies chain))
	    (pull chain (ch-adjacent-enemies enemy))
	    (recompute-liberties enemy)))
	(if (and (single chains)			    ; Set the ko point if necessary
		 (= 1 (ch-member-count (car chains)))	    ;  This line also filters out suicidal moves
		 (pt-chain point)
		 (= 1 (ch-liberty-count (pt-chain point))))
	    (setq *ko-point* (car (ch-members (car chains)))))
	(apply #'gui:set-stones stones-to-set)
	(push (undo-function-capture (pop *history*) chains point player prev-ko)
	      *history*))))

(defmacro undo-function-suicide (point passes)
  "An auxiliary macro for update-chains.  Does some bookkeeping for undoing a suicidal move."
  `#'(lambda ()
       (gui:set-stones (list (pt-location ,point)
			     NIL :previous))
       (setq *passes* ,passes)				    ; Update global variables
       (undo-function-base)))

(defmacro with-updating-bindings (&body body)
  "An auxiliary macro for update-chains.  Binds a bunch of stuff to make that function smaller."
  `(with-point
    (col row location)
    (let* ((point (aref *board* col row))		    ; The point where the move was made
	   (liberties					    ; Liberties of POINT
	    (delete-if-not
	     #'(lambda (x)
		 (and x (not (pt-color x))))
	     (copy-list (pt-neighbors point))))
	   (neighbors					    ; Neighboring chains of POINT
	    (delete-duplicates
	     (delete-if-not
	      #'chain-p
	      (mapcar #'(lambda (x)
			  (if x
			      (pt-chain x)))
		      (pt-neighbors point)))))
	   (allies					    ; Friendly neighboring chains
	    (remove-if-not
	     #'(lambda (x)
		 (eq (ch-color x) *current-player*))
	     neighbors))
	   (enemies					    ; Enemy neighboring chains
	    (remove-if
	     #'(lambda (x)
		 (eq (ch-color x) *current-player*))
	     neighbors))
	   (ataried-enemies				    ; Enemy neighboring chains with one liberty
	    (remove-if-not #'(lambda (x)
			       (= 1 (ch-liberty-count x)))
			   enemies)))
      ,@body)))

(defun update-chains (location)
  "Updates chain structures after a move at LOCATION by *CURRENT-PLAYER*."
  (with-updating-bindings
   (cond
    ((suicidal-p location)				    ; Case for suicidal moves
     (setf (pt-color (aref *board* col row)) NIL)	    ; This cancels the change made by play-at
     (let ((prev-passes *passes*))
       (push (undo-function-suicide point prev-passes)
	     *history*))
     (capture allies point))
    ((null allies)					    ; Case for isolated moves
     (create-new-chain point liberties enemies)
     (capture ataried-enemies point))
    ((single allies)					    ; Case for moves adjacent to one ally
     (join-friendly-chain
      point liberties (car allies) enemies)
     (capture ataried-enemies point))
    (T							    ; Case for connecting moves
     (merge-friendly-chains
      point liberties allies enemies)
     (capture ataried-enemies point)))))

(defmacro undo-function-pass ()
  "An auxiliary macro for pass.  Returns a function for undoing the move."
  `#'(lambda ()
       (decf *passes*)
       (undo-function-base)
       (if *game-over*
	   (setq *scoring* (setq *game-over* NIL)))))

(defmacro undo-function-doomed (chain)
  "An auxiliary macro for remove-doomed-chain.  Returns a function for undoing the removal."
  `#'(lambda ()
       (let ((stones-to-set NIL))
	 (dolist (point (ch-members ,chain))		    ; Restore the points in CHAIN
	   (push (list (pt-location point)
		       (ch-color ,chain)
		       :previous)
		 stones-to-set)
	   (setf (pt-color point)
		 (ch-color ,chain))
	   (setf (pt-chain point) ,chain))
	 (push ,chain *chains*)				    ; Restore CHAIN
	 (dolist (enemy (ch-adjacent-enemies ,chain))
	   (push ,chain (ch-adjacent-enemies enemy))
	   (recompute-liberties enemy))
	 (apply #'gui:set-stones stones-to-set))))	    ; Re-draw graphics

(defun score-chain (point)
  "An auxiliary function for count-score.  Finds a chain of NIL points around POINT, sets its :scored slot to
:black, :white, or :dame, and draws light or dark gray stones in the area."
  (setf (pt-chain point)				    ; Create a new chain
	(make-chain :members (list point)))
  (let ((chain (pt-chain point)))
    (do ((old-points)					    ; This is depth-first search
	 (new-points
	  (pt-neighbors point)
	  (aif (pop old-points)
	       (pt-neighbors it))))
	((nor old-points new-points))			    ; Keep going until there are no more points to explore
      (dolist (neighbor new-points)			    ; For each neighbor of the point being considered:
	(cond
	 ((not neighbor)
	  NIL)
	 ((member neighbor (ch-members chain))		    ;  If it's already in the chain, ignore it
	  NIL)
	 ((not (pt-color neighbor))			    ;  If it's empty and not in the chain, add it
	  (push neighbor (ch-members chain))
	  (incf (ch-member-count chain))
	  (push neighbor old-points)
	  (setf (pt-chain neighbor) chain))
	 ((not (ch-scored chain))			    ;  If this is the first stone encountered, set the
	  (setf (ch-scored chain)			    ;   chain's :scored slot to this color
		(pt-color neighbor)))
	 ((eq (pt-color neighbor)			    ;  If this is another stone of a previously seen color,
	      (ch-scored chain))			    ;   do nothing
	  NIL)
	 (T						    ;  If this stone is the opposite color, set the chain's
	  (setf (ch-scored chain) :dame)))))		    ;   :scored slot to :dame
    (case (ch-scored chain)				    ; Draw shaded stones in the area
      (:black
       (apply #'gui:set-stones
	      (mapcar #'(lambda (x)
			  `(,(pt-location x) :dark-gray NIL))
		      (ch-members chain))))
      (:white
       (apply #'gui:set-stones
	      (mapcar #'(lambda (x)
			  `(,(pt-location x) :light-gray NIL))
		      (ch-members chain)))))))

(defun undo-function-score ()
  "An auxiliary function for count-score.  Undoes the graphics and data structures created by count-score."
  (let ((lines (1- (array-dimension *board* 0)))
	(stones-to-set NIL))
    (for (col 1 lines)					    ; Destroy territory chains
	 (for (row 1 lines)
	      (let ((point (aref *board* col row)))
		(unless (pt-color point)
		  (if (not (eq (ch-scored (pt-chain point)) :dame))
		      (push `(,(pt-location point) NIL :previous)
			    stones-to-set))
		  (setf (pt-chain point) NIL)))))
    (setq *scoring* T)
    (gui:message "~a~&~a~&~a~&~a"
		 "Three passes"
		 "Game over"
		 "Click on dead chains"
		 "Press Q when done")
    (apply #'gui:set-stones stones-to-set)))		    ; Remove shaded stones


;;; EXPORTED FUNCTIONS

(proclaim '(inline in-atari-p))

(defun in-atari-p (chain)
  "Returns T iff CHAIN has exactly one liberty."
  (= 1 (ch-liberty-count chain)))

(defun suicidal-p (location)
  "Returns T iff LOCATION is a suicidal move for *CURRENT-PLAYER*."
  (not							    ; The move is suicidal if no adjacent point:
   (find-if
    #'(lambda (x)
	(if x
	    (or (not (pt-color x))			    ;  is empty,
		(and (eq *current-player* (pt-color x))	    ;  is friendly and has more than one liberty,
		     (> (ch-liberty-count (pt-chain x)) 1))
		(and (eq (opposite *current-player*)	    ;  or is enemy and is in atari
			 (pt-color x))
		     (in-atari-p (pt-chain x))))))
    (pt-neighbors
     (aref *board* (car location) (cdr location))))))

(defun legal-p (location)
  "Returns T iff LOCATION is a legal move for *CURRENT-PLAYER* (under Ing rules)."
  (let ((point						    ; A move is legal if none of the following hold:
	 (aref *board* (car location) (cdr location))))
    (nor (pt-color point)				    ;  The point is occupied
	 (and *ko-point* (eq *ko-point* point))		    ;  The point is unavailable due to ko
	 (and (suicidal-p location)			    ;  The point is surrounded by enemy stones
	      (every #'(lambda (x)
			 (or (not x)
			     (eq (pt-color x)
				 (opposite *current-player*))))
		     (pt-neighbors point))))))

(defun play-at (location)
  "Plays a stone for *CURRENT-PLAYER* at LOCATION.  Assumes the move is legal."
  (with-point
   (col row location)
   (setf (pt-color (aref *board* col row))		    ; A suicidal move will cancel this
	 *current-player*)
   (update-chains location)				    ; Do all the ugly data structure updating
   (gui:set-stones (list location
			 (pt-color (aref *board* col row))  ; This must be checked, in case the move was suicidal
			 (format NIL "~d" *move-number*)))
   (setq *current-player* (opposite *current-player*))	    ; Update some global variables
   (setq *passes* 0)
   (incf *move-number*)
   (gui:message "Last move at ~a~&~a to play"
		(coordinates location) *current-player*)))

(defun pass ()
  "Makes a pass move."
  (setq *current-player* (opposite *current-player*))
  (incf *passes*)
  (incf *move-number*)
  (cond
   ((> *passes* 2)
    (setq *game-over* T)
    (setq *scoring* T)
    (gui:message "~a~&~a~&~a~&~a"
		 "Three passes"
		 "Game over"
		 "Click on dead chains"
		 "Press Q when done"))
   (T
    (gui:message "~a makes ~:r pass~&~a to play"
		 (opposite *current-player*)
		 *passes*
		 *current-player*)))
  (push (undo-function-pass) *history*))

(defun remove-doomed-chain (location)
  "Removes the chain of stones including LOCATION.  Assumes there is such a chain."
  (let ((chain
	 (pt-chain
	  (aref *board* (car location) (cdr location))))
	(stones-to-set NIL))
    (dolist (point (ch-members chain))			    ; Clear the points in the chain
      (push (list (pt-location point) NIL NIL)
	    stones-to-set)
      (setf (pt-color point) NIL)
      (setf (pt-chain point) NIL))
    (pull chain *chains*)				    ; Remove the chain itself
    (dolist (enemy (ch-adjacent-enemies chain))
      (pull chain (ch-adjacent-enemies enemy))
      (recompute-liberties enemy))
    (apply #'gui:set-stones stones-to-set)		    ; Erase the stones
    (push (undo-function-doomed chain)
	  *history*)))

(defun handle-event (event location)
  "Takes an action appropriate to user input.  This should be called by the event loop in the GUI package."
  (case event
    ((:leftdown #\RETURN)				    ; Play a move (or remove a dead chain in scoring)
     (cond
      ((and (not *scoring*) (legal-p location))
       (play-at location))
      ((and *scoring*
	    (pt-color (aref *board*
			    (car location)
			    (cdr location))))
       (remove-doomed-chain location))
      (T
       (gui:beep))))
    ((:rightdown #\SPACE)				    ; Pass
     (unless *scoring*
       (pass)))
    ((:middledown #\BACKSPACE)				    ; Undo
     (if *history*
	 (funcall (pop *history*))
       (gui:beep)))
    ((#\q #\Q)						    ; Quit
     (cond
      (*game-over*
       (count-score)
       (setq *scoring* NIL))
      (T
       (setq *game-over* T))))
    (T
     NIL)))

(defun count-score ()
  "Counts the score, assuming everything on the board is alive.  Returns black score - white score."
  (let ((lines (1- (array-dimension *board* 0)))
	(score 0))
    (for (col 1 lines)					    ; Score each point, keeping a total
	 (for (row 1 lines)
	      (let* ((point (aref *board* col row))
		     (chain (pt-chain point)))
		(cond
		 ((not chain)
		  (score-chain point))
		 ((ch-scored chain)
		  NIL)
		 (T
		  (setf (ch-scored chain)
			(ch-color chain))))
		(case (ch-scored (pt-chain point))	    ; Alter SCORE appropriately
		  (:black (incf score))
		  (:white (decf score))))))
    (push #'undo-function-score *history*)
    (gui:message "~a wins by ~d points"			    ; Announce the score
		 (if (> score 0) "Black" "White")
		 score)
    score))

(defun play-go (&optional
		(lines
		 (if *board*
		     (1- (array-dimension *board* 0))
		   19)))
  "Sets up and begins a game of Go."
  (gui:clear-board lines)
  (setq *move-number* 1     *current-player* :black
	*chains* NIL        *history* NIL
	*ko-point* NIL      *passes* 0
	*game-over* NIL     *scoring* NIL)
  (initialize-board lines)
  (gui:message "~a~&~a~&~a~&~2a"
	       "Play:  Left button or RETURN"
	       "Undo:  Middle button or BACKSPACE"
	       "Pass:  Right button or SPACEBAR"
	       "BLACK to play"))
