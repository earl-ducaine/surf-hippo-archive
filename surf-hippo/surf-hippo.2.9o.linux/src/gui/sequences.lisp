;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GUI Source file: strings_and_lists.lisp


(IN-PACKAGE "WINDOWS-HACK")

;; Various useful functions for handling sequences, many of which probably can be
;; done better if I read CLTL more thoroughly. Functions specialized for strings are also found in
;; strings.lisp.  


;; *********************************
;; Hash Tables
;; *********************************

(defun hash-table-list (table)
  (when table
    (loop for elt being the hash-value of table collect elt)))


(defun hash-table-empty (table)
  (= 0 (hash-table-count (eval table))))
   
;;; PRINT-TABLE-KEYS and PRINTIT Functions to print out the keys of a hash table.
(defun print-table-keys (table)
  (maphash 'printit table))

(defun printit (name nd)
  (declare (ignore nd))
  (print name))

(defun named-structure-symbol (st)
  (type-of st))


;; *********************************
;; Lists, Arrays And Sequences
;; *********************************

(defun remove-all (elements-to-remove sequence)
  (if elements-to-remove
    (remove-all (cdr elements-to-remove) (remove (car elements-to-remove) sequence))
    sequence))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-double-float-listx*listy (df-listx df-listy)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for x double-float in df-listx
	for y double-float in df-listy
	summing (the df (* y x)) into out double-float finally (return out)))

(defun sum-double-float-listx*listx (df-listx)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for x double-float in df-listx
	summing (the df (* x x)) into out double-float finally (return out)))

(defun sum-float-list (list)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for val single-float in list sum val into out single-float finally (return out)))

(defun sum-double-float-list (list)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for val double-float in list sum val into out double-float finally (return out)))

(defun max-of-seq (seq)
  (typecase seq
    (array (loop for val across seq maximizing val))
    (cons (max-of-list seq))))

(defun min-of-seq (seq)
  (typecase seq
    (array (loop for val across seq minimizing val))
    (cons (min-of-list seq))))

     
(defun max-of-list (list)
  (loop for sub-list in list maximizing
	(typecase sub-list
		  (cons (max-of-list sub-list))
		  (t sub-list))))

(defun min-of-list (list)
  (loop for sub-list in list minimizing
	(typecase sub-list
	  (cons (min-of-list sub-list))
	  (t sub-list))))


;;; MAXIMIZE-VAL-OR-SEQ, MINIMIZE-VAL-OR-SEQ, A-BIT-MORE, A-BIT-LESS - Some useful utilities.
;;; VAL-OR-SEQ can be a single number, an array, a list, a list of lists, or a list of arrays, an
;;; array of lists, etc, etc. All arrays must be 1D.

(defun maximize-val-or-seq (val-or-seq)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (cond ((numberp val-or-seq)
	 val-or-seq)
	((arrayp val-or-seq)
	 (loop for index fixnum from 0 to (1- (length val-or-seq))
	       maximize (aref val-or-seq index)))
	(t
	 (loop for val-or-seq in val-or-seq maximize
	       (if (numberp val-or-seq) val-or-seq (maximize-val-or-seq val-or-seq))))))

     
(defun minimize-val-or-seq (val-or-seq &optional min-limit)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (cond ((numberp val-or-seq)
	 val-or-seq)
	((arrayp val-or-seq)
	 (loop for index fixnum from 0 to (1- (length val-or-seq))
	       when (or (not (numberp min-limit)) (> (aref val-or-seq index) min-limit))
	       minimize (aref val-or-seq index)))
	(t
	 (loop for thing in val-or-seq
	       when (or (not (numberp thing))
			(not (numberp min-limit))
			(>= thing min-limit))
	       minimize
	       (if (numberp thing) thing (minimize-val-or-seq thing min-limit))))))

(defun a-bit-more (val-or-seq &optional (how-much 0.0) fix-it)
  (let* ((max (maximize-val-or-seq val-or-seq))
	 (adjusted (+ max (abs (* how-much max)))))
    (if fix-it (ceiling adjusted) adjusted)))

(defun a-bit-less (val-or-seq &optional (how-much 0.0) fix-it min-limit)
  (let* ((min (minimize-val-or-seq val-or-seq  min-limit))
	 (adjusted (- min (abs (* how-much min)))))
    (if fix-it (ceiling adjusted) adjusted)))


(defun normalize-sequence (sequence &optional (min-is-zero-p t))
  "Return a list of normalized values derived from the numbers in the 1d SEQUENCE. Minimum value is 0
when MIN-IS-ZERO-P is true [default], otherwise taken as minimum of sequence."
  (let ((max (max-of-seq sequence))
	(min (if min-is-zero-p 0.0 (min-of-seq sequence))))
    (loop for val in (sequence-to-list sequence)
	  collect (/ (- val min) (- max min)))))

(defun add-val-to-float-list (val list)
  ;; VAL and members of LIST must be single floats.
  (mapcar #'(lambda (orig-value)
	      (declare (optimize (safety 0) (speed 3) (space 1))
		       (single-float val orig-value))
	      (+ val orig-value))
	  list))

(defun scale-float-list (scale list)
  ;; SCALE and members of LIST must be single floats.
  (mapcar #'(lambda (orig-value)
	      (declare (optimize (safety 0) (speed 3) (space 1))
		       (single-float scale orig-value))
	      (* scale orig-value))
	  list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-member-equal (item list)
  ;;  "Returns tail of list beginning with first element satisfying EQLity, :test, or :test-not with a given item."
  (do ((list list (cdr list)))
      ((null list) nil)
    (let ((car (car list)))
      (when (equal item car) 
	(return list)))))


(defun my-delete (elt list)
  ;; Guaranteed to destructively remove ELT from LIST, as long as ELT is not the car of LIST.
  (do ((list list (cdr list)))
      ((or (eq elt (cadr list)) (null list))
       (when list (rplacd list (cddr list))))))

(defun fast-sort-delete-duplicates (num-list)
  ;; For floats. Non-destructive since SORT is called on copy of NUM-LIST.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let ((out '())
	(temp-list (copy-list num-list)))
    (do ((sorted-num-list
	  (sort temp-list #'(lambda (val1 val2) (> (the single-float val1)(the single-float val2))))
	  (cdr sorted-num-list)))
	((null (cadr sorted-num-list))
	 (push (car sorted-num-list) out))
      (unless (= (the sf (car sorted-num-list))
		 (the sf (cadr sorted-num-list)))
	(push (car sorted-num-list) out)))
    out))


(defun double-float-in-list-p (list)
  (loop for x in list when (eq 'double-float (type-of x)) do (return t)))

(defun double-lists (list-of-lists)
  (nconc (copy-list list-of-lists) list-of-lists))

(defun add-to-end (list last)
  (reverse (cons last (reverse list))))

(defun atomize-list (thing)
  "If THING is an atom, returns THING; if THING is a list with one element, returns that element, otherwise returns THING."
  (if (atom thing)
    thing
    (if (= 1 (length thing))
      (car thing)
      thing)))

(defun generic-intersection (foo bar)
  "Returns as a list the commonality between the atoms or lists FOO and BAR."
  (cond
   ((and (consp foo)
	 (consp bar))
    (intersection foo bar))
   ((consp bar) (member foo bar))
   ((consp foo) (member bar foo))
   ((eq foo bar) (list foo))))

(defun thing-in-array-p (thing array)
  (loop for index from 0 to (1- (length array))
	when (eq thing (aref array index))
	do (return t)))

(defun list-of-nums (length &optional (start 0.0) (increment 1.0))
  "Return a list of LENGTH of increasing numbers (type determined by START [default 0.0] and INCREMENT [default 1.0]."
  (loop for i from start by increment for count from 0 to (1- (round length)) collect i))

(defun list-of-sf-nums (length &optional (start 0.0) (increment 1.0))
  "As LIST-OF-NUMS with returned list of single floats."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((start (s-flt start))
	(increment (s-flt increment)))
    (loop for i single-float from start by increment
	  for count fixnum from 0 to (1- (the fn (round length)))
	  collect i)))
			 
(defun array-of-nums (length &optional (start 0.0) (increment 1.0))
  "Return an array of LENGTH increasing numbers (type determined by START [default 0.0] and INCREMENT [default 1.0]."
  (list-to-array (list-of-nums length start increment)))

(defun coerce-to-list (stuff)
  (when stuff (if (consp stuff) stuff (list stuff))))

(defun string-remove (string list)
  (remove string list :test #'equal))

(defun push-lists-onto-list (lists list2)
  (loop for list in (reverse lists) do (push list list2))
  list2)

(defun every-nth (list n &optional (offset 0))
  (loop for val in list
	for count from 0
	when (= (mod (- count offset) n) 0)
	collect val))

(defun chop-list (list chop-length)
  (let ((count 0)
	(partial-out '())
	(out '()))
    (dotimes (i (length list))
      (push (car list) partial-out)
      (setq list (cdr list)
	    count (1+ count))
      (cond ((= (+ count 2) chop-length)
	     (push (reverse partial-out) out)
	   ;; Retain last xy values so that lines are joined.
 	     (setq partial-out (list (car partial-out)(cadr partial-out))
		   count 0))))
    (if (> (length partial-out) 2) (push (reverse partial-out) out))
    (reverse out)))

(defun order-list-from-key-list (list key-list &optional other-list)
    (let ((output '()))
      (loop for key in key-list do
	    (let ((search-result-list (member key list :test 'equal)))
	      (if search-result-list
		  (setq output
			(concatenate 'list (list (nth (- (length list) (length search-result-list))
					  (if other-list other-list list)))
			       output)))))
      (reverse output)))

(defun split-up (list split-length)
  (let ((subout '())
	(out '()))
    (do ((list list (cdr list))
	 (i 0 (1+ i)))
	((null list) (reverse out))
      (push (car list) subout)
      (if (or (null list) (= i split-length))
	  (progn
	    (push (reverse subout) out)
	    (setq i 0
		  subout '()))))))

(defun split-up (list split-length)
  (let ((sub))
    (loop for val in list
	  for count from 1
	  do (push val sub)
	  when (or (= count (length list))
		   (= 0 (mod count split-length)))
	  collect (reverse sub) into out
	  and do (setq sub nil)
	  finally (return out))))

(defun complement-list (list)
  "Returns a BOOLEAN complement of the elements in LIST."
  (mapcar 'null (mapcar 'true-p list)))
  
(defun last-element (seq)
  (typecase seq
    (array (aref seq (1- (length seq))))
    (cons (car (last seq)))))

(defun first-element (seq)
  (typecase seq
    (array (aref seq 0))
    (cons (car seq))))

(defun number-sequence-p (sequence)
  (let ((list (sequence-to-list sequence)))
    (when list
      (loop for elt in list
	    unless (numberp elt) do (return nil)
	    finally (return t)))))
	
(defun null-list-p (list)
  (loop for elt in list when elt do (return nil) finally (return t)))

(defun flatten (the-list)
  (mapcan #'(LAMBDA (x) (IF (LISTP x) (flatten x) (LIST x)))
	  the-list))

(defun flatten-sequence-to-list (sequence &rest other-sequences)
  (flatten-list sequence other-sequences))

(defun flatten-list (sequence &rest other-sequences)
  (when (or sequence (car other-sequences))
    (let ((sequence (typecase sequence
		      (string sequence)
		      (array (array-to-list sequence))
		      (list sequence)
		      (t sequence)))
	  (other-sequences (if (equal other-sequences '(nil)) nil
			       other-sequences)))
      (if (and (atom sequence) (not other-sequences))
	  (list sequence)
	  (loop for elt in (concatenate 'list (coerce-to-list sequence) other-sequences)
		nconcing (if (sequencep elt) (flatten-list elt) (list elt)))))))

(defun sequencep (thing)
  (or (listp thing) (arrayp thing)))

(defun no-nils (list)
  (loop for x in list when x collect x))

(defun clean-up-list (list)
  "Remove all NILs in list and delete duplicates."
  (delete-duplicates (no-nils list)))

(defun flatten-no-nils-list (sequence &rest other-sequences)
  (no-nils (flatten-list sequence other-sequences)))

(defun flatten-no-nils-sequence-to-list (sequence &rest other-sequences)
  (flatten-no-nils-list sequence other-sequences))
  
(defun flatten-no-nils-no-dups-list (sequence &rest other-sequences)
  (delete-duplicates (no-nils (flatten-list sequence other-sequences))))

(defun flatten-if-list (thing &rest other-things)
  (if (or other-things (consp thing))
      (flatten-list thing other-things)
      thing))

(defun flatten-if-list-greater-than-1 (thing &rest other-things)
  (if (or other-things (and (consp thing) (> (length thing) 1)))
      (flatten-list thing other-things)
      (if (consp thing) (car thing) thing)))

(defun atomize-delete-duplicates-flatten-no-nils-list (list)
  (atomize-list (delete-duplicates (flatten-no-nils-list list))))

(defun sequence-zerop (seq)
  (typecase seq
    (array (array-zerop seq))
    (cons (loop for val in (flatten-list seq) when (/= val 0) do (return nil) finally (return t)))))

(defun array-zerop (array)
  (loop for i from 0 to (1- (array-total-size array))
	when (/= 0 (row-major-aref array i)) do (return nil)
	finally (return t)))

(defun array-not-all-zerop (array)
  (not (array-zerop array)))

(defun df-zero-2d-array (array)
  (loop for x from 0 to (1- (car (array-dimensions array))) do
	(loop for y from 0 to (1- (cadr (array-dimensions array)))
	      do (setf (aref array x y) 0.0d0))))

(defun get-key-value-from-list (list target-key)
  (let ((signal nil))
    (loop for symbol in  list
	  when signal do (return symbol)
	  when (eq symbol target-key)
	  do (setq signal t))))

(defun pad-end (list desired-length)
  (let ((last-val (car (last list))))
    (append list (list-of-nums (- desired-length (length list)) last-val 0.0))))

(defun sequence-head (sequence &optional head-length)
  (typecase sequence
    (list (list-head sequence head-length))
    (array (array-head sequence head-length))))

(defun sequence-tail (sequence &optional tail-length)
  (typecase sequence
    (list (list-tail sequence tail-length))
    (array (array-tail sequence tail-length))))

(defun array-head (array &optional head-length)
  (if (and head-length (< head-length (length array)))
      (loop for count from 0 to (1- head-length)
	    collect (aref array count))
      array))

(defun list-head (list &optional head-length)
  (if head-length
      (loop for count from 1 to head-length
	    for val in list
	    collect val)
      list))

(defun array-tail (array &optional tail-length)
  (if tail-length
      (let* ((length (length array))
	     (start (- length tail-length)) )
	(loop for count from 0 to length
	      when (>= count start)
	      collect (aref array count)))
      array))

(defun list-tail (list &optional tail-length)
  (if tail-length
      (let* ((length (length list))
	     (start (- length tail-length)) )
	(loop for count from 0 to length
	      for val in list
	      when (>= count start)
	      collect val))
      list))


;; From a flat MOTHER list, return a list of NUMBER-OF-CHILDREN lists, each made up of the ordered
;; members of the original list, taken mod NUMBER-OF-CHILDREN. Thus, with NUMBER-OF-CHILDREN = 4,
;;
;;   '(a b c d e f g) -> '((a d) (b e) (c g) (d))
;;
(defun distribute-into-lists (mother number-of-children &optional return-as-values)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum number-of-children))
  (let ((result (loop for count fixnum from 1 to number-of-children collect '())))
    (loop for value in mother
	  for count fixnum from 0
	  do (push value (nth (mod count number-of-children) result))
	  finally (return (let ((dummy (loop for child in result collect (reverse child))))
			    (if return-as-values
				(values-list dummy) dummy))))))

;; Taken from Common Lisp by Paul Graham, page 49. (he calls this PROPER-LIST?, but the "?" screws up the user manual files).
(defun proper-list-p (x)
  (or (null x)
      (and (consp x)
	   (proper-list-p (cdr x)))))


;; *********************************
;; Sequence Conversions
;; *********************************

;; Consider VECTOR-TO-LIST* in /usr/src/cmucl/cmucl/src/code/seq.lisp
(defun array-to-list (seq)
  (typecase seq
    (array
     (loop for i from 0 to (1- (length seq))
	   collect (aref seq i)))
    (t seq)))

(defun array-to-float-list (seq)
  (typecase seq
    (array
     (loop for i from 0 to (1- (length seq))
	   collect (typecase (aref seq i)
		     (single-float (aref seq i))
		     (number (float (aref seq i)))
		     (t 0.0))))
    (t seq)))


(defun single-float-list-p (list)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (list list))
  (loop for val in list unless (typep val 'single-float) do (return nil) finally (return t)))

(defun float-list (list)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (list list))
  (if (single-float-list-p list)
      list
    (loop for val in list
	  collect (typecase val
			    (single-float val)
			    (number (float val))
			    (t 0.0)))))


(defun double-float-list (list)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (list list))
  (if (loop for val in list unless (typep val 'double-float) do (return t))
      (loop for val in list collect (d-flt val))
    list))

(defun 2darray-to-list (seq)
  (loop for j from 0 to 1 collect
	(loop for i from 0 to (1- (nth 1 (array-dimensions seq)))
	      collect (aref seq j i))))

(defun float-2dlist-to-array (seq)
  (let* ((dim-0 (length seq))
	 (dim-1 (length (car seq)))
	 (array (make-array (list dim-0 dim-1) :element-type 'single-float)))
    (loop for list in seq
	  for j from 0
	  do
	  (loop for val in list
		for i from 0
		do
		(setf (aref array j i) val)))
    array))

(defun double-float-2darray (array)
  (let* ((dim-0 (car (array-dimensions array)))
	 (dim-1 (cadr (array-dimensions array)))
	 (new-array (make-array (list dim-0 dim-1) :element-type `double-float)))
    (loop for i from 0 to (1- dim-0) do
	  (loop for j from 0 to (1- dim-1) do
		(setf (aref new-array i j) (coerce (aref array i j) 'double-float))))
    new-array))

(defun single-float-2darray (array)
  (let* ((dim-0 (car (array-dimensions array)))
	 (dim-1 (cadr (array-dimensions array)))
	 (new-array (make-array (list dim-0 dim-1) :element-type `single-float)))
    (loop for i from 0 to (1- dim-0) do
	  (loop for j from 0 to (1- dim-1) do
		(setf (aref new-array i j) (coerce (aref array i j) 'single-float))))
    new-array))

(defun 2d-array-max (array)
  (let ((max))
    (dotimes (x (array-dimension array 0))
      (dotimes (y (array-dimension array 1))
	(setq max (max (or max (aref array x y)) (aref array x y)))))
    max))

(defun 2d-array-min (array)
  (let ((min))
    (dotimes (x (array-dimension array 0))
      (dotimes (y (array-dimension array 1))
	(setq min (min (or min (aref array x y)) (aref array x y)))))
    min))


(defun normalize-2darray (array &optional invert (dynamic-range 100) (base-offset 100))
  (let* ((max (2d-array-max array))
	 (min (2d-array-min array))
	 (amp (- max min)))
    (loop for i from 0 to (1- (car (array-dimensions array))) do
	  (loop for j from 0 to (1- (cadr (array-dimensions array))) do
		(setf (aref array i j)
		      (let ((val (* (/ dynamic-range 100)
				    (+ (/ base-offset 100) (/ (- (aref array i j) min) amp)))))
			(if invert (- 1 val) val)))))
    array))
  
(defun 2dseq-to-array (seq)
  (let* ((dim-0 (length seq))
	 (dim-1 (length (car seq)))
	 (array (make-array (list dim-0 dim-1))))
    (typecase seq
      (array)
      (cons
       (loop for list in seq
	     for j from 0
	     do
	     (loop for val in list
		   for i from 0
		   do
		   (setf (aref array j i) val)))
       array))))

(defun make-2d-histo-array (x-data y-data x-incs y-incs
				   &key
				   print-out-max-mins
				   x-min x-max y-min y-max
				   (increment 1) (base-offset 0))
  (let* ((density-array (make-array (list x-incs y-incs) :element-type 'single-float))
	 (max-x-indice (1- x-incs))
	 (max-y-indice (1- y-incs))
	 (max-x (or x-max (max-of-list x-data)))
	 (min-x (or x-min (min-of-list x-data)))
	 (max-y (or y-max (max-of-list y-data)))
	 (min-y (or y-min (min-of-list y-data))))
    (loop for x in x-data
	  for y in y-data
	  do (let ((x-indice
		    (floor (* (+ 1 max-x-indice)
			      (/ (- x min-x)
				 (- max-x min-x)))))
		   (y-indice
		    (- max-y-indice
		       (floor (* (+ 1 max-y-indice)  (/ (- y min-y)
							(- max-y min-y)))))))
	       (when (and (<= 0 x-indice max-x-indice) (<= 0 y-indice max-y-indice))
		 (when (= (aref density-array x-indice y-indice) 0)
		   (setf (aref density-array x-indice y-indice) (s-flt base-offset)))
		 (setf (aref density-array x-indice y-indice)
		       (+ (s-flt increment) (aref density-array x-indice y-indice))))))
    (when print-out-max-mins (format t "Maximum/Minimum Values: ~A/~A~%"
				     (2d-array-max density-array)
				     (2d-array-min density-array)))
;    (break)
    density-array))  

(defun sequence-to-float-array (sequence)
  (typecase sequence
    ((array single-float *) sequence) 
    (t (list-to-array
	(typecase sequence
	  (array (loop for val across sequence collect (coerce val 'single-float)))
	  (list (loop for val in sequence collect (coerce val 'single-float))))))))

(defun sequence-to-double-float-array (sequence)
  (list-to-array-double
   (typecase sequence
     (array (loop for val across sequence collect (coerce val 'double-float)))
     (list (loop for val in sequence  collect (coerce val 'double-float))))))

(defun sequence-to-list (sequence)
  (typecase sequence
    (list sequence)
    (array (array-to-list sequence))))

(defun sequence-to-float-list (sequence)
  (typecase sequence
    (list (float-list sequence))
    (array (array-to-float-list sequence))))

(defun sequence-to-float-list-w-offset (wave dc-offset)
  (let ((list (sequence-to-float-list wave)))
    (if (= dc-offset 0)
	list
	(loop for val single-float in list
	      collect (- val (the sf dc-offset))))))
    
(defun sequence-to-gen-array (sequence)
  (list-to-array-generic
   (typecase sequence
     (array (loop for val across sequence collect val))
     (list (loop for val in sequence  collect val)))))

(defun list-to-array (list)
  (declare (list list))
  (when (and list (> (length list) 0))
    (let ((array (make-array (length list) :element-type 'single-float))
	  (i 0))
      (declare (fixnum i))
      (dolist  (element list)
	(setf (aref array i) (float element))
	(setq i (+ 1 i)))
      array)))

(defun list-to-array-double (list)
  (declare (list list))
  (when (and list (> (length list) 0))
    (let ((array (make-array (length list) :element-type 'double-float))
	  (i 0))
      (declare (fixnum i))
      (dolist  (element list)
	(setf (aref array i) (coerce element 'double-float))
	(setq i (+ 1 i)))
      array)))

(defun list-to-array-fix (list)
  (declare (list list))
  (when (and list (> (length list) 0))
    (let ((array (make-array (length list) :element-type 'fixnum))
	  (i 0))
      (declare (fixnum i))
      (dolist  (element list)
	(setf (aref array i) element)
	(setq i (+ 1 i)))
      array)))

(defun list-to-array-generic (list)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (list list))
  (when list
    (let ((length (length list)))
      (declare (fixnum length))
      (when (> length 0)
	(let ((array (make-array length))
	      (i 0))
	  (declare (fixnum i))
	  (dolist (element list)
	    (setf (aref array i) element)
	    (setq i (+ 1 i)))
	  array)))))




#|
********************************************************
********************************************************

In article <44chch$g58@newsbf02.news.aol.com>,
JScheller <jscheller@aol.com> wrote:
>How does one copy the values in an array to a new, independent array?

If it's a one-dimensional array (i.e. a vector), you can use COPY-SEQ.

Otherwise, you have to write a function that copies element-by-element.
Here's a function that should do it (it's untested):

|#
(defun copy-array (old-array &optional new-array)
  (let* ((rank (array-rank old-array))
	 (index-list (make-list rank :initial-element 0))
         (dimensions (apply #'vector (array-dimensions old-array))))
    (unless new-array
      (setq new-array
	    (make-array dimensions
			:element-type (array-element-type old-array)
			; :adjustable (array-adjustable-p old-array)
			:fill-pointer (and (array-has-fill-pointer-p old-array)
					   (fill-pointer old-array)))))
    (loop
      ;; copy an element
      (setf (apply #'aref new-array index-list)
            (apply #'aref old-array index-list))
      ;; Increment the index
      (let (temp
	    (index-pos (1- rank))) ; start with last dimension
        (loop
	  (when (minusp index-pos)
            (return-from copy-array new-array))
	  (setq temp (incf (nth index-pos index-list)))
          (if (>= temp (svref dimensions index-pos))
              ;; carry into higher dimension
	      (progn
		(setf (nth index-pos index-list) 0)
		(decf index-pos))
              ;; return from the increment loop
              (return nil)))))))
#|
-- 
Barry Margolin
BBN PlaNET Corporation, Cambridge, MA
barmar@bbnplanet.com
Phone (617) 873-3126 - Fax (617) 873-6351
********************************************************
********************************************************
|#

#|
(defun copy-array (array)
   (adjust-array (make-array (array-dimensions array)
                             :displaced-to array
                             :element-type (array-element-type array))
                 (array-dimensions array)
                 :displaced-to nil))
|#



#|
   From: Erik Naggum <erik@naggum.no>
   Newsgroups: comp.lang.lisp
   Date: 06 Feb 1996 16:27:30 +0000
   Organization: Naggum Software; +47 2295 0313
|#


(defun insert-after (newelt list index)
  "Insert NEWELT in LIST after the INDEXth cell.
   Returns LIST."
  (let ((cell (nthcdr index list)))
    (setf (cdr cell) (cons newelt (cdr cell))))
  list)

#|
Beautiful solution that can be rewritten to an even more concise form:
|#

(defun insert-after (newelt list index)
  "Insert NEWELT in LIST after the INDEXth cell.
  Returns LIST."
  (push newelt (cdr (nthcdr index list)))
  list)

#|
-- 
--------------------------------------------------------------------------
Bernhard Pfahringer
Austrian Research Institute for  http://www.ai.univie.ac.at/~bernhard/
Artificial Intelligence          bernhard@ai.univie.ac.at 
|#


(export '(insert-after
	  SINGLE-FLOAT-LIST-P
	  DISTRIBUTE-INTO-LISTS
	  proper-list-p
	  REDUCE-REPEATS
	  NUMBER-SEQUENCE-P
	  atomize-list
	  add-to-end
	  coerce-to-list
	  FLOAT-2DLIST-TO-ARRAY

	  hash-table-list
	  print-table-keys
	  hash-table-empty
	  printit
	  named-structure-symbol

	  FIRST-ELEMENT LAST-ELEMENT
	  my-delete
	  FAST-SORT-DELETE-DUPLICATES
	  ARRAY-ZEROP SEQUENCE-ZEROP
	  array-not-all-zerop get-key-value-from-list
	  
	  push-lists-onto-list chop-list
	  every-nth
	  order-list-from-key-list
	  FLATTEN-LIST
	  flatten-no-nils-list
	  atomize-delete-duplicates-flatten-no-nils-list
	  null-list-p
	  remove-components-in-list

	  sum-double-float-listx*listy
	  sum-double-float-listx*listx
	  sum-float-list
	  sum-double-float-list

	  
	  split-up
	  no-nils
	  COMPLEMENT-LIST

	  array-to-float-list
	  array-to-list
	  2darray-to-list
	  sequence-to-float-array
	  sequence-to-gen-array
	  list-to-array
	  list-to-array-generic
	  LIST-TO-ARRAY-FIX
	  SEQUENCE-TO-LIST
	  SEQUENCE-TO-float-LIST
	  SEQUENCE-TO-FLOAT-LIST-W-OFFSET
	  float-list
	  double-float-list
	  CLEAN-UP-LIST
	  copy-array

	  LIST-TO-ARRAY-DOUBLE
	  SEQUENCE-TO-DOUBLE-FLOAT-ARRAY
	  2DSEQ-TO-ARRAY
	  DOUBLE-FLOAT-2DARRAY
	  single-float-2darray

	  flatten
	  FLATTEN-IF-LIST
	  flatten-if-list-greater-than-1
	  flatten-no-nils-no-dups-list
	  FLATTEN-NO-NILS-SEQUENCE-TO-LIST
	  FLATTEN-SEQUENCE-TO-LIST
	  list-of-nums
	  list-of-sf-nums
	  array-of-nums
	  DF-ZERO-2D-ARRAY
	  THING-IN-ARRAY-P
	  2D-ARRAY-MAX
	  2D-ARRAY-Min
	  NORMALIZE-2DARRAY
	  make-2d-histo-array

	  generic-intersection
	  LIST-HEAD
	  SEQUENCE-HEAD
	  ARRAY-HEAD
	  ARRAY-TAIL SEQUENCE-TAIL

	  PAD-END
	  SEQUENCEP
	  LIST-TAIL
	  normalize-sequence
	  ADD-VAL-TO-FLOAT-LIST
	  SCALE-FLOAT-LIST
	  DOUBLE-FLOAT-IN-LIST-P
	  DOUBLE-LISTS
	  MAXIMIZE-VAL-OR-SEQ MINIMIZE-VAL-OR-SEQ A-BIT-MORE A-BIT-LESS
	  remove-all
	  ))  
