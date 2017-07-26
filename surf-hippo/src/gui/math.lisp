;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GUI Source file: math.lisp


(IN-PACKAGE "WINDOWS-HACK")


;; Some math functions. Check the macros.lisp file for some math macros, and sequences.lisp for some
;; sequence-oriented math functions.

(defun s-flt-and-copy-list (list)
  ;; Returns a copy of a LIST of numbers with all values coerced to single-floats.
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (loop for val in list collect (s-flt val)))

(defun single-float-<-merge-lists* (list-1 list-2 &optional key)
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (do* ((result (list :header))
	(P result))			; P points to last cell of result
       ((or (null list-1) (null list-2)) ; done when either list used up	
	(if (null list-1)		; in which case, append the
	    (rplacd p list-2)		;   other list
	    (rplacd p list-1))
	(do ((drag p lead)
	     (lead (cdr p) (cdr lead)))
	    ((null lead)
	     (values (prog1 (cdr result) ; return the result sans header
		       (rplacd result nil)) ; (free memory, be careful)
		     drag))))		; and return pointer to last element
    (cond ((let ((one (car list-2))
		 (two (car list-1)))
	     (if key
		 (< (the sf (funcall (the compiled-function key) one))
		    (the sf (funcall (the compiled-function key) two)))
		 (< (the sf one) (the sf two))))
	   (rplacd p list-2)		; append the lesser list to last cell of
	   (setq p (cdr p))		;   result.  Note: test must bo done for
	   (pop list-2))		;   list-2 < list-1 so merge will be
	  (T (rplacd p list-1)		;   stable for list-1
	     (setq p (cdr p))
	     (pop list-1)))))

(defun single-float-ascending-sort-list (list &optional key)
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (let ((head (cons :header list))	; head holds on to everything
	(n 1)				; bottom-up size of lists to be merged
	unsorted			; unsorted is the remaining list to be
					;   broken into n size lists and merged
	list-1				; list-1 is one length n list to be merged
	last)				; last points to the last visited cell
    (declare (fixnum n))
    (loop
     ;; start collecting runs of n at the first element
     (setf unsorted (cdr head))
     ;; tack on the first merge of two n-runs to the head holder
     (setf last head)
     (let ((n-1 (1- n)))
       (declare (fixnum n-1))
       (loop
	(setf list-1 unsorted)
	(let ((temp (nthcdr n-1 list-1))
	      list-2)
	  (cond (temp
		 ;; there are enough elements for a second run
		 (setf list-2 (cdr temp))
		 (setf (cdr temp) nil)
		 (setf temp (nthcdr n-1 list-2))
		 (cond (temp
			(setf unsorted (cdr temp))
			(setf (cdr temp) nil))
		       ;; the second run goes off the end of the list
		       (t (setf unsorted nil)))
		 (multiple-value-bind (merged-head merged-last)
		     (single-float-<-merge-lists* list-1 list-2 key)
		   (setf (cdr last) merged-head)
		   (setf last merged-last))
		 (if (null unsorted) (return)))
		;; if there is only one run, then tack it on to the end
		(t (setf (cdr last) list-1)
		   (return)))))
       (setf n (ash n 1))		; (+ n n)
       ;; If the inner loop only executed once, then there were only enough
       ;; elements for two runs given n, so all the elements have been merged
       ;; into one list.  This may waste one outer iteration to realize.
       (if (eq list-1 (cdr head))
	   (return list-1))))))

(defun closest-n-1-vals (list)
  "Takes a LIST of N numbers and returns a sorted list of the N-1 closest values."
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (multiple-value-bind (median sorted-list)
      (median list)
    (declare (single-float median)
	     (cons sorted-list))
    (if (> (- median (the sf (car sorted-list)))
	   (- (the sf (car (last sorted-list))) median))
	(cdr sorted-list)
	(butlast sorted-list))))

(defun average-closest-n-1-vals (list)
  "Takes a LIST of N single-float numbers and returns the single float average of the N-1 closest values."
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (loop for val single-float in list
	maximizing val into max single-float
	minimizing val into min single-float
	summing val into total single-float
	finally (return (let* ((length (length (the cons list)))
			       (average (/ total length)))
			  (/ (if (> (- average min) (- max average))
				 (- total min)
				 (- total max))
			     (1- length))))))

(defun in-middle-kludge (x-min x x-max)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float x))
  (cond ((and x-min x-max) (<= (the sf x-min) x (the sf x-max)))
	(x-min (<= (the sf x-min) x))
	(x-max (<= x (the sf x-max)))))

(defun get-nice-mag (number &optional mag (decimals 2))
  (cond ((= number 0.0) 0.0)
	((and (numberp mag) (= mag 0)) number)
	(t
	 (let* ((mag (or mag number))
		(alpha (- (round (log (abs mag) 10)) decimals)))
	   (* (round number (expt 10.0 alpha))
	      (expt 10.0 alpha))))))

(defun msd-round (num)
  (if (= num 0) num
      (let ((factor (expt 10 (floor (log (abs num) 10)))))
	(float (* factor (round num factor))))))

(defun coerce-to-even-int (num)
  (* 2 (round (/ num 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cos-degrees (degrees)
  (cos (* 2.0 pi-single (/ degrees 360.0))))

(defun sin-degrees (degrees)
  (sin (* 2.0 pi-single (/ degrees 360.0))))

(defun rad-to-deg (angle-in-radians)
 (let ((angle  (* (/  360  (* 2 pi)) angle-in-radians)))
   (if (< angle 0) (+ 360 angle) angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline square))
(defun square (x)
 (declare (single-float x))
 (* x x))

(defun df-square (x)
 (declare (double-float x))
 (* x x))

(defun cartesian-distance (x1 y1 x2 y2)
  (sqrt (+ (square (float (- x1 x2)))
	   (square (float (- y1 y2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BOUND-VAL, BOUND-INT-VAL Returns VAL after passing it through piece-wise linear squashing function defined by
;;; MAX and MIN.
(proclaim '(inline bound-int-val))
(proclaim '(inline bound-val))
(defun bound-int-val (val max min)
  (declare (type fixnum val max min))
  (min (max val min) max))

(defun bound-val (val max min)
  (min (max val min) max))


;;;;;;;;;;;;;;;
;; Array
;;;;;;;;;;;;;;;

(defun 3-by-3-identity ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((array (make-array '(3 3) :initial-element 0.0 :element-type 'single-float)))
    (declare (type (simple-array single-float (3 3)) array))
    (dotimes (i 3)	
      (declare (fixnum i))
      (setf (aref array i i) 1.0))
    array))


;;;;;;;;;;;;;;;
;; Logical
;;;;;;;;;;;;;;;

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun true-p (thing)
  (not (null thing)))

(defun negative-p (num)
  (< num 0))

(defun positive-p (num)
  (> num 0))

(defun non-negative-p (num)
  (>= num 0))

(defun non-positive-p (num)
  (<= num 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linear Regression Code
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lxx (list)
  ;; Return the population variance of the number list.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((df-values (double-float-in-list-p list))
	 (df-list (d-flt-gen list))
	 (n (length (the cons list)))
	 (sumx (sum-double-float-list df-list))
	 (sumxx (sum-double-float-listx*listx df-list))
	 (lxx (/ (- sumxx (/ (* sumx sumx) n)) n)))
    (if df-values lxx (s-flt lxx))))

(defun std-dev (list)
  (sqrt (lxx list)))

 
(defun lxy (listx listy)
  ;; Return the covariance of the number lists.
  (let* ((df-values (loop for list in (list listx listy) when (double-float-in-list-p list) do (return t)))
	 (df-listx (d-flt-gen listx))
	 (df-listy (d-flt-gen listy))
	 (n (length (the cons listx)))
	 (sumx (sum-double-float-list df-listx))
	 (sumy (sum-double-float-list df-listy))
	 (sumxy (sum-double-float-listx*listy df-listx df-listy))
	 (lxy (/ (- sumxy (/ (* sumx sumy) n)) n)))
    (if df-values lxy (s-flt lxy))))

(defun r (listx listy)
  "Return the correlation coefficient of the number lists."
  (/ (lxy listx listy) (sqrt (* (lxx listx) (lxx listy)))))


(defun lin-reg (xylists &optional print-results) ; list-of-lists = ((x_list)(y_list))
  "Calculates linear regression values for XYLISTS, which has the format ((X-LIST) (Y-LIST)).  Internally calculations are done in double precision,
but returned number types are either single or double float depending on the types of the arguments. When PRINT-RESULTS is T then linear regression
info is printed to the standard output. Returns values as (SLOPE INTERCEPT CORRELATION-COEFF). SLOPE may be a number, :UNDEFINED or :INFINITE. The
INTERCEPT [on the X axis] may be a number or :UNDEFINED."
  (lin-reg-float-core xylists print-results))

#|  
(defun lin-reg-float (list-of-lists &optional print-results) ; list-of-lists = ((x_list)(y_list))
  (multiple-value-bind (slope intercept r)
      (lin-reg-float-core list-of-lists print-results)
    (list slope intercept r)))

(defun lin-reg-float-values (list-of-lists) ; list-of-lists = ((x_list)(y_list))
  (lin-reg-float-core list-of-lists))
|#


(defun mean (list)
  "Returns single-float mean of LIST."
  (float (/ (apply '+ list) (length list))))

(defun median (list)
  "Returns the single-float median of LIST - when the length of LIST is even, then the median is the
average of the values flanking the true median. Also returns a copy of LIST, converted to single
floats and sorted."
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((length (length (the cons list)))
	 (length/2 (/ length 2))
	 (sorted-list (single-float-ascending-sort-list (s-flt-and-copy-list list))))
    (values (if (evenp length)
	      (/ (the sf (+ (the sf (nth (1- (the fn length/2)) sorted-list))
			    (the sf (nth (the fn length/2) sorted-list)))) 2)
	      (the sf(nth (if (= length 3) 1 (round length/2)) sorted-list)))
	    sorted-list)))


(defun lin-reg-float-core (list-of-lists &optional print-results debug) ; list-of-lists = ((x_list)(y_list))
					;  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((listx (car list-of-lists))
	 (listy (cadr list-of-lists))
	 (n (length (the cons listx)))
	 (df-values (loop for list in (list listx listy) when (double-float-in-list-p list) do (return t)))

	 (df-listx (d-flt-gen listx))
	 (df-listy (d-flt-gen listy))
	 (equal-x-values (apply '= df-listx))
	 (equal-y-values (apply '= df-listy))
	 (sumx (sum-double-float-list df-listx))
	 (sumy (sum-double-float-list df-listy))
	 (sumxy (sum-double-float-listx*listy df-listx df-listy))
	 (sumxx (sum-double-float-listx*listx df-listx))
	 (sumyy (sum-double-float-listx*listx df-listy))

	 (slope 0.0d0)
	 (intercept 0.0d0)
	 (meanx 0.0d0)
	 (meany 0.0d0)
	 (lxy 0.0d0)
	 (lxx 0.0d0)
	 (lyy 0.0d0)
	 (r 0.0d0)
	 (slope-num (- (* n sumxy) (* sumy sumx)))
	 (slope-den (- (* n sumxx) (* sumx sumx))))
    (declare (fixnum n) (double-float sumx sumy sumxy sumxx ; slope intercept
				      meanx meany slope-num slope-den))
    (when debug (format t "n: ~A sumx: ~A sumy: ~A sumxy: ~a sumxx: ~A num: ~A den: ~A~%" n sumx sumy sumxy sumxx slope-num slope-den))
    (setq slope (cond
		 ((or (and equal-x-values equal-y-values)
		      (= 0 slope-den slope-num))
		  :undefined)
		 ((or equal-x-values (= 0 slope-den))
		  :infinity)
		 (equal-y-values 0.0d0)
		 (t (/ slope-num slope-den))))
    (setq intercept
	  (case slope
	    (:undefined :undefined)
	    (:infinity (car df-listx))
	    (t (/ (- sumy (* slope sumx)) n))))
    (setq meanx (/ sumx n)
	  meany (/ sumy n))
    (setq lxy (- sumxy (/ (* sumx sumy) n))
	  lxx (- sumxx (/ (* sumx sumx) n))
	  lyy (- sumyy (/ (* sumy sumy) n)))
    (when debug (format t "lxy: ~A lxx: ~A lyy: ~a~%" lxy lxx lyy))
    (setq r (if (or (= lxy lyy 0) (= lxy lxx 0))
	      0.0d0
	      (if (= 0 (* lxx lyy))
		1.0d0
		(/ lxy (sqrt (* lxx lyy))))))
    (when print-results
      (format t "~%mx=~a, varx=~a, my=~a, vary=~a, covxy=~a~%" meanx (lxx listx) meany (lxx listy) (lxy listx listy))
      (format t "Slp=~a, Intcpt= ~a, r = ~a ~%" slope intercept r))
    (values (if (numberp slope) (if df-values slope (s-flt slope)) slope)
	    (if (numberp intercept) (if df-values intercept (s-flt intercept)) intercept)
	    (if df-values r (s-flt r)))))


(defun integrate-x-y (y-list x-list &key x-max x-min (y-base 0.0) average)
  ;; Lists must contain single floats. X-LIST may be a list of numbers or a single delta-x value.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((x-list (if (consp x-list) x-list (list-of-nums (length y-list) 0.0 x-list))))
    (when y-list
      (when x-max (setq x-max (s-flt x-max)))
      (when x-min (setq x-min (s-flt x-min)))
      (setq y-base (s-flt y-base))
      (let ((sum 0.0))
	(do* ((x-n (car x-list) x-n-1)
	      (x-n-1 (cadr x-list) (car x-list))
	      (x-list (cddr x-list) (cdr x-list))
	      (y-n (car y-list) y-n-1)
	      (y-n-1 (cadr y-list) (car y-list))
	      (y-list (cddr y-list) (cdr y-list)))
	     ((null x-n-1) sum)
	  (declare (single-float y-base sum))
	  (setq sum (+ sum (if (and (or (not x-max) (> (the sf x-max) (the sf x-n)))
				    (or (not x-min) (< (the sf x-min) (the sf x-n))))
			       (* (abs (- (the sf x-n) (the sf x-n-1))) (- (/ (+ (the sf y-n) (the sf y-n-1)) 2.0) y-base))
			       0.0))))
	(if average (/ sum (abs (- (the sf (first x-list)) (the sf (car (last x-list)))))) sum)))))


(export '(TRUE-P negative-p positive-p non-negative-p non-positive-p
	  s-flt-and-copy-list
	  single-float-<-merge-lists*
	  single-float-ascending-sort-list
	  average-closest-n-1-vals
	  closest-n-1-vals	  
	  in-middle-kludge
	  GET-NICE-MAG
	  msd-round
	  max-of-list min-of-list
	  MAX-OF-SEQ
	  MIN-OF-SEQ
	  cos-degrees sin-degrees
	  coerce-to-even-int
	  rad-to-deg
	  xor
	  square
	  df-square
	  cartesian-distance
	  bound-val
	  bound-int-val
	  3-by-3-identity

	  	  lxx
	  STD-DEV
	  lxy
	  r
	  mean median
	  lin-reg
;	  LIN-REG-FLOAT
;	  LIN-REG-FLOAT-VALUES
	  LIN-REG-FLOAT-CORE
	  integrate-x-y
	  ))
