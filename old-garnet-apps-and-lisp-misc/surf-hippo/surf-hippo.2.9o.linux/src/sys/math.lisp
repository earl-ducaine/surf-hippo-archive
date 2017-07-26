;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


;;; SYS Source file: math.lisp
(in-package "SURF-HIPPO")

;; Some miscellaneous math functions - there may be others scattered through the source files (e.g.
;; misc.lisp, analysis.lisp, and others. also see gui/math.lisp).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometric stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sphere-area (radius-microns)
  "Sphere surface area is in square microns - RADIUS-MICRONS is in micrometers."  
  (* 4.0 pi-single radius-microns radius-microns))

(defun sphere-area-cm2 (radius-microns)
  "Sphere surface area is in square cm - RADIUS-MICRONS is in micrometers."  
  (* 1.0e-8 (sphere-area radius-microns)))

(defun sphere-area-from-diameter (diameter)
  "Sphere surface area is in square cm - DIAMETER is in micrometers."
  (let ((radius (/ diameter 2)))
    (* 4.0 pi-single radius radius 1.0e-8)))

(defun sphere-diameter-from-area (area)
  "Returns the diameter in microns of a sphere with AREA in sq microns."
  (sqrt (/ area pi-single)))

(proclaim '(inline sphere-volume))
(defun sphere-volume (radius)
  "Returns volume in um3 of sphere with RADIUS (single float) in um (single float)."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float radius))
  (* (/ 4 3.0) pi-single radius radius radius))

(proclaim '(inline sphere-volume-from-diameter))
(defun sphere-volume-from-diameter (diameter)
  "Returns volume in um3 of sphere with DIAMETER (single float) in um (single float)."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float diameter))
  (sphere-volume (/ diameter 2)))

(proclaim '(inline cylinder-volume))
(defun cylinder-volume (length diameter)
  "Returns volume in um3 of cylinder with LENGTH and DIAMETER (both single floats) in um (single float)."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float length diameter))
  (let ((radius (/ diameter 2)))
    (* pi-single radius radius length)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(proclaim '(inline fast-fractional-part))
(defun fast-fractional-part (number)
  "Returns second result of TRUNCATE, where NUMBER is double float."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (multiple-value-bind (tru rem) (truncate (the df number) 1.0)
    (declare (ignore tru))
    rem))

(defun single-float-signum (num)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float num))
  (cond ((= 0 num) 0.0)
	((> num 0) 1.0)
	(t -1.0)))
    
;; ************* ************* ************* *************
;;
;;   Angle Related Functions
;;
;; ************* ************* ************* *************

(defun rad-to-deg (angle-in-radians)
  (let ((angle  (* (/  360  (* 2 pi-single)) angle-in-radians)))
    (if (< angle 0) (+ 360 angle) angle)))

(defun deg-to-rad (angle-in-degrees)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (the sf (* angle-in-degrees
	     (the sf (* 2.0 pi-single))
	     (the sf (/ 1.0 360)))))

(defun cos-degrees (degrees)
  (cos (* 2.0 pi-single (/ degrees 360.0))))

(defun sin-degrees (degrees)
  (sin (* 2.0 pi-single (/ degrees 360.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline df-real-log))
(defun df-real-log (number base)
  (declare (optimize (speed 3) (space 0))
	   (double-float number base))
  "Return the logarithm of df NUMBER in the df base BASE"
  (the df (/ (the df (kernel::%log number))
	     (the df (kernel::%log base)))))

(defun df-real-ln (number)
  (declare (optimize (speed 3) (space 0))
	   (double-float number))
  "Return the natural logarithm of df NUMBER"
  (the df (kernel::%log number)))

(defun log-10 (number)
  (log number 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline float-mod))
(defun float-mod (number divisor)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float number divisor))
  "Returns second result of FLOOR."
  (let ((rem (the sf (rem number divisor))))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
	rem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gaussian (x mean variance)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x mean variance))
  (without-floating-underflow-traps
   (* (/ 1.0 (sqrt (* 2.0 3.14156 variance)))
      (exp (- (float (/ (* (- x mean) (- x mean)) (* 2.0 variance))))))))

(defun gaussian (x mean variance)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x mean variance))
  (without-floating-underflow-traps
   (let ((x-shifted (- x mean)))
     (* (/ 1.0 (the sf (sqrt (* (+ pi-single pi-single) variance))))
	(exp (- (/ (* x-shifted x-shifted) (+ variance variance))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(proclaim '(inline nonlinearity))
(defun nonlinearity (input &optional nonlinearity (parameter 0.0))
  "Pass the single float INPUT through a nonlinearity and return the single float result.

    :NONLINEARITY           :PARAMETER               COMMENT

        NIL                    n/a                   linear
    :THRESHOLD              Threshold    (if INPUT >= threshold then INPUT, else 0.0)
    :NEGATE-THRESHOLD       Threshold    (if INPUT >= threshold then (- INPUT), else 0.0)
    :BELOW-THRESHOLD        Threshold    (if INPUT <= threshold then INPUT, else 0.0)
    :NEGATE-BELOW-THRESHOLD Threshold    (if INPUT <= threshold then (- INPUT), else 0.0)
    :RECTIFY                   n/a          full wave, i.e. x>0 -> x, x<0 -> -x 
"
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float input parameter))
  (case nonlinearity
    ((threshold :threshold)
     (the sf (if (>= input parameter) input 0.0)))
    (:negate-threshold
     (the sf (if (>= input parameter) (- input) 0.0)))
    (:negate-below-threshold
     (the sf (if (<= input parameter) (- input) 0.0)))
    (:below-threshold
     (the sf (if (<= input parameter) input 0.0)))
    (:rectify
     (the sf (abs input)))
    (t input)))



(proclaim '(inline single-float-min))
(defun single-float-min (number1 number2)
  (declare (optimize (safety 0) (speed 3) (space 0)) 
	   (single-float number1 number2))
  (if (< number1 number2) number1 number2))

(proclaim '(inline single-float-max))
(defun single-float-max (number1 number2)
  (declare (optimize (safety 0) (speed 3) (space 0)) 
	   (single-float number1 number2))
  (if (> number1 number2) number1 number2))


(proclaim '(inline double-float-min))
(defun double-float-min (number1 number2)
  (declare (optimize (safety 0) (speed 3) (space 0)) 
	   (double-float number1 number2))
  (if (< number1 number2) number1 number2))

(proclaim '(inline double-float-max))
(defun double-float-max (number1 number2)
  (declare (optimize (safety 0) (speed 3) (space 0)) 
	   (double-float number1 number2))
  (if (> number1 number2) number1 number2))

  
;;; These are for avoiding over or underflows in calls to exp.
(defconstant exp-upper-arg (float (floor (log most-positive-single-float))))
(defconstant exp-lower-arg -30.0)	; empirical
(proclaim '(single-float exp-upper-arg exp-lower-arg))

(defconstant exp-upper-arg-double (float (floor (log most-positive-double-float)) 0.0d0))
(defconstant exp-lower-arg-double -300.0d0) ; empirical
(proclaim '(double-float exp-upper-arg-double exp-lower-arg-double))

(defconstant exp-upper (exp exp-upper-arg))
(defconstant exp-lower (exp exp-lower-arg))

(defconstant exp-upper-double (exp exp-upper-arg-double))
(defconstant exp-lower-double (exp exp-lower-arg-double))

(defvar *notify-exp-limit* nil "When T print out message when one of EXP-W-LIMITS functions punts.")

(proclaim '(inline notify-exp-limit))
(defun notify-exp-limit (arg)
  (when *notify-exp-limit* (format *error-output* "EXP-W-LIMITS punted with ~A~%" arg)))

(proclaim '(inline exp-w-limits))
(defun exp-w-limits (arg)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float arg))
  (cond ((> arg exp-upper-arg) (notify-exp-limit arg) EXP-UPPER)
	((> arg exp-lower-arg) (exp arg))
	(t (notify-exp-limit arg) EXP-lower)))

(defun exp-w-limits-generic (arg)
  (cond ((> arg exp-upper-arg) (notify-exp-limit arg) EXP-UPPER)
	((> arg exp-lower-arg) (exp arg))
	(t (notify-exp-limit arg) EXP-lower)))

(proclaim '(inline exp-w-limits-double))
(defun exp-w-limits-double (arg)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float arg))
  (cond ((> arg exp-upper-arg-double) (notify-exp-limit arg) EXP-UPPER-DOUBLE)
	((> arg exp-lower-arg-double) (kernel::%exp arg))
	(t (notify-exp-limit arg) EXP-lower-DOUBLE)))
	

(defun in-btwn (lower num upper)
  (and (<= lower num) (< num upper)))

(defun my-xor (a b)
  (if a
      (if b nil t)
      (if b t nil)))

(proclaim '(inline rectify))
(defun rectify (number)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float number))
  (max number 0.0))
    
;;CLOSE-TO Returns T if the arguments are equal at the level of "resolution".
(defun close-to (first-float second-float &optional (resolution 0.001))
    (declare (optimize (safety 0) (speed 3) (space 1)))
  (= (round (/ (the sf first-float) (the sf resolution)))
     (round (/ (the sf second-float)(the sf resolution)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3x3 array routines. Used in histology graphics transforms
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline clear-3-by-3-array fill-3-by-3-array copy-3-by-3-array abtoc))
(defun clear-3-by-3-array (array)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) array))
  (dotimes (i 3)	
    (declare (fixnum i))
    (dotimes (j 3)	
      (declare (fixnum j))
      (setf (aref array i j) 0.0))))

(defun fill-3-by-3-identity (array)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) array))
  (clear-3-by-3-array array)
  (dotimes (i 3)	
    (declare (fixnum i))
    (setf (aref array i i) 1.0))
  array)

(defun copy-3-by-3-array (from-array to-array)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) from-array to-array))
  (dotimes (i 3)	
    (declare (fixnum i))
    (dotimes (j 3)	
      (declare (fixnum j))
      (setf (aref to-array i j) 
	    (aref from-array i j)))))

;; ABTOC Perform A*B->C on these three 3X3 matrices.
#|
(defun abtoc (a b c)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) a b c))
  (dotimes (i 3)	
    (declare (fixnum i))
    (dotimes (j 3)	
      (declare (fixnum j))
      (setf (aref (the (simple-array single-float (3 3)) c) i j)
	    (the sf
		 (let ((sum 0.0))
		   (declare (single-float sum))
		   (dotimes (k 3)
		     (declare (fixnum k))
		     (setq sum (the sf (+ sum (the sf (* (the sf (aref a i k))
							 (the sf (aref b k j))))))))
		   sum))))))
|#

(defun abtoc (a b c)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) a b c))
  (loop for i fixnum from 0 to 2 do
	(loop for j fixnum from 0 to 2 do
	      (setf (aref c i j)
		    (loop for k fixnum from 0 to 2 sum (* (aref a i k) (aref b k j)) into result single-float
			  finally (return result))))))

		 
;;;;;;;;;;;;;;;;;;;;;;


(defun permute (l)
    (if (null l) ; I don't like this. I suspect that there is a more
	'(nil)   ; natural way to make the function return '(nil) for nil.
      (mapcan #'(lambda (e-arg)
		  (mapcar #'(lambda (p) (cons e-arg p))
			  (permute (remove e-arg l))))
	      l)))

#|
Geert-Jan
-- 
Geert-Jan van Opdorp
AI-Engineering
Amsterdam, The Netherlands
geert@aie.nl

|#

(defun dolist-permute (input)
  (when input
    (let ((result (list input)))
      (when (> (length input) 1)
	(dolist (head input result)
	  (nconc result
		 (mapcar #'(lambda (list)
			     (cons head list))
			 (dolist-permute (remove head input :count 1))))))
      (remove-duplicates result :test #'equal))))

#|
Strictly speaking, I suppose you do not need to provide for the possibility of
the same element occurring more than once (:count 1 and remove-duplicates), 
since, if I remember correctly, for a permutation you assume that all elements
are distinct. Still, it seems a small price to pay for adding generality.

Cheers,

Yo

-- 
------------------------------------------------------------------------------
          _
          V                    Johan Peeters
 -------------------           Alcatel Broadband Systems
 |  A L C A T E L  |           Excelsiorlaan 44-46 - 1930 Zaventem - Belgium
 -------------------           Phone: +32 2 718 7119 - Fax: +32 2 718 7000 
  	 BBS		       Email: peeterjo@bsg.bel.alcatel.be

------------------------------------------------------------------------------

|#







