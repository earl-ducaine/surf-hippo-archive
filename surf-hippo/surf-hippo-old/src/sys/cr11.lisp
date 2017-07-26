;;; -*- Package: *D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI")surf(2 0 (NIL 0) (NIL NIL NIL) "CPTFONT"); Syntax: Common-lisp -*-
;;; Tridiagonal Solver using Cyclic Reduction.
;;; Processor id = self-address!!, *lisp code, arbitrary tri-diagonal matrix,
;;; binary encoding,  32-bit float. 
;;;
;;; Size = N, Coefficient = a, b, c, Unknown = x, rhs = y.
;;; Converge to xxx11..1 at each step. 
;;; Row i resides in processor i.
;;; Times are measured for *lisp code, release 9.
;;; New times are improved by declaring PVAR's.
;;; ===============================================================================
;;; Measured cm:time:
;;;     N   time(sec)   busy%         N    time(sec)   busy%    N  new-time  busy%
;;;                                   3    0.09         41
;;;     4   0.18         37           7    0.17         43      4   0.09      59
;;;     8   0.25         40          15    0.25         45      8   0.13      61
;;;    16   0.34         43          31    0.34         43     16   0.18      65
;;;    32   0.42         42          63    0.43         42     32   0.22      64
;;;    64   0.52         41         127    0.51         42     64   0.27      61
;;;   128   0.60         40         255    0.58         42    128   0.31      61
;;;   256   0.68         41         511    0.67         42    256   0.35      61 
;;;   512   0.75         42        1023    0.73         41    512   0.40      61
;;;  1024   0.81         40        2047    0.79         42   1024   0.45      60
;;;  2048   0.89         43        4095    0.88         41   2048   0.50      62
;;;  4096   0.96         41        8191    0.96         42   4096   0.53      61
;;;  8192   1.04         42       16383    1.04         42   8192   0.58      61
;;; 16384   1.13         41                                 16384   error Ignatius. port 0.
;;; ===============================================================================
;;;

(proclaim '(type (float-pvar 23 8) a))

(proclaim '(type (float-pvar 23 8) b))

(proclaim '(type (float-pvar 23 8) c))

(proclaim '(type (float-pvar 23 8) x))

(proclaim '(type (float-pvar 23 8) y))

(proclaim '(type boolean-pvar valid-element))

(*defvar a)

(*defvar b)

(*defvar c)

(*defvar x)

(*defvar y)

(*defvar valid-element)

(defvar N)

(defun pp-cr11 (pvar N)
    (terpri)
    (loop for i from 0 to (1- N) do
      (let ((val (pref pvar i)))
	(cond ((floatp val) (cl:format t "~10,2f" val))
	      ((null val) (format t "~6d" "N"))
	      (T (format t "~6d" val))))))

(defun initialize-cr11 (size1)
  (setq N size1)
  (when (> size1 *number-of-processors-limit*)
    (setq N *number-of-processors-limit*)
    (format t "~%Warning: size > hardware size, force size changed to ~a" N))
  (*if (<!! (self-address!!) (!! N))
       (*set valid-element t!!)
       (*set valid-element nil!!))
  (*when valid-element
    (*let ((id (self-address!!)))
      (declare (type (field-pvar 16) id))
       (*if (=!! id (!! 0))
	    (*set a (!! 0.0))
  	    (*set a (/!! (random!! (!! 10000)) (!! 100))))
       (*set b (/!! (random!! (!! 10000)) (!! 100)))
       (*if (=!! id (!! (- N 1)))
	    (*set c (!! 0.0))
  	    (*set c (/!! (random!! (!! 10000)) (!! 100))))
       (*set x (/!! (random!! (!! 10000)) (!! 100)))
;;       (format t "~%pridicted x values:")
;;       (pp-cr11 x N)
       (*set y (+!! (*!! a (if!! (=!! id (!! 0))
				 (!! 0.0)
				 (pref!! x (1-!! id))))
		    (*!! b x)
		    (*!! c (if!! (=!! id (!! (1- N)))
				 (!! 0.0)
				 (pref!! x (1+!! id))))))
       (*set x (!! 0.0)))))

(*defun cr11 (N a b c y)
  (time
    (*when valid-element
      (let ((max-loop-times (1- (floor (log N 2))))
	    (dist 1)) 
	(*let ((loop-times (!! 0))
	       (id (self-address!!)))
	  (declare (type (field-pvar 4) loop-times))
	  (declare (type (field-pvar 16) id))
	  ;;; The forward reduction.
	  (loop for i from 0 to max-loop-times
		do
	    (*when (and!! (=!! loop-times (!! i))
	    	    	  (=!! (load-byte!! id (!! i) (!! 1)) (!! 1)))
	      (*set loop-times (1+!! loop-times))
	      (*let ((left-neighbor (-!! id (!! dist)))
		     (right-neighbor (+!! id (!! dist))))
		(declare (type (signed-pvar 17) left-neighbor right-neighbor))
		(*when (>=!! left-neighbor (!! 0))
		  (*let ((alpha (-!! (/!! a (pref!! b left-neighbor)))))
		    (declare (type (float-pvar 23 8) alpha))
		    (*set a (*!! alpha (pref!! a left-neighbor)))
		    (*set b (+!! b 
				 (*!! alpha (pref!! c left-neighbor))))
		    (*set y (+!! y
				 (*!! alpha (pref!! y left-neighbor))))))
		(*when (<!! right-neighbor (!! N))
		  (*let ((beta (-!! (/!! c (pref!! b right-neighbor)))))
		    (declare (type (float-pvar 23 8) beta))
		    (*set c (*!! beta (pref!! c right-neighbor)))
		    (*set b (+!! b
				 (*!! beta (pref!! a right-neighbor))))
		    (*set y (+!! y
				 (*!! beta (pref!! y right-neighbor))))))
		(setq dist (* dist 2)))))
	  (*when (=!! loop-times (!! (1+ max-loop-times)))
	    (*set x (/!! y b)))
	  ;;; The backward substitution.
	  (loop for i from max-loop-times downto 0
		do
	    (setq dist (/ dist 2))
	    (*when (=!! loop-times (!! i))
	      (*let ((left-neighbor (-!! id (!! dist)))
		     (right-neighbor (+!! id (!! dist))))
		(declare (type (signed-pvar 17) left-neighbor right-neighbor))
		(*when (>=!! left-neighbor (!! 0))
		  (*set y (-!! y (*!! a (pref!! x left-neighbor)))))
		(*when (<!! right-neighbor (!! N))
		  (*set y (-!! y (*!! c (pref!! x right-neighbor))))))
	      (*set x (/!! y b)))))))))

(defun do-cr11 (size1)
       (*cold-boot)
       (initialize-cr11 size1)
       (cr11 N a b c y)
;;       (format t "~%Computed x values:")
;;       (pp-cr11 x N)
       )

;;; *end-of-file*
