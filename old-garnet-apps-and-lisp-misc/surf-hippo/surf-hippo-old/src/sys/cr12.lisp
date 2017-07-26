;;; -*- Package: *LISP-CL; Syntax: Common-lisp -*-
;;; Tridiagonal Solver using Cyclic Reduction.
;;; "Paris version" of cr11.lisp.
;;; Times are measured for Paris code, both for releases 9 and 10.
;;; The timings in *lisp are copied from cr11.lisp for easy comparison.
;;; ====================================================================
;;; 
;;; Measured cm:time: (release 9)
;;;         *lisp                    *lisp
;;;     N   time(sec)   busy%     N  new-time  busy%     N  Paris time   busy%
;;;     4   0.18         37       4   0.09      59       4    0.030       89
;;;     8   0.25         40       8   0.13      61       8    0.041       92
;;;    16   0.34         43      16   0.18      65      16    0.052       95
;;;    32   0.42         42      32   0.22      64      32    0.063       95
;;;    64   0.52         41      64   0.27      61      64    0.075       94
;;;   128   0.60         40     128   0.31      61     128    0.088       93
;;;   256   0.68         41     256   0.35      61     256    0.100       93
;;;   512   0.75         42     512   0.40      61     512    0.112       93
;;;  1024   0.81         40    1024   0.45      60    1024    0.123       93
;;;  2048   0.89         43    2048   0.50      62    2048    0.135       92
;;;  4096   0.96         41    4096   0.53      61    4096    0.148       92
;;;  8192   1.04         42    8192   0.58      61    8192    0.160       92
;;; 16384   1.13         41   16384 error Ignatius.  16384    0.173       91
;;;======================================================================
;;; Measured cm:time:
;;; For release 10, Paris time:
;;;      N      time
;;;      4      23.6 ms
;;;      8      32.6 ms
;;;     16      42.3 ms
;;;     32      52.6 ms
;;;     64      63.4 ms
;;;    128      74.3 ms
;;;    256      86.6 ms
;;;    512      95.9 ms
;;;   1024     106.7 ms
;;;   2048     117.7 ms
;;;   4096     128.4 ms
;;;   8192     139   ms
;;;  16384     150   ms
;;;====================================================================
;;; Break Down Analysis:
;;; For release 10, N = 16384:
;;; total time = 150 ms
;;; arith time =  87 ms  58%.
;;;  comm time =  14 ms   9%
;;; others CM  =  32 ms  21% (data movement, Paris 25us overhead etc.)
;;; others LispM = 17 ms 11% (CM idle)
;;;---------------------------
;;; For 87 ms arith:
;;;  ==> 6.2 ms per iteration.
;;;    Forward 1 /, 3 *, 4 +.
;;;    Backward 1 *, 2 +
;;;    ==> 1 /, 4 *, 6 +.
;;;---------------------------
;;; For 14 ms comm:
;;;  ==> 1062 us per iteration.
;;; Among which, 128-bit float Router-send == 408 us per send. (in average).
;;;               96-bit float Router-send == 300 us per send.
;;;               32-bit float Router-send == 177 us per send.
;;;==================================================================== 
;;;
 
(proclaim '((float-pvar 23 8) x))

(proclaim '(boolean-pvar valid-element))

(*defvar x)

(*defvar valid-element)

(defvar N)

(defvar common-out)  (defvar common-left) 
(defvar common-right)  (defvar common-tmp)

(defvar aa)  (defvar bb)  (defvar cc)  (defvar yy)

(defvar al)  (defvar bl)  (defvar cl)  (defvar yl)

(defvar ar)  (defvar br)  (defvar cr)  (defvar yr)

(defvar tmp-a-c)  (defvar tmp-b)  (defvar tmp-y)

(defun initialize-the-variables-again ()  
  (setq common-out (allocate!! nil nil '(type (field-pvar 128))))
  
  (setq common-left (allocate!! nil nil '(type (field-pvar 128))))
  
  (setq common-right (allocate!! nil nil '(type (field-pvar 128))))
  
  (setq common-tmp (allocate!! nil nil '(type (field-pvar 96))))
  
  (setq aa (make-pvar :location (+ 0  (pvar-location common-out)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	bb (make-pvar :location (+ 32 (pvar-location common-out)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	cc (make-pvar :location (+ 64 (pvar-location common-out)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	yy (make-pvar :location (+ 96 (pvar-location common-out)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8))
  
  (setq al (make-pvar :location (+ 0  (pvar-location common-left)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	bl (make-pvar :location (+ 32 (pvar-location common-left)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	cl (make-pvar :location (+ 64 (pvar-location common-left)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	yl (make-pvar :location (+ 96 (pvar-location common-left)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8))
  
  (setq ar (make-pvar :location (+ 0  (pvar-location common-right)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	br (make-pvar :location (+ 32 (pvar-location common-right)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	cr (make-pvar :location (+ 64 (pvar-location common-right)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8)
	yr (make-pvar :location (+ 96 (pvar-location common-right)) :length 32 
		      :type :float :mantissa-length 23 :exponent-length 8))
  
  (setq tmp-a-c (make-pvar :location (+ 0  (pvar-location common-tmp)) :length 32 
			   :type :float :mantissa-length 23 :exponent-length 8)
	tmp-b (make-pvar :location (+ 32  (pvar-location common-tmp)) :length 32 
			 :type :float :mantissa-length 23 :exponent-length 8)
	tmp-y (make-pvar :location (+ 64 (pvar-location common-tmp)) :length 32 
			 :type :float :mantissa-length 23 :exponent-length 8))
)  


(defun pp-cr (pvar N)
    (terpri)
    (loop for i from 0 to (1- N) do
      (let ((val (pref pvar i)))
	(cond ((floatp val) (cl:format t "~10,2f  " val))
	      ((null val) (format t "~6d" "N"))
	      (T (format t "~6d" val))))))

(defun initialize-cr12 (size1)
  (setq N size1)
  (when (> size1 *number-of-processors-limit*)
    (setq N *number-of-processors-limit*)
    (format t "~%Warning: size > hardware size, force size changed to ~a" N))
  (*if (<!! (self-address!!) (!! N))
       (*set valid-element t!!)
       (*set valid-element nil!!))
  (*when valid-element
    (*let ((id (self-address!!)))
      (declare ((field-pvar 16) id))
       (*if (=!! id (!! 0))
	    (*set aa (!! 0.0))
  	    (*set aa (/!! (random!! (!! 10000)) (!! 100))))
       (*set bb (+!! (/!! (random!! (!! 10000)) (!! 100)) (!! 200.0)))
       (*if (=!! id (!! (- N 1)))
	    (*set cc (!! 0.0))
  	    (*set cc (/!! (random!! (!! 10000)) (!! 100))))
       (*set x (/!! (random!! (!! 10000)) (!! 100)))
;;       (format t "~%pridicted x values:")
;;       (pp-cr x N)
       (*set yy (+!! (*!! aa (if!! (=!! id (!! 0))
				 (!! 0.0)
 				 (pref!! x (1-!! id))))
		    (*!! bb x)
		    (*!! cc (if!! (=!! id (!! (1- N)))
				 (!! 0.0)
				 (pref!! x (1+!! id))))))
       (*set x (!! 0.0)))))

(*defun cr12 (N aa bb cc yy)
;;  (cm:time
    (let ((max-loop-times (1- (floor (log N 2))))
	  (dist 1)
	  (loc-common-out (pvar-location common-out))
	  (loc-common-left (pvar-location common-left))
	  (loc-common-right (pvar-location common-right))
	  (loc-common-tmp (pvar-location common-tmp))
	  (loc-aa (pvar-location aa))
	  (loc-bb (pvar-location bb))
	  (loc-cc (pvar-location cc))
	  (loc-yy (pvar-location yy))
	  (loc-al (pvar-location al))
	  (loc-bl (pvar-location bl))
	  (loc-cl (pvar-location cl))
	  (loc-ar (pvar-location ar))
	  (loc-br (pvar-location br))
	  (loc-cr (pvar-location cr))
	  (loc-yr (pvar-location yr))
	  (loc-tmp-a-c (pvar-location tmp-a-c))
	  (loc-tmp-b (pvar-location tmp-b))
	  (loc-tmp-y (pvar-location tmp-y))
	  (loc-x (pvar-location x)))
      (*let ((id (self-address!!))
	     (major-row)
	     (active-row t!!)
	     (alpha)
	     (tmp1)
	     (tmp2)
	     (tmp3)
	     (tmp4)
	     (tmp-in)
	     (tmp-out)
	     (recip-bb)
	     (loop-times (!! 0))
	     (left-neighbor)
	     (right-neighbor)
	     (tmp-flag))
	(declare ((field-pvar 16) loop-times))
	(declare ((field-pvar 17) id))
	(declare (boolean-pvar major-row active-row tmp-flag))
	(declare ((float-pvar 23 8) alpha tmp1 tmp2 tmp3 tmp4 
		  tmp-in tmp-out recip-bb))
	(declare ((signed-pvar 17) left-neighbor right-neighbor))
	(let ((loc-id (pvar-location id))
	      (loc-major-row (pvar-location major-row))
	      (loc-active-row (pvar-location active-row))
	      (loc-alpha (pvar-location alpha))
	      (loc-tmp1 (pvar-location tmp1))
	      (loc-tmp2 (pvar-location tmp2))
	      (loc-tmp3 (pvar-location tmp3))
	      (loc-tmp4 (pvar-location tmp4))
	      (loc-tmp-in (pvar-location tmp-in))
	      (loc-tmp-out (pvar-location tmp-out))
	      (loc-recip-bb (pvar-location recip-bb))
	      (loc-loop-times (pvar-location loop-times))
	      (loc-left-neighbor (pvar-location left-neighbor))
	      (loc-right-neighbor (pvar-location right-neighbor)))
	  
	  ;; The forward reduction.
	  ;; Let i be major row.
	  ;;   Processor i send ai up.
	  ;;   Processor i+1 send (ai+1 bi+1 ci+1 yi+1) up.
	  ;;
	  (cm:move-constant loc-major-row 1 1)
	  (loop for i from 0 to max-loop-times do
	    (*when active-row
	      (cm:move loc-left-neighbor loc-id 17)
	      (cm:u-constant loc-left-neighbor dist 17)
	      (cm:move loc-right-neighbor loc-id 17)
	      (cm:u+constant loc-right-neighbor dist 17)
	      (cm:move-constant loc-loop-times i 16)
	      
	      ;; if ith-bit of i then major-row.
	      (cm:unsigned-plusp (+ loc-id i) 1)
	      (cm:move loc-major-row cm:test-flag 1)

	      (*when (>=!! left-neighbor (!! 0))
		(let ((num-petty-cycles))  ;;; added by ajit - not needed here.
		  (setq num-petty-cycles
			(cm:send loc-common-right loc-left-neighbor loc-common-out 128))
		  (format t "~% The number of petty cycles in the ~ath loop is ~a"
			  i num-petty-cycles))) 
	      (*when (<!! right-neighbor (!! N))
		(*when major-row
		  (cm:move loc-alpha loc-cc 32)
		  (cm:move loc-tmp1 loc-br 32)
		  (cm:move loc-tmp2 loc-cr 32)
		  (cm:move loc-tmp3 loc-ar 32)
		  (cm:move loc-tmp4 loc-yr 32))
		(*when (not!! major-row)
		  (cm:move loc-alpha loc-ar 32)
		  (cm:move loc-tmp1 loc-bb 32)
		  (cm:move loc-tmp2 loc-aa 32)
		  (cm:move loc-tmp3 loc-cc 32)
		  (cm:move loc-tmp4 loc-yy 32))
		(cm:f/ loc-alpha loc-tmp1)
		(cm:float-negate loc-alpha loc-alpha)
		(cm:move loc-tmp-a-c loc-alpha 32)
		(cm:f* loc-tmp-a-c loc-tmp2)
		(cm:move loc-tmp-b loc-alpha 32)
		(cm:f* loc-tmp-b loc-tmp3)
		(cm:move loc-tmp-y loc-alpha 32)
		(cm:f* loc-tmp-y loc-tmp4)
		(*when major-row
		  (cm:move loc-cc loc-tmp-a-c 32)
		  (cm:f+ loc-bb loc-tmp-b)
		  (cm:f+ loc-yy loc-tmp-y))
		(*when (not!! major-row)
		  ;; send packed data to the right major row, 
		  ;;  i.e., tmp-b, tmp-a-c and tmp-y.
		  (cm:send loc-common-left loc-right-neighbor loc-common-tmp 96)))
	      (*when (>=!! left-neighbor (!! 0))
		(*when major-row
		  (cm:f+ loc-bb loc-bl)
		  (cm:move loc-aa loc-al 32)
		  (cm:f+ loc-yy loc-cl)))
	      (cm:move loc-active-row loc-major-row 1)
	      (setq dist (+ dist dist))))
	  (setq dist (/ dist 2))
	  ;; If active-row 
	  ;;    then compute x = yy / bb
	  ;;    else compute recip-bb = 1/ bb 
	  ;; Overlape divisions here in order to convert 
	  ;;    from logN divisions to logN 
	  ;;     multiplications in the back substitution phase.
	  
	  (*if active-row
	       (cm:move loc-tmp1 loc-yy 32)
	       (cm:float-move-constant loc-tmp1 1.0))
	  (cm:move loc-recip-bb loc-tmp1 32)
	  (cm:f/ loc-recip-bb loc-bb)
	  (*when active-row
	    (cm:move loc-x loc-recip-bb 32)
	    (cm:u+constant loc-loop-times 1 16))
	  
	  ;;; Back Substitution Phase.
	  (loop for i from max-loop-times downto 0 do
	    (cm:move loc-left-neighbor loc-id 17)
	    (cm:u-constant loc-left-neighbor dist 17)
	    (cm:move loc-right-neighbor loc-id 17)
	    (cm:u+constant loc-right-neighbor dist 17)
	    (cm:move loc-major-row loc-active-row 1)
	    (cm:u>=constant loc-loop-times i 16)
	    (cm:move loc-active-row cm:test-flag 1)
	    (*when active-row
	      (*when major-row
		(cm:move loc-tmp-out loc-x 32)
		(cm:move loc-tmp1 loc-x 32))
	      (*when (not!! major-row)
		(cm:move loc-tmp-out loc-cc 32)
		(cm:move loc-tmp1 loc-aa 32))
	      (*when (<!! right-neighbor (!! N))
		(cm:send loc-tmp-in loc-right-neighbor loc-tmp-out 32))
	      (*when (>=!! left-neighbor (!! 0))
		(cm:move loc-tmp2 loc-tmp1 32)
		(cm:f* loc-tmp2 loc-tmp-in)
		(*if major-row
		     (cm:send loc-tmp-in loc-left-neighbor loc-tmp2 32)
		     (cm:f- loc-yy loc-tmp2)))
	      (*when (<!! right-neighbor (!! N))
		(*when (not!! major-row)
		  (cm:f- loc-yy loc-tmp-in)))
	      (*when (not!! major-row)
		(cm:move loc-x loc-yy 32)))
	    (setq dist (/ dist 2))))))
    ;; )
)

(defun do-cr12 (size1)
  (*cold-boot)
  (initialize-the-variables-again) ;; added by ajit - to reinitialize the variables
                                   ;; after the cold-boot - so a *cold-boot should go 
                                   ;; before it.
  (setq *check-arg-p* nil)
  (initialize-cr12size1)
  (*when valid-element
    (cr12 N aa bb cc yy))
  (format t "~%Computed x values:")
  (pp-cr x N)
  )

;;; *end-of-file*
