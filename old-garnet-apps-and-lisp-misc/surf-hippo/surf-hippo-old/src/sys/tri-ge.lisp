;;; -*- mode: lisp; Syntax: Common-lisp; package: surf; base: 10;  -*-
;;; Solve a tri-diagional system by Guass Elimination on LispM.
;;; N = Size of Matrix.
;;; ======================================
;;; Measured cl:time:
;;;      N    time      Kflops         
;;;      4    0.00052    69
;;;      8    0.00112    64
;;;     16    0.0023     61
;;;     32    0.0047     61
;;;     64    0.0095     61
;;;    128    0.019      61
;;;    256    0.038      61
;;;    512    0.083      56
;;;   1024    0.165      56
;;;   2048    0.32       58
;;;   4096    0.65       57
;;;   8192    1.32       56
;;;  16384    2.70       55
;;; ======================================
;;;



#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defun print-vector (v N vname)
  (loop for i from 0 to (1- N) do
    (format t "~% ~a (~a) = ~a" vname i (aref v i))))

(defun tri-ge (a b c x y N)
  (loop for i from 1 to (1- N) do
    ;;; forward elimination.
    ;;; alpha = -a (i) / b (i-1).
    ;;; b (i) = b (i) + alpha * c (i-1).
    ;;; y (i) = y (i) + alpha * y (i-1).
    (let ((alpha (- (/ (aref a i) (aref b (1- i))))))
      (setf (aref b i) (+ (aref b i) (* alpha (aref c (1- i)))))
      (setf (aref y i) (+ (aref y i) (* alpha (aref y (1- i)))))))
  (setf (aref x (1- N)) (/ (aref y (1- N)) (aref b (1- N))))
  (loop for i from (- N 2) downto 0 do
    ;;; backward elimination.
    ;;; y (i) = y (i) - (c (i) / b (i+1)) * y (i+1).
    ;;; x (i) = y (i) / b (i).
    (setf (aref y i) (- (aref y i) (* (/ (aref c i) (aref b (1+ i))) (aref y (1+ i)))))
    (setf (aref x i) (/ (aref y i) (aref b i))))
  x)

(defun do-tri-ge (N)
  (let ((a (make-array (list N) :element-type 'single-float :initial-element -1.0))
	(b (make-array (list N) :element-type 'single-float :initial-element 3.0))
	(c (make-array (list N) :element-type 'single-float :initial-element -1.0))
	(x (make-array (list N) :element-type 'single-float))
	(y (make-array (list N) :element-type 'single-float :initial-element 1.0)))
    (setf (aref a 0) 0.0)
    (setf (aref y 0) 2.0)
    (setf (aref c (1- N)) 0.0)
    (setf (aref y (1- N)) 2.0)
    (time (tri-ge a b c x y N))
    (print-vector x N 'x)
    ))
