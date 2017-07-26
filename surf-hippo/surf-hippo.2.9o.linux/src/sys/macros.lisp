;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF ; Base: 10; -*-
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


;;; SYS Source file: macros.lisp


;;; Macros and some misc. functions needed by the surf circuit simulator package.

(IN-PACKAGE "SURF-HIPPO")


;; Used especially in the storage of plot data.
(defmacro push-a-value (value key alist)
  `(or
    (do ((alist ,alist (cdr alist)))
	((or (endp alist)
	     (eq ,key (caar alist)))
	 (when (cdar alist)
	   (push ,value (cdar alist)))))

    ;; (setf (cdr ,alist) (list (cons ,key (list ,value))))
    (push (cons ,key (list ,value)) ,alist)))

(defmacro without-floating-underflow-traps (sexp)
  sexp)

#-parallel
(defmacro *all (&body body)
  `(let ()
     ., body))

(defmacro accumulate-setf (setfable increment)
  `(setf ,setfable (+ ,setfable ,increment)))

(defmacro deccumulate-setf (setfable decrement)
  `(setf ,setfable (- ,setfable ,decrement)))



;; modified from code/list.lisp for ASSOC on symbol keys (using EQ instead of EQL).
;;; In run-time environment, since these guys can be inline expanded.
(defmacro my-assoc-guts (test-guy)
  `(do ((alist alist (cdr alist)))
       ((endp alist))
     (if (car alist)
	 (if ,test-guy (return (car alist))))))


(defun my-assoc (item alist &key key test test-not)
  "Returns the cons in alist whose car is equal (by a given test or EQL) to the ITEM."
  (cond (test (my-assoc-guts (funcall test item (lisp::apply-key key (caar alist)))))
	(test-not (my-assoc-guts (not (funcall test-not item
					    (lisp::apply-key key (caar alist))))))
	(t (my-assoc-guts (eq item (lisp::apply-key key (caar alist)))))))




(defmacro cdr-assoc (key a-list) `(cdr (assoc ,key ,a-list)))

(defmacro setfable-get-a-value (key a-list) `(cdr (my-assoc ,key ,a-list)))

#|

(defmacro get-a-value (key a-list) `(cdr (my-assoc ,key ,a-list)))

(defmacro get-a-value (key association-list)
  "Return the datum associated with KEY in ASSOCIATION-LIST."
  `(do ((association-list ,association-list (cdr association-list)))
       ((endp association-list))
     (when (and (car association-list)
		(eq ,key (caar association-list)))
       (return (cdar association-list)))))


(defmacro get-a-value (key association-list &optional (test #'eq))
  "Return the datum associated with KEY in ASSOCIATION-LIST, where KEY is matching using the
function TEST [default 'EQ]."
  `(let ((association-list ,association-list))
    (when (consp (car association-list))
      (do ((association-list association-list (cdr association-list)))
	  ((endp association-list))
	(when (and (car association-list)
		   (funcall ,test ,key (caar association-list)))
	  (return (cdar association-list)))))))

|#


(defun get-a-value (key association-list &optional (test #'eq))
  "Return the datum associated with KEY in ASSOCIATION-LIST, where KEY is matching using the
function TEST [default 'EQ]."
  (let ((association-list association-list))
    (when (consp (car association-list))
      (do ((association-list association-list (cdr association-list)))
	  ((endp association-list))
	(when (and (car association-list)
		   (funcall test key (caar association-list)))
	  (return (cdar association-list)))))))

(defun assoc-with-test (key association-list &optional (test #'eq))
  "Return ASSOC associated with KEY in ASSOCIATION-LIST, where KEY is matching using the
function TEST [default 'EQ]."
  (let ((association-list association-list))
    (when (consp (car association-list))
      (do ((association-list association-list (cdr association-list)))
	  ((endp association-list))
	(when (and (car association-list)
		   (funcall test key (caar association-list)))
	  (return (car association-list)))))))

(defun get-a-value (key association-list &optional (test #'eq))
  "Return the datum associated with KEY in ASSOCIATION-LIST, where KEY is matching using the
function TEST [default 'EQ]."
  (cdr (assoc-with-test key association-list test)))


(defmacro get-a-sf-value (key a-list) `(s-flt (cdr (my-assoc ,key ,a-list))))

(defmacro get-a-df-value (key a-list) `(d-flt (cdr (my-assoc ,key ,a-list))))


(defun evaluate-funspec-in-a-list (key a-list &optional other-args)
  (let ((funspec (append (get-a-value key a-list) other-args)))
    (evaluate-funspec funspec)))

(defun evaluate-funspec (funspec &optional other-args)
  (apply (car funspec) (append (cdr funspec) other-args)))

(defmacro ch-power (a-arg b-arg)
  `(let (((a ,a-arg)
	  (b ,b-arg)))
    (if (< (the single-float a) 1e-5)
	0.0
	(let ((res 1.0))
	  (declare (single-float res))
	  (dotimes (i (the fixnum b))
	    (setf res (the single-float (* res (the single-float a)))))
	  (the single-float res)))))

(defmacro max-double-macro (a b)
  "Return the larger of A and B, where both are double floats."
  `(let ((a-temp ,a)
	 (b-temp ,b))
    (declare (double-float a-temp b-temp))
    (if (< (the df a-temp) (the df b-temp))
	b-temp a-temp)))

#|
(defmacro ch-power-double-macro (a b)
  `(if (< (the double-float ,a) 1d-5)
      0.0d0
      (let ((res 1.0d0))
	(declare (double-float res))
	(dotimes (i (the fixnum ,b))
	  (setf res (the double-float (* res (the double-float ,a)))))
	(the double-float res))))
|#

#|
(defmacro ch-power-double-macro (a-arg b-arg)
  `(let ((a ,a-arg)(b ,b-arg))
    (declare (double-float a) (fixnum b))
    (case b
      (1 a)
      (2 (* a a))
      (3 (* a a a))
      (4 (* a a a a))
      (5 (* a a a a a))
      (6 (* a a a a a a))
      (7 (* a a a a a a a))
      (0 0.0d0) 
      (t (let ((res 1.0d0))
	   (declare (double-float res))
	   (dotimes (i b)
	     (setf res (* res a)))
	   res)))))
|#

;; NG's opaque version
(defmacro CH-POWER-DOUBLE-MACRO (a-arg b-arg)
  `(let ((a ,a-arg) (b ,b-arg))
    (declare (double-float a) (fixnum b))
    (the df 
     (case b
       (1 (the df a))
       (2 (the df (* (the df a) (the df a))))
       (3 (the df (* (the df a) (the df a) (the df a))))
       (4 (let ((a**2 (* (the df a) (the df a))))
	    (declare (double-float a**2))
	    (the df (* a**2 a**2))))
       (5 (let ((a**2 (* (the df a) (the df a))))
	    (declare (double-float a**2))
	    (the df (* (the df a) a**2 a**2))))
       (6 (let ((a**3 (* (the df a) (the df a) (the df a))))
	    (declare (double-float a**3))
	    (the df (* a**3 a**3))))
       (7 (let ((a**3 (* (the df a) (the df a) (the df a))))
	    (declare (double-float a**3))
	    (the df (* (the df a) a**3 a**3))))
       (0 0.0d0) 
       (t (let ((res 1.0d0))
	    (declare (double-float res))
	    (dotimes (i b)
	      (setf res (the df (* res (the df a)))))
	    res))))))





(defmacro dotimes-unrolled ((variable limit unrolling-factor
				      &optional result-form)
                            &body body)
  "Like DOTIMES, but unrolls the loop into groups of UNROLLING-FACTOR chunks.
UNROLLING-FACTOR must be a literal constant."
  (check-type unrolling-factor (satisfies constantp) "a literal constant")
  (let ((limit-var (gensym))		; Prevent multiple evaluation (ONCE-ONLY)
        (unrolled-limit-var (gensym)))	; Prevent variable shadowing
    `(let* ((,limit-var ,limit)
            (,unrolled-limit-var
	     (- ,limit-var (mod ,limit-var ,unrolling-factor))))
      (do ((,variable 0))
	  ((>= ,variable ,unrolled-limit-var)
	   ;; do the remaining iterations one at a time
	   (do ((,variable ,variable (1+ ,variable)))
	       ((>= ,variable ,limit-var) ,result-form)
	     ,@body))
	,@(loop for i from 0 to unrolling-factor
		append body
		collect `(incf ,variable))))))





