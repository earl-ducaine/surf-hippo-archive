;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; Macros and some misc. functions needed by the surf circuit simulator package.


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")

;;; DEFVARS and DEFVAR-W-VALUE are macros that allow the declaring and
;;; assigning of more than one global variable at a time.

(defmacro defvars (&rest list)
  `(progn ,@(loop for variable in list collect `(defvar ,variable))))

(defmacro defvars-w-value (&rest list)
   `(progn ,@(loop for (variable value) in list collect `(defvar ,variable ,value))))

#+sun
(defmacro without-floating-underflow-traps (sexp)
  sexp)

#+sun
(defmacro cond-every (&rest list)
  `(progn ,@(loop for (condit exec) in list collect `(if ,condit ,exec))))
; 
; #+3600
; (defmacro cl-setf (place new-value &rest more-pairs &aux vals)
;   (if (null more-pairs)
;       `(setf ,place ,new-value)
;       (do ((v more-pairs (cddr v)))
; 	  ((null v) (setq vals (reverse vals)))
; 	(push `(setf ,(car v) ,(cadr v)) vals))
;       `(progn 'compile
; 	      (setf ,place ,new-value)
; 	      ,@vals)))
; 
; #+LAMBDA
; (defmacro cl-setf (&rest more-pairs)
;   `(setf ,@more-pairs))
;(defmacro cl-setf (&rest more-pairs)
;  `(cl:setf ,@more-pairs))

#-parallel
(defmacro *all (&body body)
  `(let ()
     ., body))


#+parallel
(defmacro *select-type (types &body body)
  `(*let ((*lisp-context nil!!))
     (declare (type (pvar boolean) *lisp-context))
     (dolist (ptype ',types)
       (*if (=!! *lisp-struct-type (!! (the (unsigned-byte 8) (eval ptype))))
	    (*set *lisp-context t!!)))
     (*when *lisp-context
       ., body)))

#+parallel
(defmacro float-abs!! (f)
  `(if!! (>=!! ,f (!! zero))
	 ,f
	 (-!! ,f)))

#+parallel
(defmacro float-minusp!! (f)
  `(<!! ,f (!! zero)))

#+parallel
(defmacro float-plusp!! (f)
  `(>!! ,f (!! zero)))
#|
#+parallel
(eval-when (load)
  (if (not (boundp 'cm:*cm-simulator-type*))
      (defun cm:time (&rest rest)
	)))
|#
#|
#+parallel
(defun my-ppp (pvar)
  (*all (*when fanout-valid
	  (do-for-selected-processors (x)
	    (format t "~a " (pref pvar x))))))
|#

(defun set-create-parameters (parameter-name model parameters)
  (let (parameter-value temp-parameter-value junk)
    (setf 
     parameter-value (eval (cdr
			    (assoc parameter-name
				   (model-template-default-params
				    (model-instance-model model)))))
     temp-parameter-value (eval (cdr
				  (assoc parameter-name (model-instance-changed-params
							      model))))
      junk (if temp-parameter-value
	       (setf parameter-value temp-parameter-value))
      temp-parameter-value (eval (cdr (assoc parameter-name parameters)))
      junk (if temp-parameter-value
	       (setf parameter-value temp-parameter-value )))
    parameter-value))

(defun my-xor (a b)
  (if a
      (if b nil t)
      (if b t nil)))

(defun square (x)
  (declare (single-float x))
  (* x x))

(defun list-to-array (list)
  (declare (list list))
  (let ((array (make-array (length list)))
	(i 0))
    (declare (fixnum i))
    (dolist  (element list)
      (setf (aref array i) element)
      (setq i (+ 1 i)))
    array))


;;; PRINT-TABLE-KEYS and PRINTIT Functions to print out the keys of a hash table.
(defun print-table-keys (table)
  (maphash 'printit table))

(defun printit (name nd)
  (declare (ignore nd))
  (print name))



#+sun
(defun named-structure-symbol (st)
  (type-of st))

(defun sim-error (message &rest args)
  "A fatal error occured, print the message and abort."
  (apply #'error (cons (string message) args)))
;  (eval `(error (string ,message) ,@args)))

(defun sim-warning (message &rest args)
  "A warning has occured, print the message and continue."
  (apply #'cerror (cons (string "continue") (cons (string message) args))))
;  (eval `(cerror (string "continue") (string ,message) ,@args)))
