;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey Drake
;;;
;;; Utilities
;;;


;;; PACKAGE

(in-package "UTILS")


;;; MACROS AND FUNCTIONS

(proclaim '(inline opposite))

(defmacro with-point ((col row point) &body body)
  "Binds COL and ROW to the car and cdr of POINT."
  `(let ((,col (car ,point))
	 (,row (cdr ,point)))
     ,@body))

(defun coordinates (location)
  "Returns the coordinate string for LOCATION."
  (format NIL "~a~d"
	  (case (car location)
	    (1 #\a)  (2 #\b)  (3 #\c)  (4 #\d)  (5 #\e)
	    (6 #\f)  (7 #\g)  (8 #\h)  (9 #\j)  (10 #\k)
	    (11 #\l) (12 #\m) (13 #\n) (14 #\o) (15 #\p)
	    (16 #\q) (17 #\r) (18 #\s) (19 #\t))
	  (cdr location)))
  
(defun opposite (color)
  ":black and :white are opposite;  the opposite of anything else is NIL."
  (case color
    (:black :white)
    (:white :black)
    (T NIL)))

(defmacro nor (&rest args)
  "Logical nor."
  `(not (or ,@args)))


;;; MACROS AND FUNCTIONS FROM PAUL GRAHAM'S _ON_LISP_

(proclaim '(inline single))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

