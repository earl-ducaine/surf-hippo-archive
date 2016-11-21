;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-



;;; module:

;;;


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:



( in-package "CONTAX" )

( export '( def-interval-fn ))


;*****************************************************************
;
;      Intervall funktionen
; 
;******************************************************************

;*******************************************************************
;
;     Formt alle Intervall in der liste l in ihre zahlen um
;
;*******************************************************************

(defun read-intervalls (l)
  (cond ((null l) nil)
	((interval-p (car l))
	 (append (intervall-to-list (car l)) 
		 (read-intervalls (cdr l))))
	(t (cons (car l)
		 (read-intervalls (cdr l))))
	)
  )


;*******************************************************************
;
;   wandelt ein intervall in die liste seiner zahlen
;
;*********************************************************************

(defun intervall-to-list (i)
  (do ((erg (list (car i)) (cons l erg))
       (l (1+ (car i)) (1+ l)))
       ((> l (cadr i)) erg)
       )
      )

;********** test ob i ein intervall ist

(defun interval-p (i)
  (and (listp i) (= 2 (length i)) (numberp (car i)) (numberp (cadr i)))
)

;*****************************************************************
;     sortiert alle zahlen aus und wandelt sie in intervalle

(defun write-intervalls (l)
  (append (list-to-intervall (sort (remove-if-not #'numberp l) #'<))
	  (remove-if #'numberp l))

)


(defun expand-intervalls (l)
 (copy-list (mapcan #'expand-one-intervall l)))

(defun expand-one-intervall (l)
  (expand-number-lists (mapcar #'(lambda (x)
				   (cond ((interval-p x) (intervall-to-list x))
					 (t (list x))
					 )
				   )
			       l
			       ))
  )

(defun expand-number-lists (l)
  (compute-combinations l))


;*****************************************************************
;     wandelt eine liste von zahlen in eine von intervallen

(defun list-to-intervall (l)
  (cond ((null l) nil)
	((= 1 (length l)) (cons (car l) (list-to-intervall (cdr l))))
	((neighboured-p (first l) (second l))
	 (make-intervall (first l) (second l) (cddr l)))
	(t (cons (car l) (list-to-intervall (cdr l))))
	)
)

(defun make-intervall (lb ub l)
 (cond 
  ((null l)
   (list (list lb ub)))
  ((neighboured-p ub (first l))
   (make-intervall lb (first l) (cdr l)))
  (t (cons (list lb ub) (list-to-intervall l)))
  )
)

;***********************************************************
;  test ob zwei zahlen benachbart sind

(defun neighboured-p (m n)
  (= 1 (abs (- m n))))



