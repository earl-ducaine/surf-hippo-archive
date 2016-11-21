;*******************************************************************************
;*
;*	File : Clash
;*
;*******************************************************************************

(defmacro catch-clash (process)
  `(catch 'clash ,process))
(defun clash (&rest list)
  (throw 'clash (cons 'backtrack list)))

(defun clash? (value)
  (and (listp value) (eq (car value) 'backtrack)))

;;; print clash

(defvar *print-clash nil)

(defun set-print-clash ()
  (setq *print-clash t)
  )
(defun reset-print-clash ()
  (setq *print-clash nil)
  )

(defmacro print-clash (l)
  `(if *print-clash
       (pp-clash ,l)))

(defun pp-clash (l)
  (let ((rule (cadr l))
	(second (caddr l))
	(obj (cadddr l))
	)
       (let ((id (cond ((object? obj)
			(get-id obj))
		       (t obj)))
	     )
	    (cond ((= (length l) 2)
		   (format t 
			   "~%*>> Clash : ~20S - ~13s" 
			   rule
			   id))
		  ((object? second)
		   (format t
			   "~%*>> Clash : ~20S - ~13s - ~13s"
			   rule
			   (get-id second)
			   id))
		  ((cnf? second)
		   (format t
			   "~%*>> Clash : ~20S - ~13s - ~13s"
			   rule
			   (get-conceptname-of-cnf second)
			   id))
		  (t (format t
			     "~%*>> Clash : ~20S - ~13s - ~13s"
			     rule
			     second
			     id))))))
