;*******************************************************************************
;*
;*	File : Print Rule
;*
;*******************************************************************************

(defvar *print-rule nil)

(defun set-print-rules ()
    (setq *print-rule t))
(defun reset-print-rules ()
    (setq *print-rule nil))


(defmacro print-rule (rule x &optional y obj2)
  `(if *print-rule
       (pp-rule ,rule ,x ,y ,obj2)))

(defun pp-rule (rule x &optional y obj2)
  (cond ((not *print-rule))
	((not y)
	 (format t "~%Call Rule : ~20S - ~13S" rule (get-id x)))
	(obj2
	 (if (relation? y)
	     (format t "~%Call Rule : ~20S - ~13S - ~13S - ~13S"
		     rule (get-id x) y (get-id obj2))
	     (format t "~%Call Rule : ~20S - ~13S - ~13S - ~13S"
		     rule x y (get-id obj2))))
	((object? x)
	 (format t "~%Call Rule : ~20S - ~13S - ~13S"
		 rule (get-id x) (get-id y)))
	((cnf? x)
	 (format t "~%Call Rule : ~20S - ~13S - ~13S"
		 rule (get-conceptname-of-cnf x)
		 (get-id y)))
	((listp x)
	 (format t "~%Call Rule : ~20S - ~13S"
		 rule 
		 (get-id y)))
	(t (format t "~%Call Rule : ~20S - ~13S - ~13S"
		   rule x (get-id y))))
  )

