;*******************************************************************************
;*
;*	File : Print Conjunctive Normal Form
;*
;*******************************************************************************

(defun print-cnf (p s k)
  (funcall (get-actual-cnf-print-function) p))

(defun get-actual-cnf-print-function ()
  (cond ((eq *print-cnf 'name)
	 #'print-conceptname-of-cnf)
	((eq *print-cnf 'all)
	 #'print-all-of-cnf)
	((eq *print-cnf 'pp)
	 #'pp-of-cnf)
	(t (error "Unknown Print Function in Get-Actual-CNF-Print-unction"))
	))
(defun set-print-name-of-cnf ()
  (setq *print-cnf 'name)
  )
(defun set-print-slot-of-cnf ()
  (setq *print-cnf 'all)
  )
(defun set-pretty-print-of-cnf ()
  (setq *print-cnf 'pp)
  )

(defun print-conceptname-of-cnf (p)
  (print (get-conceptname-of-cnf p)))
(defun print-slot-of-cnf (p)
  (print p))
(defun pp-of-cnf (p)
  (print (format nil "Concept : ~A" (get-conceptname-of-cnf p)))
  (terpri)
  (print-if (cnf-QPC p)					"QPCs                  		: ")
  (print-if (cnf-help-exists-links p)			"Some Links            		: ")
  (print-if (cnf-help-forall-links p)			"Forall Links			: ")
  (print-if (cnf-help-pos-definitive-prims p) 		"Definitive Positive Primitives	: ")
  (print-if (cnf-help-pos-possible-prims p)    		"Possible Positive Primitives   : ")
  (print-if (cnf-help-neg-definitive-prims p) 		"Definitive Negative Primitives	: ")
  (print-if (cnf-help-neg-possible-prims p)    		"Possible Negative Primitives   : ")
  (print-if (cnf-help-pos-definitive-families p)	"Definitive Positive Families	: ")
  (print-if (cnf-help-pos-possible-families p) 		"Possible Positive Families	: ")
  (print-if (cnf-help-neg-definitive-families p) 	"Definitive Negative Families	: ")
  (print-if (cnf-help-neg-possible-families p)    	"Possible Negative Families	: ")
  (print-if (cnf-help-agree p)		    		"Empty Agree           		: ")
  (print-if (cnf-help-disagree p)			"Empty Disagree	       		: ")	
  )

(defun print-if (value name)
  ;;; called by print-functions
  ;;; print iff value are not nil
  (cond (value
	 (princ (format nil "    ~A ~A" name value))
	 (terpri)))
  )

(defun format-cnf (cnf-list)
  (mapcar #'get-conceptname-of-cnf cnf-list))
