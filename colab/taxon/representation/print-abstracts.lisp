;*******************************************************************************
;*
;*	File : Print Abstract Objects
;*
;*******************************************************************************

(defun print-abstract (p s k)
  (funcall (get-actual-print-abstract-function) p))

;;; select and set print function

(defun get-actual-print-abstract-function ()
  (declare (special *print-abstract))
  (cond ((eq *print-abstract 'id)
	 #'print-id-of-abstract)
	((eq *print-abstract 'type+id)
	 #'print-type+id-abstract)
	((eq *print-abstract 'pp)
	 #'pp-of-abstract)
	(t (error "Unknown Print Function in Get-Actual-Print-Function"))
	))
(defun set-print-id-of-abstract ()
  (declare (special *print-abstract))
  (setq *print-abstract 'id)
  )
(defun set-print-type+id-of-abstract ()
  (declare (special *print-abstract))
  (setq *print-abstract 'type+id)
  )
(defun set-pretty-print-of-abstract ()
  (declare (special *print-abstract))
  (setq *print-abstract 'pp)
  )

;;; print functions

(defun print-id-of-abstract (p)
  (print (object-id (deref p))))
(defun print-type+id-abstract (p)
  (print (format nil 
		 "Abstract Object : ~A -- First : ~A" 
		 (object-id (deref p)) 
		 (object-id p))))
(defun pp-of-abstract (p)
  (if (eq (object-id (deref p)) (object-id p))
      (format t "Abstract Object : ~A" (object-id p))
      (format t "Abstract Object : ~A -- First : ~A" (object-id (deref p)) (object-id p)))
  (terpri)
  (print-if (object-prim p) 			"  Primitives :                 ")
  (print-if (object-neg-prim p)			"  Negated Primitives :         ")
  (print-if (format-cnf (object-concept p))	"  Concepts :                   ")
  (print-if (format-cnf (object-neg-concept p)) "  Negated Concepts :           ")
  (print-if (format-rfs (object-role-filler p))	"  Role Filler :                ") 
  (print-if (format-rfs (object-attr-filler p))	"  Attribute Filler :           ")
  (when (object-print-cf p)
	(format t "      Conditional Functions :      ")
	(print-conditional-functions (object-print-cf p)))
  (set-print-id-of-abstract)
  (print-if (object-apred p)			"  Abstract Predicates :        ")
  (print-if (object-neg-apred p)		"  Negated Abstract Predicates :")
  (set-pretty-print-of-abstract)
  )

;;; format functions

(defun format-rfs (role-fillers)
  (mapcar #'format-rf role-fillers))
(defun format-rf (role-filler)
  (list (car role-filler)
	(get-dereferenced-id (cadr role-filler))))


(defun print-conditional-functions (slot)
  (dolist (fct slot)
	  (apply (car fct) (cadr fct))))

(defun print-cond-fct-of-F_C (fct relation)
  (let ((user-defined-entry (get-definition-from-function fct))
	)
       (let ((actual-path (cons relation (get-pathes-of-definition-entry user-defined-entry)))
	     (restrictor  (get-restrictor-of-definition-entry user-defined-entry))
	     )
	    (format t "~%~10TF_C: Actual-Path: ~A Restrictor: ~A"
		    actual-path restrictor)
	    )))

(defun print-cond-fct-of-F_P (fct relation first pred)
  (let ((user-defined-entry (get-definition-from-function fct))
	)
       (let ((actual-path (cons relation (get-first-path-of-pathes 
					  (get-pathes-of-definition-entry user-defined-entry))))
	     (rest-pathes (rest-pathes (get-pathes-of-definition-entry user-defined-entry)))
	     (restrictor  (get-restrictor-of-definition-entry user-defined-entry))
	     )
	    (format t "~%~10TF_P: Actual-Path: ~A First: ~A Rest-Pathes: ~A Restrictor: ~A"
		    actual-path (get-id first) rest-pathes restrictor)
	    )))
