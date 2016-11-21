;*******************************************************************************
;*
;*	Conditional Functions
;*
;*******************************************************************************

(defun make-attr-cf-C (attr function)
  #'(lambda (attr-fillers role-fillers) 
	    (dolist (attr-filler attr-fillers)
		    (cond ((eq (get-role attr-filler) attr)
			   (funcall function (get-filler attr-filler))
			   (return))))) 
  )

(defun make-role-cf-C (role function)
  #'(lambda (attr-fillers role-fillers) 
	    (dolist (role-filler role-fillers)
		    (if (eq (get-role role-filler) role)
			(funcall function (get-filler role-filler))
			)))
  )

(defun make-attr-cond-function (attr function pred first)
  #'(lambda (attr-fillers role-fillers) 
	    (dolist (attr-filler attr-fillers)
		    (cond ((eq (get-role attr-filler) attr)
			   (funcall function pred first (get-filler attr-filler))
			   (return))))) 
  )

(defun make-role-cond-function (role function pred first)
  #'(lambda (attr-fillers role-fillers) 
	    (dolist (role-filler role-fillers)
		    (if (eq (get-role role-filler) role)
			(funcall function pred first (get-filler role-filler))
			)))
  )

(defun trigger-path-rules (attr-fillers role-fillers cond-functions)
  (dolist (cond-function cond-functions)
	  (funcall cond-function attr-fillers role-fillers)))

(defun trigger-attr-path-rules (attr-filler cond-funcs)
  (dolist (cond-func cond-funcs)
	  (funcall cond-func (list attr-filler) nil)))

(defun trigger-role-filler-rules (role-filler cond-funcs)
  (dolist (cond-func cond-funcs)
	  (funcall cond-func nil (list role-filler))))

(defun trigger-abstract-functions (obj)
  (dolist (function (get-abstract-functions obj))
	  (funcall function obj)))
(defun trigger-concrete-functions (obj)
  (dolist (function (get-concrete-functions obj))
	  (funcall function obj)))
