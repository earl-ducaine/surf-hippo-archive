;*******************************************************************************
;*
;*	Make-Assertions
;*
;*******************************************************************************

(defun make-membership-assertion (rule obj)
  #'(lambda () (funcall rule (deref obj))))
(defun make-membership-assertions (rules obj)
  (cond ((null rules) nil)
	(t (cons (make-membership-assertion (car rules) obj)
		 (make-membership-assertions (cdr rules) obj)))))
(defun make-role-filler-assertion (role obj1 obj2)
  #'(lambda () (role-rule 
			(deref obj1)
			role
			(deref obj2))))
(defun make-attr-filler-assertion (attr	obj1 obj2)
  #'(lambda () (attr-rule 
			(deref obj1)
			attr
			(deref obj2))))

(defun make-negated-equation-assertion (obj1 obj2)
  #'(lambda () (unequal-rule (deref obj1) (deref obj2))))
(defun make-equation-assertion (obj1 obj2)
  #'(lambda () (equal-rule (deref obj1) (deref obj2))))
(defun make-abstract-predicate-assertion (apred)
  (propagate-all-abstract! (cdr apred))
  #'(lambda () (propagate-abstract-predicate apred)))
(defun make-concrete-predicate-assertion (cpred)
  #'(lambda () (apply (get-internal-rep-of-cpred (car cpred)) (cdr cpred) '())))
