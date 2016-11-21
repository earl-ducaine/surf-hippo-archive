;*******************************************************************************
;*
;*	Propagate Abstract Predicates 
;*
;*******************************************************************************

(defun propagate-abstract-predicate (apred)
  (let ((first-argument (deref (get-first-arg-of-pred apred)))
	)
       (cond ((apred-in? apred (get-neg-apreds first-argument))
	      (clash 'propagate-abstract-predicate apred))
	     (t (dolist (arg (get-all-args-of-pred apred))
			(add-apred-erasable! (deref arg) apred)))
	     )))

(defun propagate-neg-abstract-predicate (apred)
  (let ((first-argument (deref (get-first-arg-of-pred apred)))
	)
       (cond ((apred-in? apred (get-apreds first-argument))
	      (clash 'propagate-neg-abstract-predicate apred))
	     (t (dolist (arg (get-all-args-of-pred apred))
			(add-neg-apred-erasable! (deref arg) apred)))
	     )))

(defun propagate-abstract-predicates (apreds)
  (dolist (apred apreds)
	  (propagate-abstract-predicate apred)))
(defun propagate-neg-abstract-predicates (apreds)
  (dolist (apred apreds)
	  (propagate-neg-abstract-predicate apred)))
