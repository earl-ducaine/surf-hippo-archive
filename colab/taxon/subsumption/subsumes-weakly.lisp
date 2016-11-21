;*******************************************************************************
;*
;*	File : Subsumes-Weakly -----------> DD 04.08.92
;*
;*******************************************************************************

(defun subsumes-weakly? (cnf1 cnf2)
  (let ((result t)
	)
       (if (get-cnf-agree cnf2)
	   'unknown
	   (dolist (subs-function (get-subsumption-functions cnf1) result)
		   (setq res (funcall subs-function cnf2))
		   (cond ((null res) 
			  (setq result nil)
			  (return))
			 ((eq 'unknown res)
			  (setq result 'unknown))))
	   )))

;;; test function

(defun sw? (cn1 cn2)
  (subsumes-weakly? (get-cnf-of-conceptname cn1) (get-cnf-of-conceptname cn2)))
