;*******************************************************************************
;*
;*	File : Backtracking
;*
;*******************************************************************************

(defun backtracking! ()
  (let ((choice-point (process-trail))
	)
       (cond (choice-point
	      (let ((old-or (get-or))
		    )
	      (clear-main)
	      (push-on-main! (first-alternative choice-point))
	      (when (still-alternatives? choice-point)
		    (trail! (make-choice-point (rest-alternatives choice-point)))
		    (copy-trail! (make-choice-point (rest-alternatives choice-point)))
		    )
	      (trail! (reset-or! old-or))
	      (copy-trail! (reset-or! old-or))
	      t
	      ))
	     )))

