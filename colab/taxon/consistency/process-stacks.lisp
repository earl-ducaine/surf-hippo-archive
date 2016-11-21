;*******************************************************************************
;*
;*	File : Process Stacks
;*
;*******************************************************************************


(defun process_main_stack ()
  (loop 
   ;;; while main-stack is not empty or no clash occurs 
   ;;; call functions, which are on the main-stack
   (cond ((empty-main-stack?)
	  (return t))
	 (t (funcall (pop-main)))
	 )))  

(defun process_or_stack ()
  (cond ((empty-or-stack?) 
	 ;;; abox has a model
	 '())
	(t (let ((disjunction (pop-or))
		 (old-or (get-or))
		 )
		;;; the first alternative is processed
		;;; the rest is stored on the trail
		(push-on-main! (first-alternative disjunction))
		(when (still-alternatives? disjunction)
		      (trail! (make-choice-point (rest-alternatives disjunction)))
		      (copy-trail! (make-choice-point (rest-alternatives disjunction))))
		(trail! (reset-or! old-or))
		(copy-trail! (reset-or! old-or))
		t
		))))

(defun process-trail ()
  (loop
   (let ((function (pop *trail))
	 )
	(cond ((null function) 
	       (return nil))		;;; consistency-check fails
	      ((choice-point? function)
	       (return function))	;;; another alternative is checked
	      (t (funcall function))    ;;; reset entries
	      ))))
