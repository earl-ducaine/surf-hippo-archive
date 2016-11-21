;*******************************************************************************
;*
;*	File : Propagate Types
;*
;*******************************************************************************

(defun propagate-all-abstract! (objs)
  (dolist (obj objs)
	  (propagate-abstract! obj)))
(defun propagate-abstract! (object)
  (let ((obj (deref object))
	)
       (cond ((concrete? obj)
	      (clash 'propagate-abstract! obj))
	     ((unknown? obj)
	      (bind-abstract! obj)
	      (trigger-abstract-functions obj)
	      (deref obj))
	     (t obj )
	     )))

(defun propagate-all-concrete! (objs domain)
  (dolist (obj objs)
	  (propagate-concrete! obj domain)))
(defun propagate-concrete! (object domain)
  (let ((obj (deref object))
	)
       (cond ((abstract? obj)
	      (clash 'propagate-concrete! obj))
	     ((unknown? obj)
	      (bind-concrete! obj domain)
	      (trigger-concrete-functions obj)
	      (deref obj))
	     ((eq domain (get-domain obj))
	      obj) 
	     (clash 'propagate-concrete! obj domain (get-domain obj))
	     )))
