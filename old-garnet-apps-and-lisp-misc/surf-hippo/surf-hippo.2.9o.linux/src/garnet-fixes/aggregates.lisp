;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

;; Redefining things which refer to the bbox structure....

(in-package "OPAL")

(define-method :add-component opal:aggregate (a-aggregate gob &rest args)
  (if (eq gob (g-local-value gob :window))		;; Is this a window?
      (error "*** ~A is a WINDOW, and was not added to ~A~%"
	     gob a-aggregate))
  (let ((parent (g-local-value gob :parent))
	(internally-parented (g-local-value gob :internally-parented)))
    (if (and parent (not internally-parented))
	;; The object already has a parent which wasn't assigned by
	;; aggregadgets
	(error "Graphical-object ~S has :parent ~S already." gob parent))
    (install-component a-aggregate gob args)
    ;; Set up the reverse pointer from child to aggregate
    (if internally-parented
	(destroy-slot gob :internally-parented)
	(s-value gob :parent a-aggregate)))

  ;; Propagate window and dirty bit to children
  (let ((a-window (g-local-value a-aggregate :window)))
    (when a-window
      (let ((gob-update-info (the UPDATE-INFO (g-local-value gob :update-info))))
	(set-display-slots gob a-window t)
	
	;; Place the object on its window's invalid-objects list
	(make-object-invalid gob gob-update-info a-window)
   
	;; Invalidate all of the aggregate's children (recursively)
	(if (update-info-aggregate-p gob-update-info)
	    (do-all-components gob
	      #'(lambda (c)
		  (let ((c-update-info (g-value c :update-info)))
		    (make-object-invalid c c-update-info a-window)))))

	;; Indicate that the old-bbox is not valid now since gob was
	;; not visible in this window previously...
	(setf (bbox-valid-p (update-info-old-bbox gob-update-info)) NIL))))

  ;; Signal we have changed components list
  (mark-as-changed a-aggregate :components)

  ;; Return gob
  gob)


;;; Remove-component deletes the topmost occurance of gob in aggregate
;;; 
(define-method :remove-component opal:aggregate (a-aggregate gob)

 (if (not (eq (g-local-value gob :parent) a-aggregate))
  (warn (format nil "Cannot remove-component ~A from ~A" gob a-aggregate))
  ;; add the gob's bounding box to the clipping region of the topmost
  ;; overlapping aggregate that contains it (or to its parent if no
  ;; such aggregate is found), and clear the gob's drawable, display,
  ;; and dirty slots, and recursively clear the same slots in its children
  ;; as well

  (let* ((gob-update-info (the UPDATE-INFO (g-local-value gob :update-info)))
         (a-window (update-info-window gob-update-info))
	 (a-window-update-info 
	    (when a-window (g-local-value a-window :update-info))))
    (when a-window-update-info
      (let* ((win-update-info (g-value a-window :win-update-info))
	     (window-bbox (update-info-old-bbox a-window-update-info))
	     (bbox (update-info-old-bbox gob-update-info)))

	; since the object is no longer in the window, it should be
	; removed from the window's invalid objects list.
	; (this is defined in update-basics.lisp).
	(if (update-info-aggregate-p gob-update-info)
	    (do-all-components gob #'(lambda (c)
				       (remove-from-invalid-objects-list
					c win-update-info))
			       :self T)
	    (remove-from-invalid-objects-list gob win-update-info))

	(merge-bbox window-bbox bbox)
        (set-display-slots gob nil nil)))

    (setf (update-info-invalid-p gob-update-info) NIL)
    (s-value a-aggregate :components
	     (delete gob (g-local-value a-aggregate :components)
		     :from-end t :count 1))
    (s-value gob :parent NIL)

    ;; signal we have changed components list
    (mark-as-changed a-aggregate :components))))

;;; Similar to a call to remove-component followed by a call to
;;; add-component, but faster because it does not call set-display-slots
;;; or perform any of the other overhead.  It merely sets the windows old-bbox,
;;; manually removes the object from the :components list, then reinstalls it

(define-method :move-component opal:aggregate (a-aggregate gob &rest args)
  (let* ((gob-update-info (g-local-value gob :update-info))
         (a-window (update-info-window gob-update-info))
	 (a-window-update-info (g-local-value a-window :update-info)))
    (and a-window a-window-update-info
      (let ((window-bbox (update-info-old-bbox a-window-update-info))
            (bbox (update-info-old-bbox gob-update-info)))
	(merge-bbox window-bbox bbox))))
  (s-value a-aggregate :components
	   (delete gob (g-local-value a-aggregate :components)))
  (install-component a-aggregate gob args))


;;; Remove all components
(defun remove-all-components (agg)
  (dolist (component (copy-list (g-local-value agg :components)))
    (remove-component agg component)))