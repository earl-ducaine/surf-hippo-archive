(in-package "OPAL")




;; 29.8.99 Noticed that the instances of a virtual agg's prototype are not destroyed when the parent
;; is. This seems to be one hack to do it.
(define-method :destroy-me opal:virtual-aggregate (gob &optional (top-level-p t))
  (DESTROY-ME-METHOD-VIEW-OBJECT (g-value gob :dummy-item)) ; Explicitly kill the :dummy-item.
  (call-prototype-method gob top-level-p))



#|
(define-method :destroy-me opal:virtual-aggregate (a-aggregate &optional (top-level-p t))
  (if a-aggregate
      
      (let* ((the-window (g-local-value a-aggregate :window))
             (erase-p (and top-level-p the-window))
             (parent  (g-local-value a-aggregate :parent))
             total-update-p)
	(format t "New Destroy me v-agg ~A~%" a-aggregate)
        (if erase-p;; If at top-level, then erase...
            (if (null parent)
                (if (eq a-aggregate (g-value the-window :aggregate))
                    (s-value the-window :aggregate NIL)
                    (progn
                      (format t "~%Warning in Destroy: aggregate '~A' has no parent,~%" a-aggregate)
                      (format t   "        is in window '~A', but is not that window's:aggregate.~%"
                              the-window)
                      (setq erase-p NIL)))
                (setq total-update-p (not (carefully-erase a-aggregate the-window)))))
        (when (and top-level-p parent)
          (s-value parent :components
		   (delete a-aggregate (g-local-value parent :components)))
          (mark-as-changed parent :components))
        (let ((components (g-local-value a-aggregate :components)))
          (with-constants-disabled (destroy-slot a-aggregate :components))
	  (format t "~A components ~A~%" a-aggregate components)
          (dolist (component components)
            (when (schema-p component)
              (destroy component NIL))))
        (destroy-schema a-aggregate)
        (if erase-p
            (update the-window total-update-p)))))

(define-method :destroy-me opal:aggregate (a-aggregate &optional (top-level-p T))
  (if a-aggregate
      (let* ((the-window (g-local-value a-aggregate :window))
             (erase-p (and top-level-p the-window))
             (parent  (g-local-value a-aggregate :parent))
             total-update-p)
        (if erase-p;; If at top-level, then erase...
            (if (null parent)
                (if (eq a-aggregate (g-value the-window :aggregate))
                    (s-value the-window :aggregate NIL)
                    (progn
                      (format t "~%Warning in Destroy: aggregate '~A' has no parent,~%" a-aggregate)
                      (format t   "        is in window '~A', but is not that window's:aggregate.~%"
                              the-window)
                      (setq erase-p NIL)))
                (setq total-update-p (not (carefully-erase a-aggregate the-window)))))
        (when (and top-level-p parent)
          (s-value parent :components
		   (delete a-aggregate (g-local-value parent :components)))
          (mark-as-changed parent :components))
        (let ((components (g-local-value a-aggregate :components)))
          (with-constants-disabled (destroy-slot a-aggregate :components))
;	  (format t "~A components ~A~%" a-aggregate components)
          (dolist (component components)
            (when (schema-p component)
              (destroy component NIL))))
        (destroy-schema a-aggregate)
        (if erase-p
            (update the-window total-update-p)))))
|#