;;; 10-07-92 aa


(defun destroy-taxon ()
  (clear-abox)
  (setf *subsumption-graph* (make-a-graph (make-node :value '(top))
					  (make-node :value '(bottom))))
  (clear-tbox)
  (setf *filtered-graph* *subsumption-graph*)
  (setf *classify-list* nil)
  (setf *separate-classes* (make-hash-table))
  T
  )

(defmacro sati-co? (co)
  `(fsati-co? ',co))

(defun fsati-co? (co)
  (let ((nc (get-CNF-of-conceptname co))
	(abox *abox-table*)
	)
       (clear-abox)
       (fasse (list co 'dummy))
       (let ((result (check-abox))
	     )
	    (setf *abox-table* abox)
	    result
	    )))


(defmacro drawhi (&key (name "hierarchy.ps")
		       (top 'top)
		       (shrink nil)
		       (insert nil)
		       )
  `(fdrawhi :name ',name :top ',top :shrink ',shrink :insert ',insert))

(defun fdrawhi (&key (name "hierarchy.ps")
		     (top 'top)
		     (shrink nil)
		     (insert nil))
  (princ (format nil "Drawing (sub)hierarchy starting with ~A to file ~A~%" top name)
	 )
  (draw-subsumption-graph-to-file
   :name name
   :shrink shrink
   :insert insert
   :test #'equal
   :name (get-node-of-item top)
   )
  (princ (format nil "Wrote ~A~%" (truename name)))
  )

