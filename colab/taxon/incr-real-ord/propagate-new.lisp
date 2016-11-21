(defun propagate-new-> (obj vector ub)
  (declare (special *index))
  (set-*> vector obj)
  (if ub 
	(set-ub (make-< (nr-of-b ub)) obj))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end))
      (cond ((set? counter vector)
	     (trail! (make-function 'reset-bit! 
				    *index 
				    (real-ord-obj-*< (get-obj counter))))
	     (set-bit! *index (real-ord-obj-*< (get-obj counter))))
	    )))

(defun propagate-new-< (obj vector lb)
  (declare (special *index))
  (set-*< vector obj)
  (if lb
	(set-lb (make-> (nr-of-b lb)) obj))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end) )
      (cond ((set? counter vector)
	     (trail! (make-function 'reset-bit! 
				    *index
				    (real-ord-obj-*> (get-obj counter))))
	     (set-bit! *index (real-ord-obj-*> (get-obj counter))))
	    )))

(defun propagate-new->= (obj vector ub)
  (declare (special *index))
  (set-*>= vector obj)
  (if ub 
	(set-ub ub obj))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end) )
      (cond ((set? counter vector)
	     (trail! (make-function 'reset-bit!
				    *index
				    (real-ord-obj-*<= (get-obj counter))))
	     (set-bit! *index (real-ord-obj-*<= (get-obj counter))))
	    )))

(defun propagate-new-<= (obj vector lb)
  (declare (special *index))
  (set-*<= vector obj)
  (if lb 
	(set-lb lb obj))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end) )
      (cond ((set? counter vector)
	     (trail! (make-function 'reset-bit!
				    *index
				    (real-ord-obj-*>= (get-obj counter))))
	     (set-bit! *index (real-ord-obj-*>= (get-obj counter))))
            )))
