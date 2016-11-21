(defun make-ilo-list (g)
  (let ((l nil)
	(da-bak *draw-all?)
	(db-bak *draw-bottom?)
	)
       (labels ((process-a-node 
		 (n)
		 ;(print (node-value n))
		 (declare (special *draw-test*))
		 (when (node-draw-test n)
		       (dolist (c (node-value n))
			       (push (cons
				      c 
				      (mapcan #'(lambda (n)
							(copy-list
							 (node-draw-value n)))
					      (im-lowers-passing-draw-test n))
				      )
				     l)
			       )
		       )
		 )
		)
	       
	       (setq *draw-all? nil)
	       (setq *draw-bottom? t)
	       (traverse-nodes-below (list (graph-top g)) #'process-a-node)
	       (setq *draw-all? da-bak)
	       (setq *draw-bottom? db-bak)
	       l
	       ))
  )


(defun save-subsumption (filename)
   (let ((stream (open filename :direction :output)))
      (print (reverse deflist) stream)
      (print (make-ilo-list *subsumption-graph*) stream)
      (close stream)))


(defvar subslist nil)

(defun load-subsumption (filename)
   (let ((stream (open filename :direction :input)))
     (mapcar #'eval (read stream))
     (setf subslist (read stream))
     (close stream)))

(defun listed-subsumption? (a b)
   (member b (cdr (assoc a subslist))))

