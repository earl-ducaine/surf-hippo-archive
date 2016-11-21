(setq psgraph:*extra-y-spacing* 12)
(defun draw-subsumption-graph-to-file
  ( &key
   (name "hierachy.ps")
   (shrink nil) (insert nil) (test #'equal)
   (node (graph-top *SUBSUMPTION-GRAPH*)))
  (with-open-file (*standard-output* name
				     :direction
				     :output
				     :if-exists
				     :supersede)
		  (psgraph:psgraph
		   node
		   'im-lowers-passing-draw-test
		   'draw-info
		   shrink
		   insert
		   test)
		  )
  )

(defun escape-chars (str char-list escape-char)
  (coerce
   (mapcan #'(lambda (char)
		     (if (member char char-list)
			 (list escape-char char) 
			 (list char)))
	   (coerce str 'list))
   'string)
  )

#|
(defvar *draw-test* 'user-defined-concept?)
(defun true-fn (x) t)
(defun draw-all () (setq *draw-test* 'true-fn))
(defun draw-user-defined () (setq *draw-test* 'user-defined-concept?)) 
|#

(defvar *draw-test* #'(lambda (x)
    (if *draw-all?
        T
        (if *draw-bottom?
            (user-defined-concept? x)
            (and (user-defined-concept? x) (not (eq 'bottom x)))))))


(defun  node-draw-test (node)
  (member-if *draw-test* (node-value node)))
(defun node-draw-value (node)
  (remove-if-not *draw-test* (node-value node)))

(defun  im-lowers-passing-draw-test (node)
  (node-immediate-lowers-if *SUBSUMPTION-GRAPH* node 'node-draw-test))

(defun draw-info (node)
  (let ((v (node-draw-value node)))
       (if (> (length v) 1)
	   (list (escape-chars (princ-to-string (car v)) '(#\( #\) #\\ #\ ) #\\)
		 (escape-chars (princ-to-string (cdr v)) '(#\( #\) #\\ #\ ) #\\)
		 )
	   (list (escape-chars (princ-to-string (car v)) '(#\( #\) #\\ #\ ) #\\)
		 )
	   )
       )
  )

(defun print-whole-graph (g)
  (labels ((print-a-node 
	    (n)
	    (let ((father (node-draw-value n))
		  (children (mapcar
			     'node-draw-value
			     ;#'(lambda (node)
				       ;(let ((v (node-draw-value node)))
					    ;(if (> (length v) 1)
						;v (car v))))
			     (im-lowers-passing-draw-test n))))
		 (when children 
		       (princ 
			(concatenate
			 'string
			 (remove #\(
				    (remove #\)
				    (format nil
					    "~20@A <=== " father)))
			 (let ((st (princ-to-string children)))
			      (subseq st 1 (- (length st) 1)))
			 (format nil "~%")
			 )
			)
		       )
		 )
	    )
	   )
	  (traverse-nodes (list (graph-top g)) 
			  'im-lowers-passing-draw-test 
			  #'print-a-node)
	  (terpri))
  )

