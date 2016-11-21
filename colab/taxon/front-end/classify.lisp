#|
	@(#)classify.lsp	3.6
	9/7/92
|#


#|
A new concept C is classified
we determine 1) the immediate uppers
	     2) the immediate lowers
we use insert
that's it.
|#

#|(defun value-classify (concept graph)
  (let* ((iu (compute-iu concept graph))
	 ;(xb (print (format nil "iu ~A" iu)))
	 (il (compute-il concept graph iu))
	 ;(xa (print (format nil "il ~A" il)))
	 (ul (intersection iu il))
	 (new-node (make-node :value concept))
	 ;(print (format nil "il&iu ~A" ul))
	 )
	(cond (ul (values (car ul) nil)); concept is already in graph
	      (t (node-insert new-node iu il)
		 (values new-node t)))
	)
  )
|#

(defun value-insert (uppers lowers values)
  "inserts a new-node between the nodes uppers and lowers with value values
  and returns the new node"
  (let ((new-node (make-node :value values)))
       (node-insert new-node uppers lowers)
       new-node))

(defun sort-lower-primitive-value-into-graph (upper-test value graph)
  "insert value into graph using upper-test to compute imediate uppers
  assuming bottom as the only imediate lower
  returns two values 1) the node 2) nil if the value was in the graph"
  (let ((iu (compute-iu upper-test graph)))
       (if (eq (car iu) (graph-bottom graph))
	   ;value is equivalent to bottom and thus already in the graph
	   (values (car iu) nil)
	   ;it is new
	   (let ((new-node (make-node :value value)))
		(node-insert
		 new-node
		 iu
		 (list (graph-bottom graph))
		 )
		(values new-node t)
		)
	   )
       )
  )

(defun sort-primitive-value-into-graph (graph value)
  "insert a value for which it is known that it is not yet in the graph
  and has top and bottom as immediate upper and lower, respectively.
  Returns the node "
  (let ((new-node (make-node :value value)))
       (node-insert 
	new-node
	(list (graph-top graph))
	(list (graph-bottom graph))
	)
       new-node
       )
  )

(defun value-classify (concept graph)
  (sort-value-into-graph
   #'(lambda (nd) (value-subsumes (car (node-value nd)) concept))
   #'(lambda (nd) (value-subsumes concept (car (node-value nd))))
   concept
   graph
   )
  )

(defun sort-value-into-graph (upper-test lower-test value graph)
  "Determines the position of value in the graph and inserts it into
  graph if necessary. Returns 1. the node for value 2. nil if a value
  equivalent to value has already been inserted"
  
  (multiple-value-bind
   (iu il ul)
   (compute-position-in-graph upper-test
			      lower-test graph)
   (cond (ul (values ul nil)); concept is already in graph 
	 (t (let ((new-node (make-node :value value)))
		 (node-insert new-node iu il) 
		 (values new-node t))
	    )
	 )
   )
  )

(defun compute-position-in-graph (upper-test lower-test graph)
  "returns three values
  1. imediate uppers (computed with upper-test) 
  2. immediate lowers (computed with lower-test)
  3. nil (iff not already in graph otherwise the node) "
  (let* ((iu (compute-iu upper-test graph))
	 (il (compute-il lower-test graph iu))
	 (ul (intersection iu il))
	 )
	(values iu il (if ul (car ul) nil))
	)
  )






#|
for each node i at time t we know
i > C,
not (i > C) or
nothing
This is stored in a hash-table using 
'passed
'failed
nil
A node may also be in the current "front". This is indicated by
'in-front.
|#

(defun passed? (x) (eq x 'passed))
(defun failed? (x) (eq x 'failed))
(defun my-unknown? (x) (null x))
(defun in-front? (x) (eq x 'in-front))


(defun compute-msn (nodes test)
  "compute most specifc nodes starting from nodes that pass the test"
  ;(declare (type (list node-p) nodes))
  ;(declare (ftype (function (node) t) test))
  (compute-frontier
   test
   'node-lowers
   'node-uppers
   (mapcan #'(lambda (x)
		     (if (funcall test x) (list x)
			 nil))
	   nodes)
   )
  )

(defun compute-iu (test g)
  (compute-frontier
   test
   'node-lowers
   'node-uppers
   (list (graph-top g))))


(defun compute-il-interesting (test g count)
  (compute-frontier
   test
   #'(lambda (nd)
	     (mapcan #'(lambda (n) 
			       (if (eql (gethash n count) 0)
				   (list n)
				   nil))
		     (node-uppers nd)))
   #'(lambda (nd)
	     (mapcan #'(lambda (n) 
			       (if (eql (gethash n count) 0)
				   (list n)
				   nil))
		     (node-lowers nd)))
   (list (graph-bottom g))
   )
  )



(defun compute-frontier (test forward backward front)
  (let* ((tb (make-hash-table)
	     )
	 )
	(declare (type hash-table tb))
	(labels 
	 ((compute-state
	   (nd)
	   ;(declare (type node nd))
	   (let ((state (gethash nd tb)))
		(cond ((my-unknown? state) 
		       ;check states of uppers
		       (setf (gethash nd tb)
			     (prog ((i (funcall backward nd))
				    st
				    )
				   (declare (type list i))
				   loop 
				   (unless i 
					   (return 
					    (if (funcall test nd) 
						'passed 
						'failed)
					    )
					   )
				   (setq st (compute-state (pop i)))
				   (cond ((failed? st)
					  (return 'failed)
					  )
					 (t ))
				   (go loop)
				   
				   )
			     )
		       )
		      ((failed? state) 'failed)
		      ((in-front? state) 'in-front)
		      ((passed? state) 'passed)
		      (t (break))))
	   )
	  
	  (passed-forward
	   (nd)
	   ;(declare (type node nd))
	   (prog ((nd+ (funcall forward nd))
		  ps
		  i flag
		  )
		 (declare (type list nd+ ps))
		 loop
		 (unless nd+ (return (values ps flag))
			 )
		 (let* ((i (pop nd+))
			(st (compute-state i))
			)
		       ;(declare (type node i))
		       (cond ((passed? st) (push i ps))
			     ((in-front? st) (setq flag t))
			     (t nil))
		       )
		 (go loop)
		 )
	   )
	  )
	 
	 
	 ;all elements of front are  in the front 
	 (dolist (i front)
		 (setf (gethash i tb) 'in-front)
		 )
	 (prog (iu ps)
	       (declare (type list iu ps))
	       loop
	       (unless front (return iu)
		       )
	       (let ((cur-nd (pop front)))
		    ;(declare (type node cur-nd))
		    (multiple-value-bind
		     (ps flag)
		     (passed-forward cur-nd)
		     (cond 
		      (ps (setf (gethash cur-nd tb) 'passed)
			  (dolist (i ps)
				  (push i front)
				  (setf (gethash i tb) 'in-front)))
		      (t 
		       (if flag
			   (setf (gethash cur-nd tb) `passed); no longer in-front
			   (push cur-nd iu))
		       )))
		    )
	       (go loop)
	       )
	 )
	)
  )



(defun compute-il (test g iu)
  (declare (type graph g))
  (declare (type list iu))
  (let ((count (make-hash-table))
	(l (- (length iu) 1)))
       (declare (type (integer 0 1000) l))
       (traverse-nodes-below
	(list (pop iu))
	#'(lambda (x) (setf (gethash x count) l)))
       (dolist (i iu)
	       ;(declare (type node i))
	       (traverse-nodes-below
		(list i)
		#'(lambda (x) 
			  (if (numberp (gethash x count))
			      (decf (gethash x count))))))
       ;intersting nodes have count 0 now
       
       ;proceed dualy as in compute-ul on interesting nodes
       (compute-il-interesting test g count)
       )
  )

