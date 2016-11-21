
(defun filter-graph (graph filter)
  "applies the filter to the elements of the list in node-value
   returns a new graph and a hashtable from the elements to the node of the graph"
   (let ((hash (make-hash-table))
	 (new-graph (make-a-graph (make-node :value '(top))
				  (make-node :value '(bottom))
				  )
		    )
	 )
	(traverse-nodes-below 
	 (list (graph-top graph))
	 #'(lambda 
	    (node)
	    (dolist (concept (delete-if-not filter (node-value node)))
		    (multiple-value-bind
		     (new-node new?)
		     (sort-value-into-graph
		      #'(lambda (nd) 
				(concept-subsumption-look-up
				 (car (node-value nd))
				 concept))
		      #'(lambda (nd)
				(concept-subsumption-look-up
				 concept
				 (car (node-value nd))))
		      
		      (list concept)
		      new-graph
		      )
		     
		     (when (not new?) (pushnew concept (node-value new-node)))
		     )
		    )
	    )
	 )
	
	
	;set hash table
	(traverse-nodes-below
	 (list (graph-top new-graph))
	 #'(lambda (node)
		   (dolist (i (node-value node))
			   (setf (gethash i hash) node)
			   )
		   )
	 )
	
	
	;return
	(values new-graph hash)
	)
  )


(defvar *filtered-graph* NIL)
(setf *filtered-graph* *subsumption-graph*)
(defvar *filtered-nodes* (make-hash-table))

(defun filter-subsumption-graph ()
  (multiple-value-setq (*filtered-graph* *filtered-nodes*)
    (filter-graph *subsumption-graph*
                   #'(lambda (x) (member x *hierarchy-generators*)))
))


