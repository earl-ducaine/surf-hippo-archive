#|
 	@(#)graph.lsp	2.3
 	7/22/92

	2.0: node-above test
|#

(defmacro assertions (&rest l-of-a)
  ;`(if (not ,(cons 'and l-of-a)) (break)))
  )

(defstruct (node ;(:print-function print-node)
		; (:type list)
		 )
	mark
	(depth 0 :type (integer 0 1000))
	    ;the length of the longest path from top to the node
	(uppers nil :type list)
	(lowers nil :type list)
	value)
(defun print-node (p s k)
    (format s "#~A:~A:~A"  (type-of p) (node-value p) (node-depth p))
    )

(defstruct (graph (:print-function print-graph))
	(name "a graph")
	top 
	bottom
	(mark-level 0))
(defun print-graph (p s k)
  (format s  "#~A" (list (type-of p) (graph-name p))))

(defun make-a-graph (to bo)
  ;(ASSERTIONS (node-p to) (node-p bo))
  (let ((g (make-graph :top to :bottom bo)))
       (setf (node-depth to) 0)
       (node-insert bo (list to) nil)
       g))

(defun get-new-mark (g)
  (cond ((< 100000 (graph-mark-level g))
	 (traverse-nodes-below (list (graph-top g))
			       '(lambda (nd) (setf (node-mark nd) nil)))
	 )
	(t (incf (graph-mark-level g))))
  )
  
(defun node-above (g a b)
  ;(declare (type node a b))
  ;(ASSERTIONS (node-p a) (node-p b))
  (let ((depth (node-depth a))
	(mark (get-new-mark g)))
       (declare (type (integer 0 1000) depth))
       (labels ((search-a (x)
			  ;(declare (type node x))
			  ;(incf *steps-i)
			  ;(print (format nil "~A ~A" x depth))
			  ;(princ #\.)
			  (cond ((eql mark (node-mark x)) nil)
				((> (node-depth x) depth)
				 (setf (node-mark x) mark)
				 (prog ((xs (node-uppers x)))
				       (declare (type list xs))
				       loop
				       ;(print (format nil "xs ~A" xs))
				       (unless xs (return nil))
				       (if (search-a (pop xs))
					   (return t)
					   (go loop)))
				 )
				((= (node-depth x) depth)
				 (eq a x))
				(t nil))
			  ))
	       ;(terpri)
	       (search-a b)
	       )
       )
  )


(defun naive-node-above (a b)
  ;(ASSERTIONS (node-p a) (node-p b))
  (let ((depth (node-depth a))
        (visited (make-hash-table)))
       (labels ((search-a (x)
                          (incf *steps-i)
			  ;(print (format nil "~A ~A" x depth))
			  ;(princ #\.)
			  (cond ((gethash x visited) nil)
				((eq a x)
				 (setf (gethash x visited) t))
				(t 
				 (setf (gethash x visited) t)
				 (prog ((xs (node-uppers x)))
				       loop
				       ;(print (format nil "xs ~A" xs))
				       (unless xs (return nil))
				       (if (search-a (pop xs))
					   (return t)
					   (go loop)))
				 )
				)
			  )
		)
	       ;(terpri)
	       (let ((r (search-a b)))
		    r)))
  )

(defun get-next-that-pass-test (graph nodes next test)
  (labels ((work (nodes)
		 (mapcan 
		  #'(lambda (node)
			    (if (funcall test node)
				(list node)
				(work (funcall next node))))
		  nodes)))
	  (let ((l (remove-duplicates (work (mapcan next nodes))))
		(out nil))
	       (dolist (i l)
		       (unless (member-if
				; there is one above i
				#'(lambda (n)
					  (and
					   (not (eq n i))
					   (node-above graph n i)))
				l)
			       (push i out)
			       )
		       )
	       out
	       )
	  )
  )

(defun node-immediate-uppers-if (graph n test) 
  (get-next-that-pass-test graph (list n) 'node-uppers test))
(defun node-immediate-lowers-if (graph n test)
  (get-next-that-pass-test graph (list n) 'node-lowers test))
(defun node-immediate-uppers (n) (node-uppers n))
(defun node-immediate-lowers (n) (node-lowers n))
(defun nodes-all-lowers (nodes) 
  (let ((basket nil))
       (traverse-nodes
	nodes
	'node-lowers
	#'(lambda (node) (push node basket)))
       basket))
(defun nodes-all-uppers (nodes) 
  (let ((basket nil))
       (traverse-nodes
	nodes
	'node-uppers
	#'(lambda (node) (push node basket)))
       basket))


(defun node-insert (node uppers lowers)
  ;(declare (type node node))
  (declare (type list uppers))
  (declare (type list lowers))
  ;(ASSERTIONS
   ;(node-p node) 
   ;(listp uppers)
   ;(listp lowers))
  (prog ((depth 0))
	(declare (type (integer 0 1000) depth))
	(dolist (i uppers)
		(when (> (node-depth i) depth)
		      (setq depth (node-depth i)))
		(push node (node-lowers i))
		(setf (node-lowers i)
		      (set-difference (node-lowers i) lowers)))
	(setf (node-uppers node) uppers
	      )
	
	
	(labels ((update-depth (nd d)
			       ;(declare (type node nd))
			       ;(declare (type (integer 0 1000) nd))
			       (unless (> (node-depth nd) (incf d))
				       (setf (node-depth nd) d)
				       (dolist (i (node-lowers nd))
					       (update-depth i d)))
			       )
		 )
		(setf (node-lowers node) lowers)
		(dolist (i lowers)
			(push node (node-uppers i))
			(setf (node-uppers i)
			      (set-difference (node-uppers i) uppers))
			)
		(setf (node-depth node) depth)
		(update-depth node depth))
	)
  )

(defun traverse-nodes (nodes next-fn f &optional (visited nil))
  ;(declare (type list nodes))
  ;(declare (ftype (function (node) list) next-fn)) 
  ;(declare (ftype (function (node) t) f)) 
  ;(ASSERTIONS (listp nodes)
	      ;(functionp next-fn)
	      ;(functionp f)
	      ;(or (null visited)(hash-table-p visited)))
  (let ((visited (if visited visited (make-hash-table))))
       (labels ((work (node)
		      (unless (gethash node visited)
			      (setf (gethash node visited) t)
			      (funcall f node)
			      (dolist (i (funcall next-fn node))
				      (work i)))
		      ))
	       (dolist (i nodes)
		       (work i)))
       visited))

(defun traverse-nodes-above (nodes f &optional (visited nil))
  (traverse-nodes nodes 'node-uppers f visited))

(defun traverse-nodes-below (nodes f &optional (visited nil))
  (traverse-nodes nodes 'node-lowers f visited))

#|
(defun print-whole-graph (g)
  (labels ((print-a-node (n)
			 (print (format nil
					"~A ---> ~A"
					(node-value n)
					(mapcar
					 'node-value
					 (node-lowers n))
					))))
	  ;(print (functionp 'print-a-node))
	  (traverse-nodes-below (list (graph-top g)) #'print-a-node)
	  ))

|#
