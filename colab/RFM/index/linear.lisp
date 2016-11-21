(defun flatten-idx (idxtree)
  (let* ((tag (iif.tag-of-idxtree idxtree)))
	(cond ((equal tag 'clauses) nil)
	      ((equal tag 'label) nil)
	      ((equal tag 'try-trust)
	       (cons
		(list
		 (iif.s-clauses-f-idxtree idxtree)
		 (list 'label (string (gentemp "label")))
		 idxtree)
		(mapcan2 #'flatten-idx
		         (iif.s-idxtrees-f-try-trust idxtree))))
	      ((equal tag 'indextree)
	       (cons
		(list
		 (iif.s-clauses-f-idxtree idxtree)
		 (list 'label (string (gentemp "label")))
		 idxtree)
		(mapcan2
		 #'(lambda (x)
			   (if (iif.if-s.o.?.-sequindpart x)
			       (mapcan2
				#'(lambda (y)
					  (flatten-idx
					   (iif.s-idxtree-f-switchpart y)
					   ))
				(iif.s-switchparts-f-sequindpart x))
			       (flatten-idx
				(iif.s-idxtree-f-sequindpart x)
				)))
		 (iif.s-sequindparts-f-indextree idxtree))))
	      ) ; cond
	) ; let
  ) ; defun

(defun iif.s-idxtree-f-indextree (indextree)
  (third indextree))

(defun linearize (lab+idxtree list-of-idxtrees)
  (if lab+idxtree
  (let* ((lab (iif.s-lab-f-idxtree lab+idxtree))
	 (idxtree (iif.s-idxtree-f-indextree lab+idxtree))	
	 (tag (iif.tag-of-idxtree idxtree)))
	(cond ((equal tag 'clauses) nil)
	      ((equal tag 'label) nil)
	      ((equal tag 'try-trust)
	       (cons
		(append
		 lab
		 (list
		 (list
		 (iif.s-clauses-f-idxtree idxtree)
		 (cons 'try-trust
		       (lin.search-try-trust-labels idxtree
						    list-of-idxtrees)))))
		(mapcan2
		 #'(lambda (x) (linearize x list-of-idxtrees))
		 (lin.search-idxtrees-f-try-trust idxtree
						  list-of-idxtrees))))
	      ((equal tag 'indextree)
	       (cons
		(append
		 lab
		  (list
		  (list
		  (iif.s-clauses-f-idxtree idxtree)
		  (append
		   (list 'indextree (iif.s-arg-f-indextree idxtree))
		  (lin.search-indextree-labels idxtree 
					       list-of-idxtrees)))))
		(mapcan2 #'(lambda (x)
				  (linearize
				   (lin.s-a-label x list-of-idxtrees)
				   list-of-idxtrees))
			(lin.s-indextrees-f-idxtree idxtree))))
	      ) ; cond
	) ; let
      ) ; if
  ) ; defun

(defun lin.unique (list-o-idxtrees)
  (mapcon2 #'(lambda (x) (if (member (car x)
				     (cdr x)
				     :test #'equal)
			     nil
			     (list (car x))))
	   list-o-idxtrees))

(defun lin.s-label-f-idxtree (idxtree)
  (list (first idxtree) (second idxtree)))
; (first idxtree))

(defun lin.cut-down-next-one (list-o-idxtrees idxtree max-args max-depth)
  (let ((tag (iif.tag-of-idxtree idxtree)))
       (cond ((equal tag 'clauses) nil)
	     ((equal tag 'label+idxtree) (first idxtree))
	     ((equal tag 'label) 
	      (lin.cut-down list-o-idxtrees idxtree
			    max-args max-depth))
	     (t (progn (rf-print "error in lin")
		       (break)
		       nil)))))

(defun lin.find-label (label list-o-idxtrees)
  (first
   (member label list-o-idxtrees
	   :test #'(lambda (x y)
			   (equal x
				  (lin.s-label-f-idxtree y))))))

(defun lin.cut-down (list-o-idxtrees next-label max-args max-depth)
  (if (or (= 0 max-args)
	  (= 0 max-depth))
      nil
      (if (not (null list-o-idxtrees))
      (let ((next (lin.find-label next-label list-o-idxtrees)))
	   (if (null next)
	       (error "cut-down: unknown label")
	       (let* ((nextidx (caddr next))
		      (tag (iif.tag-of-idxtree nextidx)))
		     (cond ((or (equal tag 'clauses) 
				(equal tag 'label))
			    (progn (rf-print "error in linearize")
				   nil))
			   ((equal tag 'try-trust)
			    (cons next
				  (mapcan2 
				   #'(lambda (x)
					     (lin.cut-down-next-one
					      list-o-idxtrees
					      x
					      max-args
					      max-depth))
				   (iif.s-idxtrees-f-try-trust nextidx))))
			   ((equal tag 'indextree)
			    (cons
			     next
			     (mapcan2
			      #'(lambda
				 (x)
				 (let* ((vars?
					 (equal '(var)
						(iif.s-typetag-f-sequind
						 x)))
					(depth (if vars?
						   max-depth
						   (1- max-depth)))
					(args (if vars?
						  (1- max-args)
						  max-args)))
				;
				(if (iif.if-s.o.?.-sequindpart x)
				    (mapcan2
				     #'(lambda
					(y)
					(lin.cut-down-next-one
					 list-o-idxtrees
					 (iif.s-idxtree-f-switchpart y)
					 args 
					 depth))
				     (iif.s-switchparts-f-sequindpart x))
				    (lin.cut-down-next-one
				     list-o-idxtrees
				     (iif.s-idxtree-f-sequindpart x)
				     args
				     depth))
				       ) ; let
				 ) ; lambda
			      (iif.s-sequindparts-f-indextree
			       nextidx)))))))))))

(defun lin.mk-try-trust-label-f-label (label)
  (cond ((equal (iif.tag-of-idxtree label) 'clauses) nil)
	((equal (iif.tag-of-idxtree label) 'label) label)
	(t 
      (list (first label)
	    (list 'label (string (gentemp "label")))))))
;		  (list 'try-trust (first label))))))

(defun iif.sub-label (idxtree idxtrees list-o-all-idxtrees)
  (cond ((null idxtrees) idxtree)
	((let ((tag (iif.tag-of-idxtree (first idxtrees))))
	      (or (equal tag 'clauses)
		  (and (equal tag 'label)
		       (lin.find-label (first idxtrees) list-o-all-idxtrees))))
	 (iif.sub-label idxtree
			(cdr idxtrees)
			list-o-all-idxtrees))
	(t
	 (iif.sub-label (subst (lin.mk-try-trust-label-f-label
				(first idxtrees))
			       (first idxtrees)
			       idxtree
			       :test #'equal)
			(cdr idxtrees)
			list-o-all-idxtrees))))

(defun lin.insert-t-t (list-o-idxtrees)
  (mapcan2 #'(lambda (x)
		     (lin.insert-try-trust list-o-idxtrees
					   (lin.s-label-f-idxtree x)))
	   list-o-idxtrees))

(defun lin.insert-try-trust (list-o-idxtrees next-label)
  (let ((next (lin.find-label next-label list-o-idxtrees)))
       (if (null next)
	   (let ((tt (lin.mk-try-trust-label-f-label next-label)))
		(if (not (null tt))
		    (list 
		     (append
		      tt
		      (list
		       (list
		      (first next-label)
		      (cons 'try-trust (iif.mapindex (first
						      next-label)))))))))
	   (let* ((nextidx (caddr next))
		  (tag (iif.tag-of-idxtree nextidx)))
		 (cond ((or (equal tag 'clauses)
			    (equal tag 'label))
			(progn (rf-print "error in insert-try-trust")
			       nil))
		       ((equal tag 'try-trust)
			(let ((next-trees (iif.s-idxtrees-f-try-trust nextidx)))
			     (cons
			      (iif.sub-label next next-trees list-o-idxtrees)
; after linearize a recursion
; is not possible
;			      (mapcan2
;			       #'(lambda
;				  (x)
;				  (lin.insert-try-trust list-o-idxtrees x))
;			       next-trees)
			      nil
			      )))
		       ((equal tag 'indextree)
			(let ((next-trees
			       (mapcan2
				#'(lambda
				   (x)
				   (if (iif.if-s.o.?.-sequindpart x)
				       (mapcar
					#'iif.s-idxtree-f-switchpart
					(iif.s-switchparts-f-sequindpart x))
				       (list
					(iif.s-idxtree-f-sequindpart x))))
				(iif.s-sequindparts-f-indextree nextidx))))
			     (cons
			      (iif.sub-label next next-trees list-o-idxtrees)
			      (mapcan2
			       #'(lambda
				  (x)
				  (lin.insert-try-trust list-o-idxtrees x))
			       next-trees)
			      ))))))))

(defun iif.mk-label-f-idxtree (idxtree)
  (let ((tag (iif.tag-of-idxtree idxtree))
	(clauses (iif.s-clauses-f-idxtree idxtree)))
       (cond ((equal tag 'clauses) idxtree)
	     ((equal tag 'label) idxtree)
	     ((equal tag 'label+idxtree) (list (first idxtree) 
					       (second idxtree)))
	     (t (list (iif.s-clauses-f-idxtree idxtree)
		      (list 'label (string (gentemp "label"))))))))
	   
#|
	     ((equal tag 'try-trust)
	      (list clauses
		    (list 'label
			  (list tag clauses))))
	     ((equal tag 'indextree)
	      (list clauses
		    (list 'label
			  (list tag
				(iif.s-arg-f-indextree idxtree)
				clauses)))))))
|#

(defun lin.search-try-trust-labels (idxtree list-o-idxtrees)
  (let ((found (lin.search-idxtrees-f-try-trust idxtree
						list-o-idxtrees)))
       (mapcar #'(lambda (x y)
			  (if x (iif.s-lab-f-idxtree x)
				y))
		found
		(iif.s-idxtrees-f-try-trust idxtree))))

(defun lin.search-idxtrees-f-try-trust (idxtree list-o-idxtrees)
  (mapcar #'(lambda (x)
		    (lin.s-a-label x list-o-idxtrees))
	   (iif.s-idxtrees-f-try-trust idxtree)))

(defun lin.s-a-label (idxtree list-o-idxtrees)
  (first
   (member idxtree list-o-idxtrees
	   :test #'(lambda (x y)
			   (equal x (third y))))))

(defun lin.s-a-label-if-found (idxtree list-o-idxtrees)
  (let ((found (lin.s-a-label idxtree list-o-idxtrees)))
       (if found found idxtree)))

(defun lin.search-indextree-labels (idxtree list-o-idxtrees)
  (mapcar #'(lambda (x)
                    (append
                     (iif.s-typetag-f-sequind x)
                     (list
                      (if (iif.if-s.o.?.-sequindpart x)
                          (mapcar
                           #'(lambda (y)
                                     (list (iif.s-atom-f-switchpart y)
					   (iif.s-lab-f-idxtree
                                           (lin.s-a-label-if-found
                                            (iif.s-idxtree-f-switchpart y)
					   list-o-idxtrees))))
                           (iif.s-switchparts-f-sequindpart x))
			  (iif.s-lab-f-idxtree
                          (lin.s-a-label-if-found
                           (iif.s-idxtree-f-sequindpart x)
			  list-o-idxtrees))))))
          (iif.s-sequindparts-f-indextree idxtree)))

(defun lin.s-indextrees-f-idxtree (idxtree)
  (mapcan2 #'(lambda (x)
		      (if (iif.if-s.o.?.-sequindpart x)
			  (mapcar
			   #'iif.s-idxtree-f-switchpart 
			   (iif.s-switchparts-f-sequindpart x))
			  (list
			   (iif.s-idxtree-f-sequindpart x))))
	  (iif.s-sequindparts-f-indextree idxtree)) )

(defun iif.s-typetag-f-sequind (sequind)
  (if (iif.if-s.o.?.-sequindpart sequind)
      (list (first sequind)
	    (second sequind))
      (list (first sequind))))

