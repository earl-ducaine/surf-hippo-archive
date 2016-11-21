(defun icg.mk-header (idxtree x1579)
  (let ((tag (iif.tag-of-idxtree idxtree)))
       (cond ((equal tag 'clauses) nil)
	     ((equal tag 'label) nil)
	     ((equal tag 'label+idxtree)
	      (cons
	       (icg.mk-symbol-label-f-idxtree idxtree)
	       (icg.mk-header (caddr idxtree) x1579)))
	     ((equal tag 'try-trust)
	      (icg.mk-t-r-t-list-f-idxtrees
	       (iif.s-idxtrees-f-try-trust idxtree) x1579))
	     ((equal tag 'indextree)
	      (icg.mk-s.o.t.-f-indextree idxtree x1579))
	     (t (error "falscher idxtree :icg.mk-header")))))

(defun icg.mk-t-r-t-list-f-idxtrees (idxtrees x1579)
  (unless (null idxtrees)
	  (append (unless (null (cdr idxtrees))
			  (append
			   (icg.gen-t-r-t (first idxtrees)
					  x1579
					  'try)
			   (mapcon2
			    #'(lambda
			       (x)
			       (icg.gen-t-r-t (first x)
					      x1579
					      (if (null (cdr x))
						  'trust
						  'retry)))
			    (cdr idxtrees))))
;		  (mapcon2
;		   #'(lambda (x) (icg.mk-header x x1579))
;		   idxtrees)
		   )))

(defun icg.gen-t-r-t (idxtree x1579 tag)
  (list 
   (list tag
	 (icg.mk-symbol-label-f-idxtree idxtree)
	 x1579)))

(defun icg.mk-s.o.t.-f-indextree (idxtree x1579)
  (let ((partlabels
	 (mapcar 
	  #'(lambda (x)
		    (if (iif.if-s.o.?.-sequindpart x)
			(icg.mk-symbol-label-f-idxtree idxtree)
			(icg.mk-symbol-label-f-idxtree
			 (iif.s-idxtree-f-sequindpart x)))
			)
	  (iif.s-sequindparts-f-indextree idxtree))))
       (append
	(list
	 (cons 'set_index_number 
	       (iif.s-arg-f-indextree idxtree))
	 (cons 'switch_on_term
	       partlabels))
	(mapcan2
	 #'(lambda (x y)
		   (if (iif.if-s.o.?.-sequindpart y)
		       (list x (icg.gen_switch_on_? y))
		       nil))
	 partlabels
	 (iif.s-sequindparts-f-indextree idxtree)))))

(defun icg.gen_switch_on_? (sequind)
 (let* ((switchtag (second (iif.s-typetag-f-sequind sequind)))
	(switchlabels (mapcar 
		       #'(lambda (x)
				 (icg.mk-symbol-label-f-idxtree
				  (iif.s-idxtree-f-switchpart x)))
		       (iif.s-switchparts-f-sequindpart sequind)))
	(switches (mapcar #'(lambda (x y)
				    (list
				     (iif.s-atom-f-switchpart y)
				     x))
			  switchlabels
			  (iif.s-switchparts-f-sequindpart sequind))))
       (list 
	(cond ((equal switchtag 's.o.c.) 'switch_on_constant)
	      ((equal switchtag 's.o.s.) 'switch_on_structure)
	      (t (break)))
	(1- (length switches))
	(cdr switches)
	(second (first switches)))))


(defun icg.mk-symbol-label-f-idxtree (idxtree)
  (if (equal (iif.tag-of-idxtree idxtree) 'clauses)
      (first (iif.s-clauses-f-clauses idxtree))
      (iif.s-label-f-idxtree
       (iif.mk-label-f-idxtree idxtree))))
