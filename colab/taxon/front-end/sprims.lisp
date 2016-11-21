;;;  handling separate primitive concepts
;;;  uses: tbox-manager
;;;  17-03-92 aa
;;;  hier (noch ?) nicht unterstuetzt: zwei Deklarationen
;;;  fuer dieselbe Klasse separater Konzepte
;;;  noch: jedes Konzept kommt auch selber als Teil
;;;        der Liste der separaten


(defvar *separate-classes* (make-hash-table))

(defun clear-separate-classes  ()
   (setf *separate-classes* (make-hash-table))
)

(defmacro sprim (class members)
   `(fsprim ',class ',members))


(defun fsprim (class members)
   
   (progn (dolist (m members T)
              (if (tbox-member? m)

                  (tbox-modify m
                           :type 'sprim
                           :definition m
                           :visible? T
                           :output (format nil "Primitive Concept: ~A ~%Family:           ~A" m class)
                           :internal (transform_prim->CNF m)
                           :hierarchy-node (sort-primitive-value-into-graph *subsumption-graph* 
                                                                            (list m))
                           :disjoint (adjoin class 
                                             (tbox-entry-disjoint 
                                                     (get-tbox-entry m)))
                  )
                  (tbox-insert m
                           :type 'sprim
                           :definition m
                           :visible? T
                           :output (format nil "Primitive Concept: ~A ~%Family:           ~A" m class)
                           :internal (transform_prim->CNF m)
                           :hierarchy-node (sort-primitive-value-into-graph *subsumption-graph* 
                                                                            (list m))
                           :disjoint (list class)
                  )
              )
                  
           )
          (setf (gethash class *separate-classes*)
                (union members
                       (gethash class *separate-classes*)  
                 )
          )
         
     T
    )
)

(defmacro ofam (class members)
   `(fofam ',class ',members))

(defun fofam (class members
		    )
   (setf deflist (cons (list 'ofam class members) deflist))
  (let* ((bottom-node (tbox-entry-hierarchy-node (get-tbox-entry 'bottom)))
	 (fam-node (if (tbox-member? class) 
		       (tbox-entry-hierarchy-node (get-tbox-entry class))
		       (value-insert (list (tbox-entry-hierarchy-node (get-tbox-entry 'top)))
				     (list bottom-node)
				     (list class)))))
	(tbox-insert class
		     :type 'ofam
		     :visible? T
                     :output (format nil "Open Family: ~A ~%Members:    ~A" class
                                                     (union members
		                                            (gethash class *separate-classes*) ))
		     :hierarchy-node fam-node
		     )
	(transform_family->CNF class)
	(set-node! (get-cnf-of-conceptname class) fam-node)
	(dolist (m members T)
		(let ((node (new-value-insert (list fam-node)
					      (list bottom-node)
					      (list m)))
		      )
		     (cond ((tbox-member? m)
			    (tbox-modify m
					 :type 'sprim
					 :family 'open
					 :definition m
					 :visible? T
					 :hierarchy-node node
                                         :output (format nil "Primitive Concept: ~A ~%Open Family:      ~A" m class)
					 :disjoint (adjoin class 
							   (tbox-entry-disjoint 
							    (get-tbox-entry m)))
					 ))
			    (t (tbox-insert m
					    :type 'sprim
					    :family 'open
					    :definition m
					    :visible? T
                                            :output (format nil "Primitive Concept: ~A ~%Open Family:      ~A" m class)
					    :hierarchy-node node
					    :disjoint (list class)
					    )
			       ))
		     (transform_oprim->CNF m)
		     (set-node! (get-cnf-of-conceptname m) node)
		     ))
	(setf (gethash class *separate-classes*)
	      (union members
		     (gethash class *separate-classes*)  
		     )
	      
	      
	      )
	T
	)
  
  
  )
(defmacro cfam (class members)
   `(fcfam ',class ',members))


(defun fcfam (class members)
     (setf deflist (cons (list 'cfam class members) deflist))
   
    (let* ((bottom-node (tbox-entry-hierarchy-node (get-tbox-entry 'bottom)))
           (fam-node (value-insert (list (tbox-entry-hierarchy-node (get-tbox-entry 'top)))
                                                      (list bottom-node)
                                                      (list class))))
         
           (tbox-insert class
                        :type 'cfam
                        :visible? T
                        :output (format nil "Closed Family: ~A ~%Members:      ~A" class
                                                     (union members
		                                            (gethash class *separate-classes*) ))
                        :hierarchy-node fam-node
                                                      
             )
	  (transform_family->CNF class)
	  (set-node! (get-cnf-of-conceptname class) fam-node)
                        

          (dolist (m members T)
		  (let ((node (value-insert (list fam-node)
					    (list bottom-node)
					    (list m)))
			)
		  (cond ((tbox-member? m)
			 (tbox-modify m
				      :type 'sprim
				      :family 'closed
				      :definition m
				      :visible? T
                                      :output (format nil "Primitive Concept: ~A ~%Closed Family:    ~A" m class)
				      :hierarchy-node node
				      :disjoint (adjoin class 
							(tbox-entry-disjoint 
							 (get-tbox-entry m)))
				      ))
			(t (tbox-insert m
					:type 'sprim
					:family 'closed
					:definition m
					:visible? T
                                        :output (format nil "Primitive Concept: ~A ~%Closed Family:   ~A" m class)
					:hierarchy-node node
					:disjoint (list class)
					)
			   ))
		       (transform_cprim->cnf m)
		       (set-node! (get-cnf-of-conceptname m) node)
		       ))
	  (setf (gethash class *separate-classes*)
		(union members
		       (gethash class *separate-classes*)  
		       )
		
		)
     T
    )
)

(defmacro sfam (sfam fam members)
;  (fsfam ',sfam ',fam ',members))
;; modified by BBB
  `(fsfam ',sfam ',fam ',members))

(defun fsfam (sfam fam members)
   (setf deflist (cons (list 'sfam sfam fam members) deflist))
  (cond ((family? fam)
	 (let ((fam-entry (get-tbox-entry fam))
	       )
	      (let ((members-of-fam (get-separation-class fam))
		    )
		   (cond ((and (subsetp members members-of-fam) 
			       (not (subsetp members-of-fam members))
			       )
			  (tbox-insert sfam
				       :type 'sfam
				       :visible? T
				       :family fam
				       :members members
				       :c-members (set-difference members-of-fam members)
				       )
			  (transform_subfamily->CNF sfam)
			  (setf (gethash sfam *separate-classes*) members)
			  (setf *classify-list* (cons sfam *classify-list*))
			  T
			  )
			 (t (format t "Warning : ~A isn't a real subset of ~A !" sfam members-of-fam)
			    nil)))))
	 (t (format t "Warning : ~A isn't a family !" fam)
	    nil)
	 ))


(defun get-separation-class (class)
   (gethash class *separate-classes*)
)


(defun get-disjoint-concepts (id)
   (apply 'append
          (mapcar 'get-separation-class
                        (tbox-entry-disjoint (get-tbox-entry id))
          )              
   )
)

   

(defun lsc ()
   (princ (format nil "~% Classes of disjunct primitive concepts:"))
   (princ (format nil "~% ---------------------------------------"))       
   (princ (format nil "~% Class                   Type    Members~%"))
   (maphash #'(lambda (x y) (princ (format nil "~% ~A ~23T ~A ~31T ~A" x 
                    (if (open-family? x) 'open 'closed) y)))
            *separate-classes*)
   (terpri)
   T
)


(defun cprim? (item)
   (eq 'closed (get-family-type-of-item item)))

(defun oprim? (item)
   (eq 'open (get-family-type-of-item item)))


(defun get-number-of-sprims (class)
   (length (gethash class *separate-classes*)))


(defun new-value-insert (uplist lowlist valuelist)
   (if (tbox-member? (first valuelist))
       (tbox-entry-hierarchy-node (get-tbox-entry (first valuelist)))
       (value-insert uplist lowlist valuelist)))

