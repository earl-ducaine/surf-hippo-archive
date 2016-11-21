;;;  functions to give terminological axioms
;;; uses: tbox-manager
;;; 17-03-92 aa
;;; change: prim takes symbols for unary primitive concepts or
;;; a pair consisting of id and arity for abstract predicates
;;; 11-06-92 aa

(defvar *hierarchy-generators* nil)

(defmacro edom-instal (&rest dummy)
   `T)

(defmacro doma (&rest dummy)
   `T)
 

(defmacro conc (&rest deflist)
  ;; please, only one def as input
    `(apply #'fconc (list ',deflist)))


(defun fconc (f &rest rest)
  (let ((deflist (cons f rest)))
       (dolist (definition deflist T)
	       (let ( (name (first definition))
		      (def (replace-cts-as-restrictors (second definition)))
		      )
		    (if  (parse-conc def)
			 (tbox-insert name
				      :type 'conc
				      :arity 1
				      :visible? T
				      :output definition
				      :definition (list name def)
				      :shifted-not (shift-not def)
				      )          
			 (progn 
			  (terpri)
			  (princ (format
				  nil 
				  "~a not a concept definition" definition))
			  (terpri)
			  ))))))

(defun fconc-unvisible (name definition)
      (let ((def (replace-cts-as-restrictors definition))
            )
           (if  (parse-conc def)
                (tbox-insert name
                             :type 'conc
                             :arity 1
                             :visible? NIL
                             :output definition
                             :definition (list name def)
                             :shifted-not (shift-not def)
                 )          
                (progn (terpri)
                       (princ (format nil "~a not a concept definition" definition))
                       (terpri)
                ))))

(defun fconc-p (name def)
      (let ((def (replace-cts-as-restrictors def)))
           (if  (parse-conc def)
                (progn
                (tbox-insert name
                             :type 'conc
                             :pconc? 'T
                             :arity 1
                             :visible? T
                             :output def
                             :definition (list name def)
                             :shifted-not (shift-not def)
                 )
                 (tbox-insert-hierarchy-node name
                                             (sort-lower-primitive-value-into-graph 
                                                     #'(lambda (nd) (value-subsumes (car (node-value nd)) name))
                                                     (list name)
                                                     *subsumption-graph*))
                  )          
                (progn (terpri)
                       (princ (format nil "~a not a concept definition" def))
                       (terpri)
                )
           )
      )
)


(defmacro attr (&rest attrlist)
  `(apply #'fattr ',attrlist))

(defun fattr (f &rest rest)
  (let ((attrlist (cons f rest)))
       (dolist (id attrlist T)
	       (tbox-insert id 
			    :type 'attr
			    :visible? T
			    :definition id)
	       )
       )
  )

(defmacro role (&rest rolelist)
  `(apply #'frole ',rolelist))


(defun frole (f &rest rest)
  (let ((rolelist (cons f rest)))
       (dolist (id rolelist T)
	       (tbox-insert id 
			    :type 'role
			    :definition id)
	       )
       )
  )

(defmacro prim (&rest primlist)
  `(apply #'fprim ',primlist))

(defun fprim (f &rest rest)
  (let ((primlist (cons f rest)))
       (dolist (id primlist T)
	       (let ((node (sort-primitive-value-into-graph
			    *subsumption-graph*
			    (list id)))
		     )
		    (symbolp id)
		    (tbox-insert id 
				 :type 'prim
				 :arity 1
				 :visible? T
				 :hierarchy-node node
				 :output (format nil "Primitive Concept: ~A" id)
				 :definition id
				 )
		    (transform_prim->CNF id)
		    (set-node! (get-cnf-of-conceptname id) node)
		    )))
  )


(defun fprim-unvisible (f &rest rest)
  (let ((primlist (cons f rest)))
       (dolist (id primlist T)
	       (symbolp id)
	       (tbox-insert id 
			    :type 'prim
			    :arity 1
			    :visible? NIL
			    
			    :definition id)
	       
	       )
       )
  )


(defmacro apred (&rest apredlist)
  ;;; please, only one def as input
  `(apply #'fapred (list ',apredlist)))

(defun fapred (f &rest rest)
  (let ((apredlist (cons f rest)))
       (dolist (id apredlist T)
	       
	       (tbox-insert (first id)
			    :type 'apred
			    :arity (second id)
			    :visible? T
			    :definition id)
	       
	       )
       
       )
  )


(defmacro cpred (&rest cpredlist)
  ;; please, only one def as input
  `(apply #'fcpred (list ',cpredlist)))


(defun fcpred (f &rest rest)
  (let ((cpredlist (cons f rest)))
       (dolist
	(def cpredlist T)
	(let ((id (first def))
	      (domain (first (second def)))
	      (varlist (second (second def)))
	      ;; the predicate term has to be expanded using the functions
	      ;; of file exp-pred
	      ;; this allows to use nested predicate definitions
	      (term (third (second (expand-pred def))))
	      (arity (length (second (second def))))
	      )
	     (tbox-insert id
			  :type 'cpred
			  :arity arity
			  :definition def
			  :cpred-domain domain
			  :cpred-varlist varlist
			  :cpred-term term
			  :visible? T
			  :internal (make-internal-rep-of-predicate domain
								    varlist
								    term)
			  :negated (make-internal-rep-of-neg-predicate domain
								       varlist
								       term)
			  )
	     )
	)
       )
  )





(defun make-restrictor-concepts (term)
  (mrc-help term))

(defun mrc-help (subterm)
  (cond ((keyword? subterm) subterm)
	((tbox-member? subterm) subterm)
	((and (listp subterm) 
	      (or (eq (first subterm) 'forall) (eq (first subterm) 'some)) 
	      (= (length subterm) 3) 
	      (concept-term? (third subterm)) 
	      (not (conceptname? (third subterm))))
	 (if (or (sprim? (third subterm)) (prim? (third subterm))) subterm 
	     (let ((new-concept (gentemp "Restriction-Helper")))
		  (fconc-unvisible new-concept (if (or (prim? (third subterm)) 
						       (sprim? (third subterm))) 
						   (third subterm) 
						   (mrc-help (third subterm))))
		  (list (first subterm) (second subterm) new-concept))))
	(t (mapcar #'mrc-help subterm))
	))


(defun replace-cts-as-restrictors (r)
  (if (listp r)
      (cond  ((restriction? r)
	      (make-restrictor-concepts r))
	     ((conjunction? r)
	      (make-conjunction (mapcar #'mrc-help 
					(get-concepts-of-conjunction r))))  
	     ((disjunction? r)
	      (make-disjunction (mapcar #'mrc-help 
					(get-concepts-of-disjunction r))))
	     ((negation? r)
	      (make-negation (mrc-help (get-concept-of-negation r))))
	     (t r)
	     )
      r)
  )


(defun keyword? (item)
  (member item '(and or forall some not agree disagree)))



(defun insert-primitive (g n)
  ())



(defmacro pconc (&rest deflist)
  ;; please, only one def as input
  `(apply #'fpconc (list ',deflist)))



(defun fpconc (f &rest rest)
  (let ((deflist (cons f rest)))
       (dolist (definition deflist T)
	       (let ( (name (first definition))
		      (def (replace-cts-as-restrictors (second definition)))
		      (help-concept (gentemp "pconc-helper")) )
		    (fprim-unvisible help-concept)
		    (fconc-p name
			     (make-conjunction (list help-concept def)))
		    )
	       )
       )
  )


(defmacro hierarchy (&rest hierlist)
  `(funcall #'fhierarchy ',hierlist))

(defun fhierarchy (hierlist)
  (dolist (concname hierlist T)
	  (setf *hierarchy-generators*
		(adjoin concname *hierarchy-generators*)))
  )



