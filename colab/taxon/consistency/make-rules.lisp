;************************************************************
;*                                                          *
;*                File : make-rules
;*                                                          *
;************************************************************

(defun make-top-rule ()
  #'(lambda (obj) (top-rule obj)))
(defun make-bottom-rule ()
  #'(lambda (obj) (bottom-rule obj)))

(defun make-conjunction-rule (concepts)
  #'(lambda (obj) (conjunction-rule concepts obj)))
(defun make-disjunction-rule (concepts)
  #'(lambda (obj) (disjunction-rule concepts obj)))

(defun make-conceptname-rule (concept)
  #'(lambda (obj) (conceptname-rule concept obj)))
(defun make-conceptname-rule-II (concept)
  #'(lambda (obj) (conceptname-rule-II concept obj)))
(defun make-neg-conceptname-rule (concept)
  #'(lambda (obj) (neg-conceptname-rule concept obj)))

(defun make-prim-rule (prim)
  #'(lambda (obj) (primitive-rule prim obj)))
(defun make-neg-prim-rule (prim)
  #'(lambda (obj) (neg-primitive-rule prim obj)))

(defun make-family-rule (cf)
  #'(lambda (obj) (family-rule cf obj)))
(defun make-subfamily-rule (cf cprims)
  #'(lambda (obj) (subfamily-rule cf cprims obj)))
(defun make-neg-family-rule (cf)
  #'(lambda (obj) (neg-family-rule cf obj)))
(defun make-neg-subfamily-rule (cf cprims)
  #'(lambda (obj) (neg-subfamily-rule cf cprims obj)))

(defun make-sprim-rule (fam prim)
  #'(lambda (obj) (sprim-rule fam prim obj)))
(defun make-neg-oprim-rule (fam prim)
  #'(lambda (obj) (neg-oprim-rule fam prim obj)))
(defun make-neg-cprim-rule (fam prim)
  #'(lambda (obj) (neg-cprim-rule fam prim obj)))

(defun make-role-rule (role)
  #'(lambda (obj1 obj2) (role-rule obj1 role obj2)))
(defun make-attr-rule (attr)
  #'(lambda (obj1 obj2) (attr-rule obj1 attr obj2)))

(defun QPC-rule (rule restrictor-rule)
  #'(lambda (obj) (funcall rule restrictor-rule obj)))
(defun make-conc-path-rule (path-rule path path-rules)
  #'(lambda (obj) (funcall path-rule path path-rules obj)))

(defun QPP-rule (rules pred)
  #'(lambda (obj) (funcall rules (make-internal-repr-of-predicate pred) (deref obj) (deref obj))))
(defun make-predicate-rule (last-rule counter path-rules)
  #'(lambda (int-pred first obj) (funcall last-rule counter path-rules int-pred first (deref obj))))
(defun make-pred-path-rule (path-rule path path-rules)
  #'(lambda (int-pred first obj) (funcall path-rule path path-rules int-pred first (deref obj))))


(defun make-disagreement-rule (path-rules)
  #'(lambda (obj) (funcall path-rules (make-internal-repr-of-agreement) obj obj)))
(defun make-agreement-rule (path-rules)
  #'(lambda (obj) (funcall path-rules (make-internal-repr-of-agreement) obj obj)))


(defun make-agree-rule (pathes)
  (make-agreement-rule (make-path-rules pathes 'forall 'AR 1 'equal)))
(defun make-disagree-rule (pathes)
  (make-disagreement-rule (make-path-rules pathes 'some 'DAR 1 'unequal)))

(defun make-QPC-rule (quantifier path restrictor &optional negated)
  (make-QPC quantifier
	    path
	    (QPC-rule (if negated 'NCR 'CR)
		      (get-restrictor-rule restrictor negated))
	    restrictor))

(defun make-QPP-rule (quantifier path restrictor negated neg-pred-p)
  (QPP-rule (make-path-rules path 
			     quantifier 
			     (get-last-rule quantifier restrictor negated neg-pred-p) 
			     1
			     restrictor) 
	    restrictor))


(defun make-path-rules (pathes quantifier rule counter restrictor)
  (cond ((empty-pathes? pathes)
	 (cond ((eq rule 'SCPR) 
		(make-propagate-predicate-function))
	       ((eq rule 'NSCPR)
		(make-cond-propagate-predicate-function))
	       (t (make-do-nothing-function))
	       ))
	((empty-path? (first-path pathes))
	 (let ((predicate-rule (make-predicate-rule rule 
						    counter
						    (make-path-rules (rest-pathes pathes)
								     quantifier
								     rule
								     (1+ counter)
								     restrictor)))
	       )
	      (if (forall? quantifier)
		  (map-from-function-to-definition! predicate-rule pathes restrictor))
	      predicate-rule))
	(t (let ((path-rule (make-pred-path-rule 
			     (get-pred-path-rule quantifier
						 (get-first-relation-of-path (first-path pathes)))
			     (get-first-relation-of-path (first-path pathes))
			     (make-path-rules (cons (cdr (first-path pathes))
						    (rest-pathes pathes))
					      quantifier
					      rule
					      counter
					      restrictor)))
		 )
		(if (forall? quantifier)
		    (map-from-function-to-definition! path-rule 
						      pathes 
						      restrictor))
		path-rule))))
  

(defun make-QPC (quantifier path restrictor-rule restrictor)
  (cond ((empty-path? path) 
	 (map-from-function-to-definition! restrictor-rule '() restrictor)
	 restrictor-rule)
	(t (let ((path-rule (make-conc-path-rule 
			     (get-conc-path-rule quantifier
						 (get-first-relation-of-path path))
			     (get-first-relation-of-path path)
			     (make-QPC quantifier 
				       (get-rest-path path) 
				       restrictor-rule
				       restrictor)))
		 )
		(cond ((some? quantifier)
		       path-rule)
		      (t (map-from-function-to-definition! path-rule 
							   path
							   restrictor)
			 path-rule))))
	))

;*******************************************************************************
;*
;*	Help Functions
;*
;*******************************************************************************

(defun get-conc-path-rule (quantifier relation)
  (if (functional? relation)
      (if (some? quantifier) 'SAC 'FAC)
      (if (some? quantifier) 'SRC 'FRC))
  )
(defun get-pred-path-rule (quantifier relation)
  (if (functional? relation)
      (if (some? quantifier) 'SAP 'FAP)
      (if (some? quantifier) 'SRP 'FRP))
  )
(defun get-last-rule (quantifier restrictor negated negated-restrictor)
  (cond ((apred? restrictor)
	 (if negated 
	     (if negated-restrictor 'NAPNR 'NAPR)
	     (if negated-restrictor 'APNR 'APR)))
	((cpred? restrictor)
	 (if negated 
	     (if (forall? quantifier)
		 'NFCPR 'NSCPR)
	     (if (forall? quantifier)
		 'FCPR 'SCPR)))
	((agreement? restrictor)
	 'AR)
	((disagreement? restrictor)
	 'DAR)
	(t (error "UNKNOWN RESTRICTION in get-last-rule ")))) 
				  
(defun get-restrictor-rule (restrictor negated)
  (if negated
      (cond ((conceptname? restrictor)
	     (make-neg-conceptname-rule (get-cnf-of-conceptname restrictor)))
	    ((prim? restrictor)
	     (make-neg-prim-rule restrictor))
	    ((family? restrictor)
	     (make-neg-family-rule restrictor))
	    ((subfamily? restrictor)
	     (make-neg-subfamily-rule restrictor))
	    ((cprim? restrictor)
	     (make-neg-cprim-rule (get-family restrictor) restrictor))
	    ((oprim? restrictor)
	     (make-neg-oprim-rule (get-family restrictor) restrictor))
	    )
      (cond ((conceptname? restrictor)
	     (make-conceptname-rule (get-cnf-of-conceptname restrictor)))
	    ((prim? restrictor)
	     (make-prim-rule restrictor))
	    ((family? restrictor)
	     (make-family-rule restrictor))
	    ((subfamily? restrictor)
	     (make-subfamily-rule restrictor))
	    ((sprim? restrictor)
	     (make-sprim-rule (get-family restrictor) restrictor))
	    (t (error "Unknown Restrictor in get-restrictor-rule"))
	)))

(defun make-do-nothing-function ()
  #'(lambda (x y z) 'done))
(defun make-propagate-predicate-function ()
  #'(lambda (pred dummy1 dummy2) (propagate-single-predicate pred)))
(defun make-cond-propagate-predicate-function ()
  #'(lambda (pred dummy1 dummy2) (propagate-cond-single-predicate pred)))
