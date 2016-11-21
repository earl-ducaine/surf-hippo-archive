;************************************************************
;*                                                          *
;*                File : transform concepts in CNF
;*                                                          *
;************************************************************

(defun transform_concept->CNF (conceptname)
  ;;; concept : structure of type concept
  ;;; conceptterm : p.e. (and human male)
  ;;; 
  (let ((cnf (make-cnf))
	(conceptterm (get-concept-term conceptname))
	)
       (put-conceptname-of-cnf! cnf conceptname)
       (funcall (get-CNF-rule conceptterm) cnf conceptterm)
       (dolist (QPC (get-QPCs cnf))
	       (create-new-concepts cnf QPC))
       (put-cnf-of-conceptname! conceptname cnf)
       (classify conceptname)
       ))

(defun transform_prim->CNF (prim)
  (let ((cnf (make-cnf))
	)
       (put-conceptname-of-cnf! cnf prim)
       (CNF-tr-prim cnf prim)
       (put-cnf-of-conceptname! prim cnf)
       ))
(defun transform_family->CNF (fam)
  (let ((cnf (make-cnf))
	)
       (put-conceptname-of-cnf! cnf fam)
       (CNF-tr-family cnf fam)
       (put-cnf-of-conceptname! fam cnf)
       ))
(defun transform_subfamily->CNF (sfam)
  (let ((cnf (make-cnf))
	)
       (put-conceptname-of-cnf! cnf sfam)
       (CNF-tr-subfamily cnf sfam) 
       (put-cnf-of-conceptname! sfam cnf)
       (classify sfam)
       ))
(defun transform_oprim->CNF (prim)
  (let ((cnf (make-cnf))
	)
       (put-conceptname-of-cnf! cnf prim)
       (CNF-tr-oprim cnf prim)
       (put-cnf-of-conceptname! prim cnf)
       ))
(defun transform_cprim->CNF (prim)
  (let ((cnf (make-cnf))
	)
       (put-conceptname-of-cnf! cnf prim)
       (CNF-tr-cprim cnf prim)
       (put-cnf-of-conceptname! prim cnf)
       ))

(defun CNF-tr-conceptname (cnf name)
  (let ((cnf-of-name (get-cnf-of-conceptname name))
	)
       (add-to-rules! cnf (make-conceptname-rule-II cnf-of-name))
       (add-to-neg-rules! cnf (make-neg-conceptname-rule cnf-of-name))
       ;;;
       (add-list-to-definitive-primitives! cnf (get-pos-definitive-prims cnf-of-name))
       (add-list-to-possible-primitives! cnf (get-pos-possible-prims cnf-of-name))
       (add-list-to-neg-definitive-primitives! cnf (get-neg-definitive-prims cnf-of-name))
       (add-list-to-neg-possible-primitives! cnf (get-neg-possible-prims cnf-of-name))
       ;;;
       (add-list-to-forall-links! cnf (get-forall-links cnf-of-name))
       (add-list-to-exists-links! cnf (get-exists-links cnf-of-name))
       ;;;
       (add-list-to-definitive-families! cnf (get-pos-definitive-families cnf-of-name))
       (add-list-to-neg-definitive-families! cnf (get-neg-definitive-families cnf-of-name))
       (add-list-to-possible-families! cnf (get-pos-possible-families  cnf-of-name))
       (add-list-to-neg-possible-families! cnf (get-neg-possible-families  cnf-of-name))
       ;;;
       (add-to-SR! cnf (make-concept-SR cnf-of-name))
       ;;;
       (if (get-cnf-agree cnf-of-name)
	   (set-agree! cnf))
       (if (get-cnf-disagree cnf-of-name)
	   (set-disagree! cnf))
       ;;;
       (dolist (QPC (get-QPCs cnf-of-name))
	       (funcall (get-CNF-rule QPC) cnf QPC t))
       ))

(defun CNF-tr-prim (cnf prim)
  (add-to-rules! cnf (make-prim-rule prim))
  (add-to-neg-rules! cnf (make-neg-prim-rule prim))
  (add-to-definitive-primitives! cnf prim)
  (add-to-SR! cnf (make-prim-SR prim))
  )

(defun CNF-tr-family (cnf fam)
  (add-to-rules! cnf (make-family-rule fam))
  (add-to-neg-rules! cnf (make-neg-family-rule fam))
  (add-family-to-definitive-families! cnf fam)
  (add-family-to-possible-families! cnf fam)
  (add-to-SR! cnf (make-family-SR fam))
  )
(defun CNF-tr-subfamily (cnf sfam) 
  (let ((fam-entry (get-tbox-entry sfam))
	)
       (let ((fam (tbox-entry-family fam-entry))
	     (members (tbox-entry-members fam-entry))
	     (c-members (tbox-entry-c-members fam-entry))
	     )
	    (add-to-rules! cnf (make-subfamily-rule fam c-members))
	    (add-to-neg-rules! cnf (make-neg-subfamily-rule fam members))
	    (dolist (member members)
		    (add-sprim-to-possible-families! cnf member))
	    (add-to-SR! cnf (make-sfamily-SR sfam))
	    )))

(defun CNF-tr-cprim (cnf prim)
  (add-to-rules! cnf (make-sprim-rule (get-family prim) prim))
  (add-to-neg-rules! cnf (make-neg-cprim-rule (get-family prim) prim))
  (add-sprim-to-definitive-families! cnf prim)
  (add-sprim-to-possible-families! cnf prim)
  (add-to-SR! cnf (make-cprim-SR prim))
  )
(defun CNF-tr-oprim (cnf prim)
  (add-to-rules! cnf (make-sprim-rule (get-family prim) prim))
  (add-to-neg-rules! cnf (make-neg-oprim-rule (get-family prim) prim))
  (add-sprim-to-definitive-families! cnf prim)
  (add-sprim-to-possible-families! cnf prim)
  (add-to-SR! cnf (make-oprim-SR prim))
  )

(defun CNF-tr-and (cnf conceptterm)
  (dolist (next-concept (get-concepts-of-conjunction conceptterm))
	  (funcall (get-CNF-rule next-concept)
		   cnf 
		   next-concept)
	  ))

(defun CNF-tr-or (cnf conceptterm) 
  (add-to-possibles! cnf conceptterm)
  (add-to-SR! cnf (make-or-SR))
  (add-to-rules! cnf (constructors->functions conceptterm))
  (add-to-neg-rules! cnf (constructors->functions (shift-not (make-negation conceptterm))))
  )

(defun CNF-tr-not (cnf conceptterm)
  (let ((neg-concept (get-concept-of-negation conceptterm))
	)
       (cond ((conceptname? neg-concept)
	      (let ((neg-CNF (get-CNF-of-conceptname neg-concept))
		    )
		   (add-to-rules! cnf (make-neg-conceptname-rule neg-cnf))
		   (add-to-neg-rules! cnf (make-conceptname-rule neg-cnf))
		   ;;;
		   (add-list-to-neg-possible-primitives! cnf 
							 (union (get-pos-possible-prims neg-CNF)
								(get-pos-definitive-prims neg-CNF)))
		   (add-list-to-possible-primitives! cnf
						     (union (get-neg-possible-prims neg-CNF)
							    (get-neg-definitive-prims neg-CNF)))
		   ;;;
		   (add-list-to-exists-links! cnf (get-forall-links neg-CNF))
		   (add-list-to-forall-links! cnf (get-exists-links neg-CNF))
		   ;;;
		   (add-list-to-possible-families! cnf (get-neg-possible-families neg-CNF))
		   (add-list-to-possible-families! cnf (get-neg-definitive-families neg-CNF))
		   (add-list-to-neg-possible-families! cnf (get-pos-definitive-families neg-CNF))
		   (add-list-to-neg-possible-families! cnf (get-pos-possible-families neg-CNF))
		   ;;;
		   (if (get-cnf-agree neg-CNF)
		       (set-disagree! cnf))
		   (if (get-cnf-disagree neg-CNF)
		       (set-agree! cnf))
		   ;;;
		   (add-to-SR! cnf (make-neg-concept-SR neg-CNF))
		   ))
	     ((prim? neg-concept)
	      (add-to-rules! cnf (make-neg-prim-rule neg-concept))
	      (add-to-neg-rules! cnf (make-prim-rule neg-concept))
	      (add-to-neg-definitive-primitives! cnf neg-concept)
	      (add-to-SR! cnf (make-neg-prim-SR neg-concept)))
	     ((family? neg-concept)
	      (add-to-rules! cnf (make-neg-family-rule neg-concept))
	      (add-to-neg-rules! cnf (make-family-rule neg-concept))
	      (add-family-to-neg-definitive-families! cnf neg-concept)
	      (add-to-SR! cnf (make-neg-family-SR neg-concept))
	      )
	     ((subfamily? neg-concept)
	      (let ((fam-entry (get-tbox-entry neg-concept))
		    )
		   (let ((fam (tbox-entry-family fam-entry))
			 (members (tbox-entry-members fam-entry))
			 (c-members (tbox-entry-c-members fam-entry))
			 )
			(add-to-rules! cnf (make-neg-subfamily-rule fam members))
			(add-to-neg-rules! cnf (make-subfamily-rule fam c-members))
			(dolist (member members)
				(add-sprim-to-neg-definitive-families! cnf member))
			(add-to-SR! cnf (make-neg-sfamily-SR neg-concept))
			)))
	     ((cprim? neg-concept)
	      (add-to-rules! cnf (make-neg-cprim-rule (get-family neg-concept) neg-concept))
	      (add-to-neg-rules! cnf (make-sprim-rule (get-family neg-concept) neg-concept))
	      (add-sprim-to-neg-definitive-families! cnf neg-concept)
	      (add-to-SR! cnf (make-neg-sprim-SR neg-concept))
	      )
	     ((oprim? neg-concept)
	      (add-to-rules! cnf (make-neg-oprim-rule (get-family neg-concept) neg-concept))
	      (add-to-neg-rules! cnf (make-sprim-rule (get-family neg-concept) neg-concept))
	      (add-sprim-to-neg-definitive-families! cnf neg-concept)
	      (add-to-SR! cnf (make-neg-sprim-SR neg-concept))
	      )
	     ((restriction? neg-concept)
	      (let ((quantifier (get-quantifier neg-concept))
		    (path (get-pathes-of-restriction neg-concept))
		    (restrictor (get-restrictor neg-concept))
		    (first-roles (get-first-relation-list (get-pathes-of-restriction neg-concept)))
		    )
		   (add-to-rules! cnf 
				 (make-QPC-rule (negate-quantifier quantifier)
						path
						restrictor
						t))
		   (add-to-neg-rules! cnf
				     (make-QPC-rule quantifier
						    path
						    restrictor))
		   (cond ((forall? quantifier)
			  (add-list-to-exists-links! cnf first-roles)
			  (add-to-SR! cnf (make-some-SR first-roles))
			  )
			 (t (add-list-to-forall-links! cnf first-roles)
			    (add-to-SR! cnf (make-forall-SR first-roles)))
			 )
		   ))
	     )))

(defun CNF-tr-forall-frp-C (cnf term &optional second-mode)
  (let ((path (get-path-of-conceptname term))
	(restrictor (get-restrictor term))
	)
       (unless second-mode
	       (add-to-forall-links! cnf (get-first-relation-of-path  path))
	       (add-to-SR! cnf (make-forall-SR  (get-first-relation-list  path)))
	       (add-to-neg-rules! cnf 
				  (make-QPC-rule (make-quantifier-some)
						 (get-path-of-conceptname term)
						 restrictor
						 t)))
       (let ((other-R (search-one-restr-with-equal-path cnf path))
	     )
	    (cond (other-R
		   (delete-QPC! cnf other-R)
		   (add-to-quantifier-path-C! 
		    cnf
		    (make-QP-restriction (get-quantifier other-R)
					 path
					 (compute-new-restrictor restrictor
								 (get-restrictor other-R))))
		   )
		  (t (add-to-quantifier-path-C! cnf term))
		  )))
  )

(defun CNF-tr-some-frp-C (cnf term &optional second-mode)
  (let ((path (get-path-of-conceptname term))
	(restrictor (get-restrictor term))
	)
       (unless second-mode
	       (add-to-exists-links! cnf (get-first-relation-of-path path))
	       (add-to-SR! cnf (make-some-SR (get-first-relation-list path)))
	       (add-to-neg-rules! cnf 
				  (make-QPC-rule (make-quantifier-forall)
						 path
						 restrictor
						 t)))
       (let ((other-R (search-one-restr-with-equal-path cnf path))
	     )
	    (cond (other-R 
		   (delete-QPC! cnf other-R)
		   (add-to-quantifier-path-C! 
		    cnf
		    (make-exists-in-restriction path
						(compute-new-restrictor restrictor
									(get-restrictor other-R))))
		   )
		  (t (add-to-quantifier-path-C! cnf term))
		  )))
  )

(defun CNF-tr-forall-rp-C (cnf term &optional second-mode)
  (let ((path (get-path-of-conceptname term))
	(restrictor (get-restrictor term))
	)
       (unless second-mode
	       (add-to-forall-links! cnf (get-first-relation-of-path path))
	       (add-to-SR! cnf (make-forall-SR (get-first-relation-list path)))
	       (add-to-neg-rules! cnf 
				  (make-QPC-rule (make-quantifier-some) 
						 path
						 restrictor
						 t)))
       (let ((forall-R (search-one-value-restr-with-equal-path cnf path))
	     )
	    (dolist (some-R (search-all-exists-in-restr-with-equal-path cnf path))
		    (let ((new-restrictor (compute-new-restrictor restrictor 
								  (get-restrictor some-R)))
			  )
			 (unless (eq new-restrictor some-R)
				 (add-to-quantifier-path-C! 
				  cnf
				  (make-exists-in-restriction path
							      new-restrictor)))))
	    (cond (forall-R
		   (delete-QPC! cnf forall-R)
		   (add-to-quantifier-path-C! 
		    cnf
		    (make-value-restriction path
					    (compute-new-restrictor restrictor
								    (get-restrictor forall-R)))))
		  (t (add-to-quantifier-path-C! cnf term))
		  )))
  )

(defun CNF-tr-some-rp-C (cnf term &optional second-mode)
  (let ((path (get-path-of-conceptname term))
	(restrictor (get-restrictor term))
	)
       (unless second-mode
	       (add-to-exists-links! cnf (get-first-relation-of-path path))
	       (add-to-SR! cnf (make-some-SR (get-first-relation-list path)))
	       (add-to-neg-rules! cnf 
				  (make-QPC-rule (make-quantifier-forall)
						 path
						 restrictor
						 t)))
       (let ((forall-R (search-one-value-restr-with-equal-path cnf path))
	     )
	    (cond (forall-R
		   (add-to-quantifier-path-C! 
		    cnf
		    (make-value-restriction path
					    (compute-new-restrictor restrictor
								    (get-restrictor forall-R)))))
		  (t (add-to-quantifier-path-C! cnf term))
		  )))
  )

(defun CNF-tr-QPP (cnf term)
  (let ((quantifier (get-quantifier term))
	(path (get-pathes-of-predicate-restriction term))
	(predicate (get-restrictor term))
	(neg-pred-p (negated-restrictor? term))
	)
       (add-to-rules! cnf (make-QPP-rule quantifier
					path
					predicate
					'()
					neg-pred-p))
       (add-to-neg-rules! cnf (make-QPP-rule (negate-quantifier quantifier)
					    path
					    predicate
					    t
					    neg-pred-p))
       (cond  ((forall? quantifier)
	       (add-list-to-forall-links! cnf (get-first-relation-list path))
	       (add-to-SR! cnf  (make-forall-SR (get-first-relation-list path)))
	       )
	      (t (add-list-to-exists-links! cnf (get-first-relation-list path))
		 (add-to-SR! cnf (make-some-SR (get-first-relation-list path))))
       )))

(defun CNF-tr-agreement (cnf term)
  (let ((path (get-pathes-of-agreement term))
	)
       (if (agree-with-empty-path? term)
	   (set-agree! cnf))
       (add-to-rules! cnf (make-agree-rule path))
       (add-to-neg-rules! cnf (make-disagree-rule path))
       (add-list-to-forall-links! cnf (get-first-relation-list path))
       (add-to-SR! cnf (make-forall-SR (get-first-relation-list path)))
       ))

(defun CNF-tr-disagreement (cnf term)
  (let ((path (get-pathes-of-agreement term))
	)
       (if (disagree-with-empty-path? term)
	   (set-disagree! cnf))
       (add-to-rules! cnf (make-disagree-rule path))
       (add-to-neg-rules! cnf (make-agree-rule path))
       (add-list-to-exists-links! cnf (get-first-relation-list path))
       (add-to-SR! cnf (make-some-SR (get-first-relation-list path)))
       ))

(defun create-new-concepts (cnf QPC)
  (let ((quantifier (get-quantifier QPC))
	(path (get-path-of-conceptname QPC))
	(restrictor  (get-restrictor QPC))
	)
       (cond ((not (conjunction? restrictor))
	      (add-to-exec-QPC! cnf
				(make-QPC-rule quantifier
					       path
					       restrictor)))
	     (t (let ((concept (get-equal-concept restrictor))
		      )
		     (cond (concept
			    (add-to-exec-QPC! cnf
					      (make-QPC-rule quantifier
							     path
							     concept)))
			   (t (let ((new-name (make-new-conceptname))
				    )
				   (rplacd (cdr QPC) (list new-name))
				   (set-created-concept! new-name)
				   (fconc-unvisible new-name restrictor)
				   (add-to-exec-QPC! cnf
						     (make-QPC-rule quantifier
								    path
								    new-name))))
			   )))
  )))

(defun term-exists? (term list-or-term)
  (cond ((symbolp list-or-term)
	 (eq term list-or-term))
	((term-exists? term (cadr list-or-term)))
	((term-exists? term (caddr list-or-term)))))

(defun compute-new-restrictor (cp conj-of-cps)
  (cond ((concept-more-specialized? (get-cnf-of-conceptname cp) conj-of-cps)
	 cp)
	((conj-of-concepts-more-specialized? (get-cnf-of-conceptname cp) conj-of-cps)
	 conj-of-cps)
	(t (make-conjunction (list cp conj-of-cps)))
	))

(defun concept-more-specialized? (cnf-of-cp conj-of-cps)
  (cond ((conjunction? conj-of-cps)
	 (let ((cnf-of-cp-of-conj (get-cnf-of-conceptname (get-first-term conj-of-cps)))
	       )
	      (let ((conj>=cp (if (and (get-node cnf-of-cp)
				       (get-node cnf-of-cp-of-conj))
				  (subsumption-look-up cnf-of-cp-of-conj cnf-of-cp)
				  (or (eq cnf-of-cp-of-conj cnf-of-cp)
                                      (subsumes-weakly? cnf-of-cp-of-conj cnf-of-cp))))
		    )
		   (if (eq conj>=cp t)
		       (concept-more-specialized? cnf-of-cp (get-second-term conj-of-cps)))
		   )))
	(t (let ((cnf-of-cp-of-conj (get-cnf-of-conceptname conj-of-cps))
		 )
		(let ((conj>=cp (if (and (get-node cnf-of-cp)
					 (get-node cnf-of-cp-of-conj))
				    (subsumption-look-up cnf-of-cp-of-conj cnf-of-cp)
				    (or (eq cnf-of-cp-of-conj cnf-of-cp)
					(subsumes-weakly? cnf-of-cp-of-conj cnf-of-cp))))
		      )
		     (eq conj>=cp t)
		     )))
	))

(defun conj-of-concepts-more-specialized? (cnf-of-cp conj-of-cps)
  (cond ((conjunction? conj-of-cps)
         (let ((cnf-of-cp-of-conj (get-cnf-of-conceptname (get-first-term conj-of-cps)))
               )
              (let ((cp>=conj (if (and (get-node cnf-of-cp)
                                       (get-node cnf-of-cp-of-conj))
                                  (subsumption-look-up cnf-of-cp cnf-of-cp-of-conj)
                                  (or (eq cnf-of-cp-of-conj cnf-of-cp)
				      (subsumes-weakly? cnf-of-cp cnf-of-cp-of-conj))))
                    )
                   (if (eq cp>=conj t)
                       t
                       (conj-of-concepts-more-specialized? cnf-of-cp (get-second-term conj-of-cps)))
                   )))
        (t (let ((cnf-of-cp-of-conj (get-cnf-of-conceptname conj-of-cps))
                 )
                (let ((cp>=conj (if (and (get-node cnf-of-cp)
                                         (get-node cnf-of-cp-of-conj))
                                    (subsumption-look-up cnf-of-cp cnf-of-cp-of-conj)
                                    (or (eq cnf-of-cp-of-conj cnf-of-cp)
					(subsumes-weakly? cnf-of-cp cnf-of-cp-of-conj))))
                      )
                     (eq cp>=conj t)
                     )))
        ))


(defun make-CNF-of-Bottom ()
  (let ((bottom-CNF (make-CNF))
	)
       (add-to-rules! bottom-CNF (make-bottom-rule))
       (add-to-neg-rules! bottom-CNF (make-top-rule))
       (add-to-SR! bottom-CNF (make-bottom-SR))
       bottom-CNF
       ))

(defun make-CNF-of-Top ()
  (let ((top-CNF (make-CNF))
	)
       (add-to-rules! top-CNF (make-top-rule))
       (add-to-neg-rules! top-CNF (make-bottom-rule))
       (add-to-SR! top-CNF (make-top-SR))
       top-CNF
       ))
