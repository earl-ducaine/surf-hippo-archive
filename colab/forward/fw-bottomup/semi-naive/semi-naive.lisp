;-------------------------------------------------------------------------
;
; Semi-naive bottom-up evaluation
; Projektarbeit
; Mai 1991
; by Thomas Labisch
; Integration in COLAB
; September 1991
; Extensions for handling multiple conclusion
; July 1992
; by Thomas Labisch
; Interaction with TAXON in work
; May 1992 - ...
;  Original atrrtrems can be treated.
;  September 1992
; by Thomas Labisch
;
;-------------------------------------------------------------------------
(defvar *semi-count* 0 "Counter for the levels of evaluation in trace mode")



;-------------------------------------------------------------------------------
;
; main procedure
;
; function:     forward-cmd-seminaive
; arguments:    nil
; returns:      the transitive closure of the facts of the database wrt to the
;               rules of the database and
;               the applied rules of the database
;
;-------------------------------------------------------------------------------
(defun forward-cmd-seminaive ()
  "Top-level command for semi-naive bottom-up evaluation. The tags from facts
  and rules are cut of."
  (or *up-rulebase*
      (if *automatic-mode*
	  (colab-splitrules-forward nil)))
  (cond ((or (null *up-rulebase*)
	     (null *factbase*)) 
	 (princ "No rules or facts are given")
	 (terpri))
	(t
	 (let ((edb (mapcar #'cadr *factbase*))
	       (idb (mapcar #'cdr *up-rulebase*)))
	      (forward-cmd-semi edb idb)))))



(defun forward-cmd-semi (edb idb)
  "First the backward predicates are determined. In a preprocessing step
  first recursive and nonrecursive rules and predicates are computed and the
  indexing of facts and rules is done. Then the entire bottom-up evaluation
  is called. At the end, clauses are build from the computed relations and 
  added to the derived facts."
  (setq *backward-preds* (set-bps (append *fw-prelude* *rfi-database*)))
  (if (taxon-p)
      (init-taxon))
  (let* ((rec-rules-nonrec-rules (recursive idb))
         (rec-rules (first rec-rules-nonrec-rules))
         (nonrec-rules (second rec-rules-nonrec-rules))
         (head-order-lb-level (order-subgoals-all rec-rules
                                                  (append rec-rules
                                                          nonrec-rules)) ))
	(let ((number (build-index-idb-r head-order-lb-level 0)))
	     (build-index-idb-nr nonrec-rules number)
	     (if (taxon-p)
		 (let ((data (get 'preds 'data))
		       (add-data (get 'preds 'add-data)))
		      (putpropp 'preds 
				(union data add-data)
				'dbtaxon)
		      (putpropp 'preds
				(union (get 'taxon 'concepts)
				       (get 'taxon 'attributs))
				'taxon)))
	     (let* ((*print-case* :downcase)
		    (plist (union (get 'preds 'base)
				  (build-index-edb edb)))
		    (olist (union (get 'preds 'rec) plist))
		    (preds (union (get 'preds 'dbtaxon) olist)))
		   (putpropp 'preds plist 'base)
		   (putpropp 'preds preds 'all)
		   (if *fw-spy*
		       (show-spy preds))
		   (setq *semi-count* 0)
		   (all-steps preds nil)
		   (let ((facts (make-clauses preds)))
			(rem-props)
			(setq *derived-factbase* 
			      (set-difference (make-hn-clauses facts)
					      *factbase*
					      :test #'equal)))))))



(defun taxon-p ()
  (member :taxon *features*))



(defun init-taxon ()
  (putpropp 'taxon
	    (tx 'get-concept-names)
	    'concepts)
  (putpropp 'taxon
	    (tx 'get-attr-names)
	    'attributs))



(defun add-data-t (head)
  (eq (car head) 'add-data))



(defun data-t (head)
  (eq (car head) 'data))



;-------------------------------------------------------------------------------
;
; Dividing of the rules into
; 1. recursive and
; 2. not recursive rules
;
;-------------------------------------------------------------------------------
(defun recursive (idb)
  (recursive1 idb idb))



(defun recursive1 (rules idb)
  "Divides recursive from nonrecursive rules."
  (cond ((or (null rules) (null idb)) nil)
        (t
         (let* ((rule (car rules))
		(head (car rule))
		(m-nm (recursive1 (cdr rules) idb)))
	       (cond ((recursive-p (mc-premises rule) idb)
		      (update-property-lists head)
		      (list (cons rule (first m-nm))
			    (second m-nm)))
		     (t
		      (update-property-lists head)
		      (add-to-predlists (mc-premises rule))
		      (list (first m-nm)
			    (cons rule (second m-nm)))))))))



(defun update-property-lists (head)
  (if (taxon-conclusion-p head)
      (let* ((add-data-p (add-data-t head))
	     (head (if add-data-p
		       (cdr head)
		       head)))
	    (putpropp 'preds
		      (union (list (first head))
			     (get 'preds 'add-data))
		      'add-data)
	    (putpropp 'preds
		      (union (list (first head))
			     (get 'preds 'rec))
		      'rec))
      (putpropp 'preds
		(union (list (first head))
		       (get 'preds 'rec))
		'rec)))



(defun add-to-predlists (premises)
  (mapcar #'(lambda (x)
		    (if (and (literal-p x)
			     (not (backward-pred-p (car x))))
			(if (taxon-premise-p x)
			    (let ((tpp (taxon-premise-pred x)))
				 (putpropp 'preds
					   (union (list tpp)
						  (get 'preds 'data))
					   'data))
			    (putpropp 'preds
				      (union (list (first x))
					     (get 'preds 'base))
				      'base))))
		  premises))



(defun recursive-p (premises idb)
  "Tests whether a rule is recursive, i.e. contains a premise which is
  recursive."
  (and premises
       (let ((premise (car premises)))
	    (if (literal-p premise)
		(let ((succ (or (member (car premise)
					(get 'preds 'rec))
				(and (not (member (car premise)
						  (get 'preds 'base)))
				     (recursive-premise-p premise idb)))))
		     (or succ
			 (recursive-p (cdr premises) idb)))
		(recursive-p (cdr premises) idb)))))



(defun recursive-premise-p (premise idb)
  "Tests whether a premise is a recursive one, i.e. appearing as a head of a
  clause somewhere else in the idb."
  (and idb
       (let ((rule (car idb)))
	    (or (multiple-match-p (mc-head rule) premise)
		(recursive-premise-p premise (cdr idb))))))



(defun match-p (x y)
  "Tests for the same functor and arity."
  (and (eq (first x) (first y))
       (eql (length x) (length y))
       (putpropp 'preds
		 (union (list (car x))
			(get 'preds 'rec)
			:test #'equal)
		 'rec)))



;-------------------------------------------------------------------------------
;
; Additions to the bottom-up Interpreter to handle 
; multiple conclusion in the rules.
;
;-------------------------------------------------------------------------------
(defun m-conclusion-p (rule)
  "Tests whether a head contains multiple conclusions."
  (member '<- rule))



(defun multiple-match-p (conclusions premise)
  "Matches each conclusion to the premise."
  (and conclusions
       (let ((conclusion (car conclusions)))
	    (or (ext-match-p conclusion premise)
		(multiple-match-p (cdr conclusions) premise)))))



(defun mc-premises (rule)
  "Delivers the premises of a rule with one or multiple conclusions."
  (let ((res (member '<- rule)))
       (if res
           (cdr res)
           (cdr rule))))



(defun mc-head (rule)
  "Delivers a list of conclusion(s) of a rule with one or multiple
  conclusions in reverse order."
  (let ((res (member '<- (reverse rule))))
       (if res
           (cdr res)
           (list (car rule)))))



;-------------------------------------------------------------------------------
;
; Additions to the bottom-up Interpreter to integrate the taxonomical
; hierarchy TAXON.
;
;-------------------------------------------------------------------------------
(defun ext-match-p (head premise)
  "If we want to match data-premises to add-data conclusions, we have to 
  inspect also all upper concepts of the specific conclusion. Therefore these
  upper concepts have to be retrieved from the TAXON system."
  (if (and (taxon-premise-p premise)
           (taxon-conclusion-p head))
      (let ((tpp (taxon-premise-pred premise))
	    (tcp (taxon-conclusion-pred head)))
	   (or (eql tpp tcp)
	       (and (concept-p tpp)
		    (concept-p tcp)
	       (let ((class (or (get tcp 'upper-concepts)
				(retrieve-and-hold-classes tcp))))
		    (putpropp 'preds
			      (union (list tpp)
				     (get 'preds 'data))
			      'data)
		    (putpropp 'preds
			      (union (list tcp)
				     (get 'preds 'add-data))
			      'add-data)
		    (if (not (member tpp 
				     class 
				     :test #'equal))
			(let ((class (or (get tpp 'upper-concepts)
					 (retrieve-and-hold-classes tpp))))
			     (member tcp
				     class
				     :test #'equal)))))))
	   (match-p head premise)))



(defun concept-p (term)
  (member term (get 'taxon 'concepts)))



(defun taxon-premise-p (term)
  "Tests whether term is a taxon premise."
  (or (and (consp term)
	   ;(eql (length term) 4)        ;to be deleted soon
	   (eq (car term) 'data))
      (or (member (car term) 
		  (get 'taxon 'concepts))
	  (member (car term)
		  (get 'taxon 'attributs)))))



(defun taxon-premise-pred (term)
  (if (eq (first term) 'data)
      (second term)
      (first term)))



(defun taxon-conclusion-p (term)
  "Tests whether term is a taxon conclusion."
  (or (and (consp term)
	   ;(eql (length term) 4)        ;to be deleted soon
	   (eq (car term) 'add-data))
      (or (member (car term) 
		  (get 'taxon 'concepts))
	  (member (car term)
		  (get 'taxon 'attributs)))))

 

(defun taxon-conclusion-pred (term)
  (if (eq (first term) 'add-data)
      (second term)
      (first term)))



(defun retrieve-and-hold-classes (concept)
  "Holds the upper classes in a property list."
  (let ((classes (tx-upper-classes (list concept))))
       (putpropp concept classes 'upper-concepts)))



(defun tx-upper-classes (clist)
  "Computes the union of all upper concepts of given list of concepts." 
  (let ((upper-classes (tx-upper-classes-1 clist)))
       (and upper-classes
            (union upper-classes
                   (tx-upper-classes upper-classes)
                   :test #'equal))))



(defun tx-upper-classes-1 (clist)
  "Computes the immediate upper concepts of a list of concepts."
  (and clist
       (let ((upper-classes (tx 'im-uppers (car clist))))
            (union upper-classes
                   (tx-upper-classes-1 (cdr clist))
                   :test #'equal))))



(defun tx-all-instances ()
  "Get all instances from the hierarchy of the TAXON ABox."
  (tx 'get-instance-names))


#|
(defun tx-lower-classes (clist)
  "Computes the union of all lower concepts of given list of concepts." 
  (let ((lower-classes (tx-lower-classes-1 clist)))
       (and lower-classes
            (union lower-classes
                   (tx-lower-classes lower-classes)))))



(defun tx-lower-classes-1 (clist)
  "Computes the immediate lower concepts of a list of concepts."
  (and clist
       (let ((lower-classes (tx 'im-lowers (car clist))))
            (union lower-classes
                   (tx-lower-classes-1 (cdr clist))))))



|#
;-------------------------------------------------------------------------------
;
; Ordering of the head and the subgoals of the rules.
;
; function:     order-subgoals-all
; arguments:    rules
;               rec-rules : the rec-rules recursive rules
; returns:      a list with the ordered rules where they are now a list of
;               1. the head of the rule
;               2. the recursive premises
;               3. the non recursive ordinary premises followed by the
;                  built-in predicates
;               4. the level of the recursion
;
;-------------------------------------------------------------------------------
(defun order-subgoals-all (rules rec-rules)
  (and rules
       (let* ((rule (car rules))
	      (rem-lb (collect-builtins (mc-premises rule)))
              (r-n-l (order-subgoals (first rem-lb) rec-rules)))
         (cons (cons (mc-head rule)
                     (list (first r-n-l)
                           (append (second r-n-l) (second rem-lb))
                           (third r-n-l)))
               (order-subgoals-all (cdr rules) rec-rules)))))




(defun order-subgoals (subgoals rec-rules)
  (cond ((null subgoals) (list nil nil 0))
        (t
         (let ((r-n-level (order-subgoals (cdr subgoals) rec-rules)))
           (cond ((recursive-premise-p (car subgoals) rec-rules)
                  (list (cons (car subgoals) (first r-n-level))
                        (second r-n-level)
                        (+ 1 (third r-n-level))))
                 (t
                  (list (first r-n-level)
                        (cons (car subgoals) (second r-n-level))
                        (third r-n-level))))))))



(defun collect-builtins (subgoals)
  "Ordinary predicates and builtins are seperated."
  (and subgoals
       (let ((rem-b (collect-builtins (cdr subgoals)))
             (subgoal (car subgoals)))
         (cond ((or (lisp-builtin-p subgoal)
                    (backward-pred-p (car subgoal))
                    (eq 'false subgoal)
                    (eq 'is (car subgoal)))
                (list (first rem-b) (cons subgoal (second rem-b))))
               (t
                (list (cons subgoal (first rem-b))
                      (second rem-b)))))))



(defun collect-preds (rules)
  "All predicate names of conclusion of rules are collected."
  (remove-duplicates (collect-preds-1 rules)
                     :test #'equal))



(defun collect-preds-1 (rules)
  "The name after add-data is taken as the predname of those rules,
  otherwise the first item of head-literals are collected."
  (and rules
       (let ((rule (car rules)))
            (if (add-data-t (car rule))
                (cons (cadar rule)
                      (collect-preds-1 (cdr  rules)))
                (append (mapcar #'car (mc-head rule))
                        (collect-preds-1 (cdr rules)))))))



;-------------------------------------------------------------------------------
;
; Indexing of the facts
;
;-------------------------------------------------------------------------------
(defun build-index-edb (edb)
  (let ((preds (get 'preds 'dbtaxon)))
       ;(print (get 'preds 'dbtaxon))
       (if preds
	   (let* ((instances (tx-all-instances))
		  (tx-facts (retrieve-instances instances))
		  (edb1 (construct-new-factbase tx-facts edb)))
		 (build-index-edb-1 tx-facts)
		 (build-index-edb-1 edb1))
	   (build-index-edb-1 edb))))



(defun retrieve-instances (instances)
  (and instances
       (let ((facts (retrieve-instances-1 (car instances))))
	    (append facts
		    (retrieve-instances (cdr instances))))))



(defun retrieve-instances-1 (instance)
  (let* ((cc (tx 'concept-closure instance))
	 (cc-new (intersection (get 'preds 'dbtaxon)
			       cc))
	 (attrterm (fill-attributs instance)))
	(build-facts cc-new (list instance attrterm))))



(defun fill-attributs (instance)
  (let ((center1 (tx 'attr-filler 'center1 instance))
	(center2 (tx 'attr-filler 'center2 instance))
	(radius1 (tx 'attr-filler 'radius1 instance))
	(radius2 (tx 'attr-filler 'radius2 instance)))
       (list 'tup
	     (list 'tup 'center1 center1)
	     (list 'tup 'center2 center2)
	     (list 'tup 'radius1 radius1)
	     (list 'tup 'radius2 radius2))))



(defun build-facts (concepts rel)
  (and concepts 
       (cons (cons (car concepts)
		   rel)
	     (build-facts (cdr concepts) rel))))



(defun construct-new-factbase (tx-facts edb)
  (setq *factbase*
	(set-difference *factbase*
			(mapcar #'(lambda (x)
					  (list 'hn x))
				tx-facts)
			:test #'equal))
  (set-difference edb
		  tx-facts
		  :test #'equal))



(defun build-index-edb-1 (edb)
  (and edb
       (let ((res-pred (index-edb edb)))
         (cons (second res-pred) 
	       (build-index-edb-1 (first res-pred))))))



(defun index-edb (edb)
  (let ((pred-remedb (index1-edb (caar edb) (cdr edb))))
    (putpropp (caar edb) (cons (cdar edb) (first pred-remedb)) 'before)
    (putpropp (caar edb) (cons (cdar edb) (first pred-remedb)) 'all)
    (list (second pred-remedb) (caar edb))))



(defun index1-edb (sym edb)
  (and edb
       (let ((e-z (index1-edb sym (cdr edb))))
         (cond ((eq sym (caar edb))
                (list (cons (cdar edb) (first e-z)) (second e-z)))
               (t
                (list (first e-z) (cons (car edb) (second e-z))))))))



;-------------------------------------------------------------------------------
;
; Indexing of the the recursive rules
;
;-------------------------------------------------------------------------------
(defun build-index-idb-r (list number)
  "Recursive rules are indexed by the their recursive body predicates."
  (cond ((null list) number)
	(t
	 (let* ((term (car list))
		(head (first term))
		(recpremises (second term))
		(base (third term))
		(level (fourth term)))
	       (index-idb-r recpremises 
			    head 
			    recpremises
			    (remove-data base)
			    level
			    number)
	       (build-index-idb-r (cdr list) (1+ number))))))



(defun build-rule (head premises level)
  "Builds a rule of the form (head premises level)."
  (list head premises level))



(defun index-idb-r (triggers head premises-1 premises-2 level number)
  "Builds the final rule fo the form:
	(pattern (head <premises-1 - pattern + premises-2> level) number).
  The number is used for deletion of ground rules and to test whether a rule
  has already been applied."
  (and triggers
       (let* ((trigger (car triggers))
              (pattern (if (data-t trigger)
                           (cdr trigger)
                           trigger))
	      (new-premises-1 (remove pattern
				      (remove-data premises-1)
				      :test #'equal))
	      (new-premises (order-premises (cdr pattern)
					    new-premises-1))
              (tag (car pattern)))
             (putpropp tag
                      (union (list (list pattern 
					 (build-rule head
						     (append new-premises
							     premises-2)
						     level)
					 number))
			     (get tag 'rules)
                             :test #'equal)
                      'rules)
             (index-idb-r (cdr triggers) 
			  head
			  premises-1
			  premises-2
			  level
			  number))))



(defun remove-data (premises)
  (and premises
       (let ((premise (car premises)))
	    (cons (if (data-t premise)
		      (cdr premise)
		      premise)
		  (remove-data (cdr premises))))))



(defun order-premises (pattern premises)
  "Heuristiken zur Umordnung von Praemissen! not yet implemented."
  (declare (ignore pattern))
  premises)



;-------------------------------------------------------------------------------
; One Heuristic
;-------------------------------------------------------------------------------
;(defun order-premises (args premises)
;  "Heuristiken zur Umordnung von Praemissen!"
;  (let ((var-args (search-variables args)))
;       (order-premises-1 var-args premises)))



(defun order-premises-1 (vars premises)
  (let* ((list (build-first-list vars premises))
         (ordered-list (order-list list)))
        (resolve-list ordered-list)))



(defun build-first-list (vars premises)
  (and premises
       (let* ((premise (car premises))
              (args (cdr premise))
              (number-vars (count-bound-vars args vars)))
             (cons (list premise number-vars)
                   (build-first-list (cdr premise) vars)))))



(defun count-bound-vars (args bounds)
  (let ((vars (search-variables args)))
       (length (search-bounds vars bounds))))



(defun order-list (list)
  (sort list #'> :key #'second))



(defun resolve-list (list)
  (and list
       (let* ((item (first list))
              (same-rest (all-sames (cdr list) (second item)))
              (same (first same-rest))
              (rest (second same-rest))
              (sames (cons item same)))
             (if (consp same)
                 (let* ((assoc-list (build-second-list (mapcar #'car sames)))
                        (ordered-list (order-list assoc-list))
                        (ordered-premises (resolve-list-1 ordered-list)))
                       (append ordered-premises
                               (resolve-list rest)))
                 (cons (car item)
                       (resolve-list rest))))))



(defun all-sames (list key)
  (and list
       (let* ((same-rest (all-sames (cdr list) key))
              (same (first same-rest))
              (rest (second same-rest))
              (item (car list))
              (itemkey (second item)))
             (if (eql key itemkey)
                 (list (cons item
                             same)
                       rest)
                 (list same
                       (cons item
                             rest))))))



(defun build-second-list (premises)
  (and premises
       (let* ((premise (car premises))
              (number (length (search-variables (cdr premise)))))
             (cons (list premise number)
                   (build-second-list (cdr premises))))))



(defun resolve-list-1 (list)
  (mapcar #'car list))




;-------------------------------------------------------------------------------
;
; Indexing of the not recursive rules
;
;-------------------------------------------------------------------------------
(defun build-index-idb-nr
        (rules number)
  (and rules
       (progn (index-idb-nr (car rules) number)
              (build-index-idb-nr (cdr rules) (1+ number))
              t)))



(defun index-idb-nr
        (rule number)
  (let* ((premises (mc-premises rule))
         (newpremises (remove-builtins premises)))
        (index-idb-r newpremises
                     (mc-head rule)
                     premises
		     nil
                     0
                     number)))



(defun remove-builtins (premises)
  (and premises
       (let ((succ (remove-builtins (cdr premises)))
             (subgoal (car premises)))
            (cond ((or (lisp-builtin-p subgoal)
                       (backward-pred-p (car subgoal))
                       (eq 'false subgoal)
                       (eq 'is (car subgoal)))
                   succ)
                  (t
                   (cons subgoal succ))))))



;-------------------------------------------------------------------------------
;
; Show the indexed facts and rules, if spy is enabled.
;
;-------------------------------------------------------------------------------
(defun show-spy (preds)
  (princ "Facts :")
  (terpri)
  (mapcar #'(lambda (x) 
		    (let ((list (get x 'all))) 
			 (if list
			     (format t
				     "~2T ~A ~34T : ~A ~%"
				     x 
				     (car list)))
			 (print-rest (cdr list)))) 
	  preds)
  (princ "Rules :")
  (terpri)
  (mapcar #'(lambda (x)
		    (let ((list (get x 'rules)))
			 (if list 
			     (format t
				     "~2T ~A ~34T : ~A ~%"
				     x
				     (car list)))
			     (print-rest (cdr list))))
	   preds))



;-------------------------------------------------------------------------------
;
; Seminaive bottom-up evaluation
;
; function:     all-steps
; arguments:    preds : predicates appearing in the database
;               flag : nil for the first step, t for the second to nth step
;
;-------------------------------------------------------------------------------
(defun all-steps
       (preds flag)
  (subgoals-all preds nil flag)
  (let ((preds1 (get 'preds 'all))
	(rels (get 'rels 'data)))
       ;(pprint rels)
       (if rels 
	   (complete-cycle rels))
       (let ((bd (count-difference preds1)))
	    (or (null bd)
		(all-steps preds1 t)))))




;-------------------------------------------------------------------------------
;
; To given facts the applyable rules are searched
;
;-------------------------------------------------------------------------------
(defun subgoals-all
       (preds apprules flag)
  (cond ((null preds) apprules)
        (t
         (let ((facts (get (car preds) (if flag 'diff 'all)))
               (rules (get (car preds) 'rules))
	       (more-rules (if (member (car preds)
				       (get 'preds 'taxon)
				       :test #'equal)
			       (mapcar-not-nil #'(lambda (x)
							 (get x 'rules))
						    (switch-index
						     (car preds))))))
;	      (print more-rules)
              (cond ((or (null facts)
                         (null rules))
                     (subgoals-all (cdr preds) apprules flag))
                    (t
                     (let ((newrules (find-rules facts
						 (car preds)
						 rules
						 more-rules
						 apprules
						 flag
						 nil)))
			  (subgoals-all (cdr preds) newrules flag))))))))




(defun switch-index (pred)
  "Switches to upper classes for finding a rule, if the trigger is a 
  data predicate."
  (or (get pred 'upper-concepts)
      (retrieve-and-hold-classes pred)))



;-------------------------------------------------------------------------------
;
; Find the applyable rules
;
;-------------------------------------------------------------------------------
(defun find-rules
        (facts tag rules more-rules apprules flag appfacts)
  (cond ((null facts) apprules)
        (t
         (let* ((new-appfacts (cons (car facts) appfacts))
		(res (find-rules-1 (cons tag (car facts)) 
				   rules 
				   more-rules
				   apprules 
				   flag 
				   (cdr facts)
				   new-appfacts)))
	       (find-rules (cdr facts) 
			   tag 
			   rules 
			   more-rules
			   res 
			   flag
			   new-appfacts)))))


(defun find-rules-1
        (pattern rules more-rules apprules flag facts appfacts)
  (cond ((null rules) 
	 (if more-rules
	     (find-rules-1 pattern
			   (car more-rules)
			   (cdr more-rules)
			   apprules
			   flag
			   facts
			   appfacts)
	     apprules))
        (t
         (let ((rule-env-rem (find-rules-2 pattern rules)))
              (cond ((null rule-env-rem) apprules)
                    (t
                     (let* ((pat-rule-number (first rule-env-rem))
			    (rem (third rule-env-rem))
			    (number (third pat-rule-number))) 
			   (cond ((member number apprules :test #'eql)
				  (find-rules-1 pattern
						rem
						more-rules
						apprules
						flag 
						facts
						appfacts))
				 (t
				  (let* ((pat (first pat-rule-number))
					 (rule (second pat-rule-number))
					 (level (third rule))
					 (env (second rule-env-rem))
					 (stack (list 
						(list level
						      (set-difference
						       (get (car pattern) 
							    (if (and flag
								     (= level 1))
								'diff
								'all))
						       appfacts
						       :test #'equal)
						      '((bottom))
						      (cons pat
							    (second rule))
						      appfacts))))
					(if flag 
					    (semi-naive-evaluation rule
								   env
								   stack
								   number)
					    (naive-evaluation rule
							      env
							      stack
							      number)))
				  (find-rules-1 pattern
						rem
						more-rules
						(cons number apprules)
						flag 
						facts
						appfacts))))))))))


(defun find-rules-2 (pattern rules)
  (and rules
       (let ((env (unify pattern (de-inst (caar rules)) '((bottom)))))
	    (if env
		(list (car rules)
		      env
		      (cdr rules))
		(find-rules-2 pattern (cdr rules))))))



;-------------------------------------------------------------------------------
;
; For the first step of seminaive evalution naive evaluation is performed
;
;-------------------------------------------------------------------------------
(defun naive-evaluation
       (rule env stack number)
  (let* ((subgoals (second rule))
         (head (first rule))
         (ground (ground-list-p head))
         (rellist (unify-all-subgoals subgoals
				   0
				   env
				   stack
				   nil
				   ground
				   (mapcar #'cdr head))))
        (if (and ground rellist) (del-rule number (fourth (first stack))))
	(store-rels head rellist)))





;-------------------------------------------------------------------------------
;
; Seminaive evaluation for not lineare rules
;
;-------------------------------------------------------------------------------
(defun semi-naive-evaluation
        (rule env stack number)
  (let* ((subgoals (cadr rule))
         (head (car rule))
         (level (caddr rule))
         (ground (ground-list-p head))
         (rellist (differentiate level 
				 subgoals 
				 ground 
				 (mapcar #'cdr head) 
				 env 
				 stack)))
        (if (and ground rellist) (del-rule number (fourth stack)))
	(store-rels head rellist)))



(defun ground-p (struc)
  "Tests whether a structure is ground, i.e. contains no variables"
  (or (atom struc)
      (cond ((vari-t struc) nil)
            (t
             (and (ground-p (car struc))
                  (ground-p (cdr struc)))))))



(defun ground-list-p (heads)
  (or (null heads)
      (and (ground-p (car heads))
	   (ground-list-p (cdr heads)))))



(defun differentiate
        (level subgoals ground args env stack)
  (do ((st stack (let* ((elem (pop stack))
			(appfacts (fifth elem))
			(level n) )
		       (push (cons level
				   (cons (set-difference 
					  (get (caar (fourth elem))
					       (if (= level 1)
						   'diff
						   'all))
					  appfacts
					  :test #'equal)
					 (cddr elem)))
				   stack)))
       (n (1- level) (- n 1))
       (res nil (append (unify-all-subgoals subgoals
                                            n
                                            env 	;'((bottom))
					    st
                                            nil
                                            ground
                                            args) res)))
      ((or (and ground res) (< n 0)) res)))




(defun del-rule (number premises)
  (and premises
       (let ((premise (car premises)))
            (if (consp premise)
                (let* ((tag (car premise))
                       (rules (get tag 'rules)))
                      (if rules
                          (putpropp tag
                                    (remove number
                                            rules
                                            :test #'del-equal)
                                    'rules))))
            (del-rule number (cdr premises)))))



(defun del-equal (x y)
  (eql x (third y)))



;-------------------------------------------------------------------------------
;
; Additions for inserting facts into the taxonomical hierarchy.
;
;-------------------------------------------------------------------------------
(defun store-rels (conclusions rellist)
  (and conclusions 
       (let ((conclusion (car conclusions))
	     (rels (mapcar #'car rellist))
	     (newrellist (mapcar #'cdr rellist)))
	    (if (taxon-conclusion-p conclusion)
		(handle-taxon-conclusion (if (add-data-t conclusion)
					     rels
					     (mapcar #'(lambda (x)
							       (cons 
								(car conclusion)
								x))
						     rels)))
		(putpropp (car conclusion)
;			  (remove-duplicates
			   (union (remove-duplicates rels :test #'equal)
				  (get (car conclusion) 'after)
				  :test #'equal)
;			   :test #'equal)
			  'after))
	    (store-rels (cdr conclusions) newrellist))))



(defun handle-taxon-conclusion (rels)
  (let ((rels1 (remove-duplicates rels
				  :test #'equal)))
       (keep-taxon-rels rels1)))



(defun keep-taxon-rels (rels)
  (putpropp 'rels
	    (union rels
		   (get 'rels 'data)
		   :test #'equal)
	    'data))



(defun complete-cycle (rels)
  (let* ((recs (get 'preds 'dbtaxon))
	 (rels1 (add-to-taxon rels recs)))
	(retrieve-closure rels1 recs)
	(remprop 'rels 'data)))



(defun add-to-taxon (rels recs)
  (and rels
       (let* ((rel (car rels))
              (res (tx 'assert-ind? (second rel)
                                    (first rel)
                                    (resolve-args (cdr (third rel))))))
	     (cond (res
		    (cons rel
			  (add-to-taxon (cdr rels) recs)))
		   (t
		    (add-to-taxon (cdr rels) recs))))))



(defun resolve-args (args)
  (and args
       (let ((arg (car args)))
	    (cons (cdr arg)
		  (resolve-args (cdr args))))))



(defun retrieve-closure (rels recs)
  (and rels
       (let* ((rel (car rels))
	      (cc (tx 'concept-closure (second rel)))
	      (cc-new (intersection cc
				   recs)))
	     (if cc-new
		 (index-tx-rel cc (cdr rel)))
	     (retrieve-closure (cdr rels) recs))))


(defun index-tx-rel (concepts rel)
  (and concepts
       (let ((pred (car concepts)))
         (putpropp pred
                   (union (list rel)
                          (get pred 'after)
                          :test #'equal)
                   'after)
         (index-tx-rel (cdr concepts) rel))))




;-------------------------------------------------------------------------------
;
; Extraction of final terms, is-terms and built-in predicates
;
;-------------------------------------------------------------------------------
(defun unify-all-subgoals
        (subgoals level env stack flag ground args)
  (cond ((null subgoals)
         (cond ((or ground
                    (null stack))
                (list (ultimate-instant args env)))
               (t
                (cons (ultimate-instant args env)
                      (popp stack ground args)))))
        (t
         (let ((term (ultimate-opis-assoc (car subgoals) env)))
           (cond ((eq 'false term) nil)
                 ((final-p term)
                  (unify-all-subgoals (cdr subgoals)
                                      level
                                      env
                                      stack
                                      flag
                                      ground
				      args))

                 ((or (is-t term)
                      (lisp-builtin-p (car term))
                      (backward-pred-p (car term)))
;                 (print term)
                  (let* ((userline (append subgoals (list (list 'inst args)))))
;                       (print userline)
                        (let (
                         (res (and-process (deanon-request
                                            (if *rfi-static*
                                                (flatten-request userline)
                                                userline))
                                           env
                                           (list (let(;(p (get 'preds 'dbtaxon))
						      (preds (get 'preds
                                                                   ;'base
								   'all)))
                                                      (make-hn-clauses
                                                       (make-clauses
							 preds)))
                                                 *fw-prelude*
                                                 *rfi-database*
                                                 )
                                           1
                                           'tupof)))
;                       (print res)
                        (cond ((or ground
                                   (null stack))
                               res)
                              (t
                               (append res (popp stack ground args)))))))
;                ((is-t term)
;                 (let ((res (cond ((final-p (un-is term))
;                                   (un-is term))
;                                  ((lisp-builtin-p (car (un-is term)))
;                                   (lisp-exec (un-is term) env))
;                                  (t 'false))))
;                   (cond ((eq res 'false)
;                          (and stack (popp stack ground)))
;                         (t
;                          (let ((isenv (unify (de-inst1 (second term))
;                                              res
;                                              env)))
;                            (cond ((null isenv)
;                                   (and stack (popp stack ground)))
;                                  (t
;                                   (unify-all-subgoals (cdr subgoals)
;                                                       level
;                                                       isenv
;                                                       stack
;                                                       flag
;                                                       ground))))))))
;                ((lisp-builtin-p (car term))
;                 (cond ((eq (lisp-exec term env) 'false)
;                        (and stack (popp stack ground)))
;                       (t
;                        (unify-all-subgoals (cdr subgoals)
;                                            level
;                                            env
;                                            stack
;                                            flag
;                                            ground))))
                 (t
                  (unify-one-subgoal subgoals
                                     level
                                     env
                                     stack
                                     flag
                                     ground
                                     args)))))))


(defun de-inst1
       (x)
  (cond ((atom x) x)
        ((inst-t x) (de-inst1 (cadr x)))
        (t (cons (de-inst1 (car x))
                 (de-inst1 (cdr x))))))





;-------------------------------------------------------------------------------
;
; Take the not evaluated piece from the stack and go on
;
;-------------------------------------------------------------------------------
(defun popp
        (stack ground args)
  (let* ((item (pop stack))
	 (level (first item))
	 (fb (second item))
	 (env (third item))
	 (sb (fourth item)))
	(unify-all-subgoals sb level env stack fb ground args)))



;-------------------------------------------------------------------------------
;
; generalized unifycation procedure
;
;-------------------------------------------------------------------------------
(defun unify-one-subgoal
       (subgoals level env stack flag ground args)
  (let ((db (or flag (get (caar subgoals)
                          (cond ((> level 1) 'all)
                                ((= level 1) 'diff)
                                (t 'before))))))
    (cond ((null db)
           (and stack (popp stack ground args)))
          (t
           (let ((newenv (match (de-inst1 (cdar subgoals))
                                (car db)
                                env)))
             (cond ((null (cdr db))
                    (cond ((null newenv)
                           (and stack (popp stack ground args)))
                          (t
                           (unify-all-subgoals (cdr subgoals)
                                               (- level 1)
                                               newenv
                                               stack
                                               nil
                                               ground
                                               args))))
                   (t
                    (cond ((null newenv)
                           (unify-one-subgoal subgoals
                                              level
                                              env
                                              stack
                                              (cdr db)
                                              ground
                                              args))
                          (t
			   (let ((item (list level
					     (cdr db)
					     env
					     subgoals)))
                             (unify-all-subgoals (cdr subgoals)
                                                 (- level 1)
                                                 newenv
                                                 (push item stack)
                                                 nil
                                                 ground
                                                 args)))))))))))



;-------------------------------------------------------------------------------
;
; Pattern matching
;
;-------------------------------------------------------------------------------
(defun match (x y environment)
  (let ((x (ultimate-assoc x environment)))
       (cond ((equal x y) environment)
             ((anonymous-p x)
              environment)
             ((vari-t x) (cons (list x y) environment))
             ((or (atom x) (atom y)) nil)
             (t
              (let ((new-environment (match (car x)
                                            (car y)
                                            environment)))
                   (and new-environment
                        (match-args (cdr x) (cdr y) new-environment)))))))



(defun match-args (x y environment)
  (let ((x (ultimate-assoc x environment)))
       (cond ((equal x y) environment)
             ((anonymous-p x)
              environment)
             ((vari-t x) (cons (list x y) environment))
             ((bar-t x)
              (match (cadr x) (cons 'tup y) environment))
             ((or (atom x) (atom y)) nil)
             (t
              (let ((new-environment (match (car x)
                                            (car y)
                                            environment)))
                   (and new-environment
                        (match-args (cdr x) (cdr y) new-environment)))))))



;-------------------------------------------------------------------------------
;
; counts whether there are new facts derived or not
;
;-------------------------------------------------------------------------------
(defun count-difference (preds)
  (setq *semi-count* (1+ *semi-count*))
  (if *fw-spy*
      (format t "Difference ~A : ~%" *semi-count*))
  (count-difference1 (mapcar #'count-difference2 preds)))




(defun count-difference1 (list)
  (and list
       (or (car list) (count-difference1 (cdr list)))))



(defun count-difference2 (x)
  (let* ((diff (set-difference (get x 'after) (get x 'all) :test #'equal))
	 (before (get x 'all))
	 (*print-case* :downcase)
	 (all (append diff before)))
	(if *fw-spy*
	    (let ((item (car diff)))
		 (format t "~2T ~A ~34T : ~A ~%" x item)
		 (print-rest (cdr diff))))
	(remprop x 'after)
	(putpropp x before 'before)
	(putpropp x all 'all)
	(putpropp x diff 'diff)))




(defun print-rest (list)
  (and list
       (let ((item (car list)))
            (format t "~34T   ~A ~%" item)
            (print-rest (cdr list)))))


;-------------------------------------------------------------------------------
;
; Building facts from the relations
;
;-------------------------------------------------------------------------------
(defun make-clauses (preds)
  (and preds
       (append (mapcar #'(lambda (x) (cons (car preds) x))
                       (get (car preds) 'all))
               (make-clauses (cdr preds)))))



(defun rem-props ()
;  (break)
  (rem-props1 (get 'preds 'all))
  (rem-props2 (get 'preds 'dbtaxon))
  (remprop 'taxon 'concepts)
  (remprop 'taxon 'attributs)
  (remprop 'rels 'data)
  (remprop 'preds 'all)
  (remprop 'preds 'base)
  (remprop 'preds 'rec)
  (remprop 'preds 'data)
  (remprop 'preds 'add-data)
  (remprop 'preds 'dbtaxon)
  (remprop 'preds 'taxon))



(defun rem-props1 (preds)
  (mapcar #'(lambda (x) (remprop x 'before)
                        (remprop x 'rules)
                        (remprop x 'all)) preds))


(defun rem-props2 (preds)
  (mapcar #'(lambda (x) (remprop x 'upper-concepts))
          preds))


;-------------------------------------------------------------------------------
;
; Instantiation of the conclusions
;
;-------------------------------------------------------------------------------
;(defun make-head
;       (head envlist)
;  (and envlist
;       (cons (ultimate-instant head (car envlist))
;             (make-head head (cdr envlist)))))



(defun make-hn-clauses (facts)
  (mapcar #'(lambda (x) (list 'hn x)) facts))

