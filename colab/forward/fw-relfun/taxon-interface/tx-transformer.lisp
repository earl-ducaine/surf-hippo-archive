;; Transformer of implicite taxon concepts to explicite (with add-data/data) taxon concepts

(defun impl2expl_db (db list_of_concepts)
  (mapcar #'(lambda(cl)
	      (impl2expl_clause cl list_of_concepts))
	  db))

(defun impl2expl_clause (clause list_of_concepts)
  (let ((concs (mapcar #'(lambda(cc)
			   (impl2expl_conc cc list_of_concepts))
		       (conclusions clause)))
	(prems (mapcar #'(lambda(pr)
			   (impl2expl_prem pr list_of_concepts))
		       (premisses clause)))
	(tag (car clause)))
    
    (if (= 1 (length concs))
	(cons tag (append concs prems))
      (cons tag (append concs (list arrow) prems)))))


(defvar *expl_tx_conc* 'add-data)
(defun impl2expl_conc (conc list_of_concepts)
  (if (member (car conc)
	      list_of_concepts)
      (cons *expl_tx_conc* conc)
    conc))


(defvar *expl_tx_prem* 'data)
(defun impl2expl_prem ( prem list_of_concepts)
  (if (member (car prem)
	      list_of_concepts)
      (cons *expl_tx_prem* prem)
    prem))



;;; complex attribute terms into simple attribute terms
;;;
;;; complex_clause =({rl|up} {<conc>|<conc_1> ... <conc_N> <-} <prem_1> ... <prem_M>}
;;; <conc> = ( <concept_name> <concept_identifier> <attr_terms> )
;;; <concept_name> = lisp_symbol
;;; <concept_identifier> = lisp_expr
;;; <attr_terms> = (tup <attr_term_1> ...  <attr_term_N>)
;;; <attr_term>  = (tup <attr_name_1> <attr_value_N> )
;;; <attr_name>  = lisp_symbol
;;; <attr_value> = lisp_expr
;;;
;;; <prem> = { <expression_wich_calls_a_complex_clause> | something else }
;;; <expression_wich_calls_a_complex_clause> =
;;;         (<concept_name> <concept_identifier> (inst (tup <attr_term_1> ... <attr_term_N>)))       
;;; is transformed into simple attribute term 

;;; simple_clause =({rl|up} <conc_0> ... <conc_N> <- <Cprem_0> ... <Cprem_N> <prem_1> ... <prem_M>)
;;; <conc_0> = (<concept_name> <concept_identifier>)
;;; <conc_1..N> = (<attr_name_1..N> <concept_identifier> <attr_value_1..N>)
;;; <Cprem0..N> = <conc_0..N>
;;; <prem> = if complex_call:
;;;         (<concept_name> <concept_identifier>)
;;;         (<attr_name_1..N> <concept_identifier> <attr_value_1..N>)
;;;         else unchanged

;;; Optimization: not yet implemented
;;;    general: only the Cprem_i which <attr_value_i> is used in <prem_1..M> are generated
;;;             double Cprem_i (in the semantics meaning) are removed 

(defun compl2simpl_clause(clause list_of_concepts list_of_attributes)
  (let* ((concs (conclusions clause))
	 (prems (premisses clause))
	 (new_concs (append-list-elements
		     (mapcar #'(lambda(conc)
				 (compl2simpl_conc conc list_of_concepts list_of_attributes))
			     concs)))
	 (new_prems (append-list-elements
		     (mapcar #'(lambda(prem)
				 (compl2simpl_prem prem list_of_concepts list_of_attributes))
			     prems)))
	 (tag (car clause))
	)
    (cons tag (append new_concs `(,arrow) new_concs new_prems))))

(defun compl2simpl_conc(conc list_of_concepts list_of_attributes )
  (if (complex_conc_p conc list_of_concepts list_of_attributes)
      (let* ((concept_name (car conc))
	     (concept_identifier (cadr conc))
	     (attr_terms (cdr(caddr conc)))  ;; cdr to remove leading tup
	     )
	(cons `(,concept_name ,concept_identifier)
	      (mapcar #'(lambda(attr)
			  (compl2simpl_attr attr concept_identifier))
		      attr_terms)))
    (list conc)))


(defun compl2simpl_attr(attr_term concept_identifier)
  (let ((attr_name (cadr attr_term))
	(attr_value (caddr attr_term)))
    `(,attr_name ,concept_identifier ,attr_value)))

(defun compl2simpl_prem(prem list_of_concepts list_of_attributes)
  (if (complex_prem_p prem list_of_concepts list_of_attributes)
      (let* ((concept_name (car prem))
	     (concept_identifier (cadr prem))
	     (attr_terms (cdar(cdr(caddr prem))))  ;;; remove the inst predicate
	     )
	(cons `(,concept_name ,concept_identifier)
	      (mapcar #'(lambda(attr)
			  (compl2simpl_attr attr concept_identifier))
		      attr_terms)))
    (list prem)))


(defun complex_conc_p (conc list_of_concepts list_of_attributes)
  (and (= (length conc) 3)
       (symbolp (car conc)) ;; concept_name
       (member (car conc) list_of_concepts)
       (listp (caddr conc)) ;; attr_terms
       (attr_terms_p (caddr conc) list_of_attributes )))

(defun complex_prem_p (conc list_of_concepts list_of_attributes)
  (and (= (length conc) 3)
       (symbolp (car conc)) ;; concept_name
       (listp (caddr conc))
       (= (length (caddr conc)) 2) ;; (inst (tup ...))
       (eq (caaddr conc) 'inst)
       (attr_terms_p (cadr(caddr conc)) list_of_attributes )))

(defun attr_terms_p(attr_terms list_of_attributes )
  (and (listp attr_terms)
       (eq 'tup (car attr_terms))
       (eval (cons 'and (mapcar #'(lambda(attr)
				    (attr_term_p attr list_of_attributes))
				(cdr attr_terms))))))

(defun attr_term_p(attr_term list_of_attributes)
  (if (and (listp attr_term)
	   (= (length attr_term) 3)
	   (eq (car attr_term) 'tup)
	   (symbolp (cadr attr_term))
	   (member (cadr attr_term) list_of_attributes))
      t
    nil))