;*******************************************************************************
;*
;*	Basic structures for the representation of concepts in CNF
;*
;*******************************************************************************

;;; compiler instructions

(proclaim '(inline get-QPCs get-exec-QPCs get-rules get-neg-rules
		   ))


;*******************************************************************************
;*
;*	Conjunctive Normal Form
;*
;*******************************************************************************

(defstruct (CNF (:include cnf-help)
		(:print-function print-cnf)
		(:predicate cnf?)
		)
  (rules 		nil :type list)
  (neg-rules    	nil :type list)
  (QPC			nil :type list)
  (exec-QPC		nil :type list)
  (node			nil )
  )

(defun add-to-rules! (cnf term)
  (pushnew term (cnf-rules cnf)))
(defun add-to-neg-rules! (cnf term)
  (pushnew term (cnf-neg-rules cnf)))
(defun add-to-quantifier-path-C! (cnf term)
  (pushnew term (cnf-QPC cnf)))
(defun add-to-exec-QPC! (cnf qpc)
  (pushnew qpc (cnf-exec-QPC cnf)))

(defun delete-QPC! (cnf qpc)
  (setf (cnf-QPC cnf) 
	(delete qpc (cnf-QPC cnf))))

(defun get-QPCs (cnf)
  (cnf-QPC cnf))
(defun get-exec-QPCs (cnf)
  (cnf-exec-QPC cnf))
(defun get-rules (cnf)
  (cnf-rules cnf))
(defun get-neg-rules (cnf)
  (cnf-neg-rules cnf))

(defun set-node! (cnf node)
  (setf (cnf-node cnf) node))
(defun get-node (cnf)
  (cnf-node cnf))

;;; search functions

(defun search-one-restr-with-equal-path (cnf path)
  ;;; and return this restriction
  (dolist (restriction (cnf-QPC cnf))
	  (if (equal (get-path-of-conceptname restriction)
		     path)
	      (return restriction)))
  )

(defun search-one-value-restr-with-equal-path (cnf path)
  ;;; and return this restriction
  (dolist (restriction (cnf-QPC cnf))
	  (if (and (equal (get-path-of-conceptname restriction)
			  path)
		   (value-restriction? restriction))
	      (return restriction))))

(defun search-all-exists-in-restr-with-equal-path (cnf path)
  ;;; and return these restrictions
  (do ((restrictions (cnf-QPC cnf) (cdr restrictions))
       (result nil)
       )
	((null restrictions) result)
	(if (and (equal (get-path-of-conceptname (car restrictions))
			path)
		 (exists-in-restriction? (car restrictions)))
	    (setq result (cons (car restrictions) result))))
  )

;*******************************************************************************
;*
;*	Links from Conceptnames to CNFs and vice versa	
;*
;*******************************************************************************

(defvar *concept-table (make-hash-table))

(defun get-CNF-of-conceptname (name)
  (gethash name *concept-table)
  )

(defun put-CNF-of-conceptname! (name cnf)
  (setf (gethash name *concept-table) cnf)
  )

(defvar *conceptname-table (make-hash-table))

(defun get-conceptname-of-cnf (cnf)
  (gethash cnf *conceptname-table)
  )
(defun put-conceptname-of-cnf! (cnf name)
  (setf (gethash cnf *conceptname-table) name)
  )

(defun clear-name<-->cnf ()
  (let ((top (make-cnf-of-top))
	(bottom (make-cnf-of-bottom))
	)
       (clrhash *concept-table)
       (clrhash *conceptname-table)
       (put-cnf-of-conceptname! 'top top)
       (put-cnf-of-conceptname! 'bottom bottom)
       (put-conceptname-of-cnf! top 'top)
       (put-conceptname-of-cnf! bottom 'bottom)
       ))


;************************************************************
;*                                                          *
;*               Created Concepts	 
;*                                                          *
;************************************************************


(defvar *created-concepts (make-hash-table))

(defun set-created-concept! (name)
  (setf (gethash name  *created-concepts) t))

(defun created-conceptname? (name)
  (gethash name  *created-concepts))

;*******************************************************************************
;*
;*	Help Functions
;*
;*******************************************************************************

#|
(defun add-conjunction-of-conceptnames! (concept term)
  (cond ((conjunction? term)
	 (dolist (arg (get-concepts-of-conjunction term))
		 (add-conjunction-of-conceptnames! concept arg)))
	(t (add-to-conceptnames! concept (get-CNF-of-conceptname term)))
	))
|#

(defun replace-quantifier! (quantifier restriction)
  ;;; Changes the quantifier of the restriction
  ;;; quantifier      : some 
  ;;; restriction     : (forall path C)
  ;;; --> restriction : (some path C)
  (rplaca restriction quantifier))



(defun add-to-possibles! (cnf term)
  (cond ((or (disjunction? term)
	     (conjunction? term))
	 (dolist (ct (get-concepts-of-disjunction term))
		 (add-to-possibles! cnf ct)))
	((conceptname? term)
	 (let ((cnf-of-name (get-CNF-of-conceptname term))
	       )
	      (add-list-to-possible-primitives! cnf (get-pos-possible-prims cnf-of-name))
	      (add-list-to-possible-primitives! cnf (get-pos-definitive-prims cnf-of-name))
	      (add-list-to-neg-possible-primitives! cnf (get-neg-possible-prims cnf-of-name))
	      (add-list-to-neg-possible-primitives! cnf (get-neg-definitive-prims cnf-of-name))
	      (add-list-to-forall-links! cnf (get-forall-links cnf-of-name))
	      (add-list-to-exists-links! cnf (get-exists-links cnf-of-name))
	      (add-list-to-possible-families! cnf (get-pos-possible-families  cnf-of-name))
	      (add-list-to-possible-families! cnf (get-pos-definitive-families cnf-of-name))
	      (add-list-to-neg-possible-families! cnf (get-neg-possible-families  cnf-of-name))
	      (add-list-to-neg-possible-families! cnf (get-neg-definitive-families cnf-of-name))
	      (if (get-cnf-agree cnf-of-name)
		  (set-agree! cnf))
	      (if (get-cnf-disagree cnf-of-name)
		  (set-disagree! cnf))
	      ))
	((negation? term)
	 (let ((nc (get-concept-of-negation term))
	       )
	      (cond ((conceptname? nc)
		     (let ((cnf-of-name (get-CNF-of-conceptname nc))
			   )
			  (add-list-to-possible-primitives! cnf (get-neg-possible-prims cnf-of-name))
			  (add-list-to-possible-primitives! cnf (get-neg-definitive-prims cnf-of-name))
			  (add-list-to-neg-possible-primitives! cnf (get-pos-possible-prims cnf-of-name))
			  (add-list-to-neg-possible-primitives! cnf (get-pos-definitive-prims cnf-of-name))
			  (add-list-to-forall-links! cnf (get-exists-links cnf-of-name))
			  (add-list-to-exists-links! cnf (get-forall-links cnf-of-name))
			  (add-list-to-possible-families! cnf (get-neg-possible-families  cnf-of-name))
			  (add-list-to-possible-families! cnf (get-neg-definitive-families cnf-of-name))
			  (add-list-to-neg-possible-families! cnf (get-pos-possible-families  cnf-of-name))
			  (add-list-to-neg-possible-families! cnf (get-pos-definitive-families cnf-of-name))
			  (if (get-cnf-agree cnf-of-name)
			      (set-disagree! cnf))
			  (if (get-cnf-disagree cnf-of-name)
			      (set-agree! cnf))
			  ))
		    ((exists-in-restriction? nc)
		     (add-list-to-forall-links! cnf (get-first-relation-list (get-pathes-of-restriction nc))))
		    ((value-restriction? nc)
		     (add-list-to-exists-links! cnf (get-first-relation-list (get-pathes-of-restriction nc))))
		    ((agreement? nc)
		     (add-list-to-exists-links! cnf (get-first-relation-list (get-pathes-of-agreement nc)))
		     (if (agree-with-empty-path? nc)
			 (set-disagree! cnf)))
		    ((disagreement? nc)
		     (add-list-to-forall-links! cnf (get-first-relation-list (get-pathes-of-agreement nc)))
		     (if (agree-with-empty-path? nc)
			 (set-agree! cnf)))
		    ((family? nc)
		     (add-family-to-neg-possible-families! cnf nc))
		    ((sprim? nc)
		     (add-sprim-to-neg-possible-families! cnf nc))
		    ((prim? nc)
		     (add-to-neg-possible-primitives! cnf nc))
		    (t (error "Unknown negated conceptterm in Add-Possibles!"))
		    )))
	      ((exists-in-restriction? term)
	       (add-list-to-exists-links! cnf (get-first-relation-list (get-pathes-of-restriction term))))
	      ((value-restriction? term)
	       (add-list-to-forall-links! cnf (get-first-relation-list (get-pathes-of-restriction term))))
	      ((agreement? term)
	       (add-list-to-forall-links! cnf (get-first-relation-list (get-pathes-of-agreement term)))
	       (if (agree-with-empty-path? term)
		   (set-agree! cnf)))
	      ((disagreement? term)
	       (add-list-to-exists-links! cnf (get-first-relation-list (get-pathes-of-agreement term)))
	       (if (agree-with-empty-path? term)
		   (set-disagree! cnf)))
	      ((family? term)
	       (add-family-to-possible-families! cnf term))
	      ((sprim? term)
	       (add-sprim-to-possible-families! cnf term))
	      ((prim? term)
	       (add-to-possible-primitives! cnf term))
	      ))



(defun contains-agreement-with-empty-path? (ct)
  (cond ((or (disjunction? ct)
	     (conjunction? ct))
	 (funcall #'or-fct
		  (mapcar #'contains-agreement-with-empty-path?
			  (get-concepts-of-disjunction ct))))
	((conceptname? ct)
	 (get-cnf-agree (get-CNF-of-conceptname ct)))
	((agreement? ct)
	 (agree-with-empty-path? ct))
	))

(defun contains-disagreement-with-empty-path? (ct)
  (cond ((or (disjunction? ct)
	     (conjunction? ct))
	 (funcall #'or-fct 
		  (mapcar #'contains-disagreement-with-empty-path?
			  (get-concepts-of-disjunction ct))))
	((conceptname? ct)
	 (get-cnf-disagree (get-CNF-of-conceptname ct)))
	((agreement? ct)
	 (disagree-with-empty-path? ct))
	))

(defun make-some-P-Conj (path c1 c2)
  (make-exists-in-restriction path (make-conjunction (list c1 c2))))
(defun make-forall-P-Conj (path c1 c2)
  (make-value-restriction path (make-conjunction (list c1 c2))))
(defun make-Q-P-Conj (quantifier path c1 c2)
  (cond ((some? quantifier) (make-some-P-Conj path c1 c2))
	((forall? quantifier) (make-forall-P-Conj path c1 c2))
	))

(defun or-fct (list)
  (dolist (el list)
	  (if el
	      (return t))))

(defun my-union (&rest list)
  (do ((res (car list))
       (list (cdr list) (cdr list))
       )
      ((null list) res)
      (setq res (union res (car list)))
      ))
