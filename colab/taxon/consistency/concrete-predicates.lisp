;*******************************************************************************
;*
;*	File : Propagate Concrete Predicates
;*
;*******************************************************************************

(defvar *print-complex-cpred nil)

(defun propagate-concrete-predicates (pred all-args)
  (let ((cpred (get-internal-rep-of-cpred pred))
	)
       (dolist (args all-args)
  (if *print-complex-cpred 
      (format t "~%Complex Predicate Call : ~A ------ Args : ~A"
		     pred (mapcar #'get-id args)))
	  (apply cpred args '()))))

(defun propagate-neg-concrete-predicates (pred all-args)
  (let ((cpred (get-neg-internal-rep-of-cpred pred))
	)
       (dolist (args all-args)
  (if *print-complex-cpred 
      (format t "~%Complex Predicate Call : ~A ------ Args : ~A"
	      pred (mapcar #'get-id args)))
	  (apply cpred args '()))))

(defun propagate-single-predicate (pred)
  (apply (get-internal-rep-of-cpred (get-pred-of-int-repr pred))
	 (mapcar #'deref (get-all-arguments pred))
	 '()))

(defun propagate-cond-single-predicate (pred)
  (let ((args (mapcar #'deref (get-all-arguments pred)))
	(predicate (get-pred-of-int-repr pred))
	)
       (if (all-args-have-the-right-domain? args (get-domain-of-cpred predicate))
	   (apply (get-neg-internal-rep-of-cpred predicate) args '()))))

(defun all-args-have-the-right-domain? (args domain)
  (dolist (arg args t)
	  (unless (and (concrete? arg) (eq domain (get-domain arg)))
		  (return nil))))
;*******************************************************************************
;*
;*	File : Make-Predicates
;*
;*******************************************************************************

(defun make-complex-predicate-rule (args body)
  (make-complex-predicate (body->tx-body args body)))

(defun make-complex-predicate (body)
  #'(lambda (args) (apply body args '())))

(defun body->tx-body (args body)
  (cond ((conjunction? body)
	 (make-conjunction-of-cpreds-rule (mapcar #'(lambda (l) (body->tx-body args l))
						  (get-concepts-of-conjunction body))))
	((disjunction? body)
	 (make-disjunction-of-cpreds-rule (mapcar #'(lambda (l) (body->tx-body args l))
						  (get-concepts-of-conjunction body))))
	((negation? body)
	 (negated-body->tx-body args (get-concept-of-negation body)))
	((RO-predicate? (get-C-predicate body))
	 (make-simple-CP (RO-predicate->int-RO-pred (get-C-predicate body))
			 (mapcar #'(lambda (arg) (get-access-function args arg))
				 (get-C-arguments body))))
	((CONTAX-predicate? (get-C-predicate body))
	 (make-simple-CP (CONTAX-predicate->int-CONTAX-pred (get-C-predicate body))
			 (mapcar #'(lambda (arg) (get-access-function args arg))
				 (get-C-arguments body))))
	(t (error "Unknown predicate in body->tx-body"))
	))

(defun negated-body->tx-body (args body)
  (cond ((conjunction? body)
	 (make-disjunction-of-cpreds-rule (mapcar #'(lambda (l) (body->tx-body args
									       (make-negation l)))
						  (get-concepts-of-conjunction body))))
	((disjunction? body)
	 (make-conjunction-of-cpreds-rule (mapcar #'(lambda (l) (body->tx-body args
									       (make-negation l)))
						  (get-concepts-of-disjunction body))))
	((negation? body)
	 (body->tx-body args (get-concept-of-negation body)))
	((RO-predicate? (get-C-predicate body))
	 (make-simple-CP (negated-RO-predicate->int-RO-pred (get-C-predicate body))
			 (mapcar #'(lambda (arg) (get-access-function args arg))
				 (get-C-arguments body))))
	((CONTAX-predicate? (get-C-predicate body))
	 (make-simple-CP (negated-CONTAX-predicate->int-CONTAX-pred (get-C-predicate body))
			 (mapcar #'(lambda (arg) (get-access-function args arg))
				 (get-C-arguments body))))
	(t (error "Unknown predicate in negated-body->tx-body"))
	))


(defun make-simple-CP (predicate accesslist)
  #'(lambda (args) (apply predicate (apply-list-of-functions accesslist args))))
(defun apply-list-of-functions (function-list args)
  (mapcar #'(lambda (function) (funcall function args))
	  function-list))

;;; rules

(defun make-conjunction-of-cpreds-rule (conj)
  #'(lambda (args) (mapcar #'(lambda (fcts) (apply fcts args '())) conj))) 

(defun make-disjunction-of-cpreds-rule (disj)
  #'(lambda (args) (push-concepts-on-or! 
		    (mapcar #'(lambda (call) (make-call-fct call args))
			    (make-predicate-calls disj)))))
(defun make-call-fct (call args)
  #'(lambda () (apply call args '())))

(defun make-predicate-calls (disj)
  (do ((disj disj (cdr disj))
       (result nil)
       )
      ((null disj) (reverse result))
      (setq result (cons (make-predicate-call (car disj))
			 result))))
(defun make-predicate-call (pred)
  #'(lambda (args) (apply pred args '())))


;*******************************************************************************
;*
;*	Help Functions
;*
;*******************************************************************************

(defun get-access-function (args arg)
  (let ((pos (position arg args))
	)
       (if pos
	   #'(lambda (arguments) (nth pos arguments))
	   #'(lambda (dummy) arg))
       ))

(defun get-C-predicate (body)
  (car body))
(defun get-C-arguments (body)
  (cdr body))

(defun get-first-RO-arg (body)
  (cadr body))
(defun get-second-RO-arg (body)
  (caddr body))
