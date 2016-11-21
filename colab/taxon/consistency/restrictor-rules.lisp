;************************************************************
;*                                                          *
;*                File : restrictor-rules
;*                                                          *
;************************************************************

;;; Concept Name Rule

(defun CR (rule obj)
;  (print-rule 'CR obj)
  (funcall rule (propagate-abstract! obj))
  )

;;; Negated Concept Name Rule

(defun NCR (rule obj)
;  (print-rule 'NCR obj)
  (cond ((concrete? obj))
	((unknown? obj)
	 (add-printable-function! obj #'print-fct-of-NCR rule)
	 (add-abstract-function! (make-neg-conceptname-function rule) obj))
	(t (funcall rule obj))
	))

;;; Concrete Predicate Rules

(defun SCPR (position next-function pred first-obj obj)
  (print-rule 'SCPR position obj)
  (let ((concrete-obj (propagate-concrete! obj (get-domain-of-cpred (get-pred-of-int-repr pred))))
	)
       (set-nth-arg! pred position concrete-obj)
       (funcall next-function pred first-obj first-obj)
       ))

(defun FCPR (position next-function pred first-obj obj)
  (print-rule 'CPR position obj)
  (propagate-concrete-predicates (get-pred-of-int-repr pred)
				 (get-concrete-args-and-propagate-type pred position obj))
  (unless (all-arguments-exist? pred)
	  (funcall next-function pred first-obj first-obj))
  )

;;; Negated Concrete Predicate Rules

(defun NSCPR (position next-function pred first-obj obj)
  (print-rule 'NSCPR position obj)
  (cond ((abstract? obj)
	 (clash 'NSCPR obj))
	(t (set-nth-arg! pred position obj)
	   (funcall next-function pred first-obj first-obj))
	))

(defun NFCPR (position next-function pred first-obj obj)
  (print-rule 'NFCPR position obj)
  (cond ((abstract? obj))
	((unknown? obj)
	 (add-printable-function! obj #'print-fct-of-N_PR next-function first-obj position pred)
	 (add-concrete-function! (make-NFCPR position next-function pred first-obj) 
				 obj))
	(t (propagate-neg-concrete-predicates (get-pred-of-int-repr pred)
					      (get-concrete-args pred position obj))
	   (unless (all-arguments-exist? pred)
		   (funcall next-function pred first-obj first-obj))
	   )
	))

;;; Abstract Predicate Rule

(defun APR (position next-function pred first-obj obj)
  (print-rule 'APR position obj)
  (propagate-abstract-predicates (get-abstract-predicates-and-propagate-type pred position obj))
  (unless (all-arguments-exist? pred)
	  (funcall next-function pred first-obj first-obj))
  )
(defun APNR (position next-function pred first-obj obj)
  (print-rule 'APNR position obj)
  (propagate-neg-abstract-predicates (get-abstract-predicates-and-propagate-type pred position obj))
  (unless (all-arguments-exist? pred)
	  (funcall next-function pred first-obj first-obj))
  )

;;; Negated Abstract Predicate Rule

(defun NAPR (position next-function pred first-obj obj)
  (print-rule 'NAPR position obj)
  (cond ((concrete? obj))
	((unknown? obj)
	 (add-printable-function! obj #'print-fct-of-N_PR next-function first-obj position pred)
	 (add-abstract-function! (make-NAPR position next-function pred first-obj) 
				 obj))
	(t (propagate-neg-abstract-predicates (get-abstract-predicates pred
								       position
								       obj))
	   (unless (all-arguments-exist? pred)
		   (funcall next-function pred first-obj first-obj))
	   )
	))

(defun NAPNR (position next-function pred first-obj obj)
  (print-rule 'NAPR position obj)
  (cond ((concrete? obj))
	((unknown? obj)
	 (add-printable-function! obj #'print-fct-of-N_PR next-function first-obj position pred)
	 (add-abstract-function! (make-NAPR position next-function pred first-obj) 
				 obj))
	(t (propagate-abstract-predicates (get-abstract-predicates pred
								   position
								   obj))
	   (unless (all-arguments-exist? pred)
		   (funcall next-function pred first-obj first-obj))
	   )
	))

;;; Agreement Rule

(defun AR (dummy next-function agree first obj)
  (print-rule 'AR dummy obj)
  (cond ((no-arg-so-far? agree)
	 (set-first-arg! agree obj)
	 (funcall next-function agree first first))
	(t (equal-rule (get-first agree) obj))
	)
  )

;;; Disagreement Rule

(defun DAR (dummy next-function disagree first obj)
  (print-rule 'DAR dummy obj)
  (cond ((no-arg-so-far? disagree)
	 (set-first-arg! disagree obj)
	 (funcall next-function disagree first first))
	(t (unequal-rule (get-first disagree) obj))
	)
  )

;*********************************************************************
;
; 		help-functions for restrictor rules
;
;*********************************************************************

(defun get-abstract-predicates-and-propagate-type (pred position value)
  (cond ((exists-argument? pred position value) nil)
	((incomplete? pred position)
	 (set-nth-arg! pred position value)
	 '())
	((first-propagation? pred position)
	 (set-nth-arg! pred position value)
	 (propagate-all-abstract! (get-all-arguments pred))
	 (create-predicates pred position value))
	(t (set-nth-arg! pred position value)
	   (propagate-abstract! value)
	   (create-predicates pred position value))
	))
(defun get-concrete-args-and-propagate-type (pred position value)
  (cond ((exists-argument? pred position value) nil)
	((incomplete? pred position)
	 (set-nth-arg! pred position value)
	 '())
	((first-propagation? pred position)
	 (set-nth-arg! pred position value)
	 (propagate-all-concrete! (get-all-arguments pred) 
				  (get-domain-of-cpred (get-pred-of-int-repr pred)))
	 (produce-all-new-args (make-dereferenced-matrix pred position value)))
	(t (set-nth-arg! pred position value)
	   (propagate-concrete! value (get-domain-of-cpred (get-pred-of-int-repr pred)))
	   (produce-all-new-args (make-dereferenced-matrix pred position value)))
	))

(defun get-abstract-predicates (pred position value)
  (cond ((exists-argument? pred position value) nil)
	((incomplete? pred position)
	 (set-nth-arg! pred position value)
	 nil)
	(t (set-nth-arg! pred position value)
	   (create-predicates pred position value))
	))
(defun get-concrete-args (pred position value)
  (cond ((exists-argument? pred position value) nil)
	((incomplete? pred position)
	 (set-nth-arg! pred position value)
	 nil)
	(t (set-nth-arg! pred position value)
	   (produce-all-new-args (make-dereferenced-matrix pred position value)))
	))

(defun create-predicates (pred position value)
  (mapcar #'(lambda (args)
		    (make-predicate (get-pred-of-int-repr pred) args))
	  (produce-all-new-args (make-matrix pred position value))))

(defun produce-all-new-args (matrix)
  (do ((reverse (cdr (reverse matrix)) (cdr reverse))
       (result (mapcar 'list (car (last matrix))))
       )
      ((null reverse) result)
      (setq result (produce-next-argument-level result (car reverse)))
      ))
(defun produce-next-argument-level (result-so-far next-args)
  (do ((next-args next-args (cdr next-args))
       (result nil)
       )
      ((null next-args) result)
      (do ((result-so-far result-so-far (cdr result-so-far))
	   )
	  ((null result-so-far))
	  (setq result (cons (cons (car next-args)
				   (car result-so-far))
			     result))))
  )

;************************************************************
;*                                                          *
;*               	make-functions 
;*                                                          *
;************************************************************

(defun make-neg-conceptname-function (rule)
  #'(lambda (obj) (funcall rule (deref obj))))
(defun make-NAPR (pos next-funtion pred first)
  #'(lambda (obj) (NAPR pos next-funtion pred first (deref obj))))
(defun make-NFCPR (pos next-funtion pred first)
  #'(lambda (obj) (NFCPR  pos next-funtion pred first (deref obj))))
	    

