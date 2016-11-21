;(defvar *codetyp* 'bwam)
(defvar *codetyp* 'uwam)
 
;;;;;; Simple Register MOVE - no X/Y/A can be distinguished

(defun g-movesd (source destination)
 (d_xreg_assoc destination y-x-usage-list)
 (if (eq source destination) nil
 (cond	((eq *codetyp* 'bwam) `((wpm-put_x_value ,(1- source) 
						 ,(1- destination)))  )
	((eq *codetyp* 'uwam) `((put_value_temp ,source ,destination))  )
	((eq *codetyp* 'hwam) `((put_xal ,source ,destination))  )
	(t (error "unknown *codetyp* : g-movesd"))
 )
 )
)

;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  P U T - instructions	PERMs
;;;;;;

(defun g-put-variable-perm (Yn An)
 (d_xreg_assoc An y-x-usage-list)
 (add-y-x-list Yn An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_y_variable ,(1- Yn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_variable_perm ,Yn ,An))  )
	((eq *codetyp* 'hwam) `((put_yar ,Yn ,An))  )
	(t (error "unknown *codetyp* : g-put-variable-perm"))
 )
)

(defun g-put-value-perm (Yn An)
 (d_xreg_assoc An y-x-usage-list)
 (add-y-x-list Yn An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_y_value ,(1- Yn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_value_perm ,Yn ,An))  )
	((eq *codetyp* 'hwam) `((put_yal ,Yn ,An))  )
	(t (error "unknown *codetyp* : g-put-value-perm"))
 )
)

(defun g-put-unsafe-value-perm (Yn An)
 (d_xreg_assoc An y-x-usage-list)
 (add-y-x-list Yn An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_unsafe_value ,(1- Yn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_unsafe_value_perm ,Yn ,An))  )
	((eq *codetyp* 'hwam) `((put_uns_yal ,Yn ,An))  )
	(t (error "unknown *codetyp* : g-put-unsafe-value-perm"))
 )
)


;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  P U T - instructions	TEMPS
;;;;;;

(defun g-put-variable-temp (Xn An)
 (d_xreg_assoc An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_x_variable ,(1- Xn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_variable_temp ,Xn ,An))  )
	((eq *codetyp* 'hwam) `((put_xar ,Xn ,An))  )
	(t (error "unknown *codetyp* : g-put-variable-temp"))
 )
)

(defun g-put-value-temp (Xn An)
(if (eq Xn An) nil
(progn
 (d_xreg_assoc An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_x_value ,(1- Xn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_value_temp ,Xn ,An))  )
	((eq *codetyp* 'hwam) `((put_xal ,Xn ,An))  )
	(t (error "unknown *codetyp* : g-put-value-temp"))
 )
)
)
)

;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  P U T - instructions	CONSTS
;;;;;;

(defun g-put-nil (An)
 (d_xreg_assoc An y-x-usage-list)
 (cond 	((eq *codetyp* 'bwam) `((wpm-put_nil ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_nil ,An))  )
	((eq *codetyp* 'hwam) `((put_nil ,An))  )
	(t (error "unknown *codetyp* : g-put-nil"))
 )
)

(defun g-put-constant (Const An)
 (d_xreg_assoc An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_constant ,Const ,(1- An)))  )
	((eq *codetyp* 'uwam) `((put_constant ,Const ,An))  )
	((eq *codetyp* 'hwam) `((put_con ,Const ,An))  )
	(t (error "unknown *codetyp* : g-put-constant"))
 )
)


;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  G E T - instructions	PERMs
;;;;;;

(defun g-get-variable-perm (Yn An)
 (d_xreg_assoc An y-x-usage-list)
 (add-y-x-list Yn An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-get_y_variable ,(1- Yn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((get_variable_perm ,Yn ,An))  )
	((eq *codetyp* 'hwam) `((get_yar ,Yn ,An))  )
	(t (error "unknown *codetyp* : g-get-variable-perm"))
 )
)

(defun g-get-value-perm (Yn An)
 (d_xreg_assoc An y-x-usage-list)
 (add-y-x-list Yn An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-get_y_value ,(1- Yn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((get_value_perm ,Yn ,An))  )
	((eq *codetyp* 'hwam) `((get_yal ,Yn ,An))  )
	(t (error "unknown *codetyp* : g-get-value-perm"))
 )
)

;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  G E T - instructions	TEMPs
;;;;;;

(defun g-get-variable-temp (Xn An)
 (d_xreg_assoc Xn y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-get_x_variable ,(1- Xn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((get_variable_temp ,Xn ,An))  )
	((eq *codetyp* 'hwam) `((get_xar ,Xn ,An))  )
	(t (error "unknown *codetyp* : g-get-variable-temp"))
 )
)

(defun g-get-value-temp (Xn An)
(if (eq Xn An) nil
 (progn
 (d_xreg_assoc Xn y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-get_x_value ,(1- Xn) ,(1- An)))  )
	((eq *codetyp* 'uwam) `((get_value_temp ,Xn ,An))  )
	((eq *codetyp* 'hwam) `((get_xal ,Xn ,An))  )
	(t (error "unknown *codetyp* : g-get-value-temp"))
 )
 )
)
)


;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  G E T - instructions	CONSTs
;;;;;;

(defun g-get-nil (An)
 (d_xreg_assoc An y-x-usage-list)
 (cond  ((eq *codetyp* 'bwam) `((wpm-get_nil ,(1- An))) )
	((eq *codetyp* 'uwam) `((get_nil ,An)) )
	((eq *codetyp* 'hwam) `((get_nil ,An)) )
	(t (error "unknown *codetyp* : g-get-nil"))
 )
)

(defun g-get-constant (Const An)
 (d_xreg_assoc An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-get_constant ,Const ,(1- An))) )
	((eq *codetyp* 'uwam) `((get_constant ,Const ,An)) )
	((eq *codetyp* 'hwam) `((get_con ,Const ,An)) )
	(t (error "unknown *codetyp* : g-get-constant"))
 )
)

;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  G E T - instructions	strucures/lists
;;;;;;

(defun g-get-structure (functor arity An)
 (d_xreg_assoc An y-x-usage-list)
 (cond  ((eq *codetyp* 'bwam) `((wpm-get_structure ,functor ,arity ,(1- An))) )
	((eq *codetyp* 'uwam) `((get_structure ,(list functor arity) ,An)) )
	(t (error "unknown *codetyp* : g-get-structure"))
 )
)

(defun g-get-list (An)
 (d_xreg_assoc An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-get_list ,(1- An))) )
	((eq *codetyp* 'uwam) `((get_list ,An)) )
	(t (error "unknown *codetyp* : g-get-list"))
 )
)


;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  P U T - instructions	strucures/lists
;;;;;;

(defun g-put-structure (functor arity An)
 (d_xreg_assoc An y-x-usage-list)
 (cond  ((eq *codetyp* 'bwam) `((wpm-put_structure ,functor ,arity ,(1- An))) )
	((eq *codetyp* 'uwam) `((put_structure ,(list functor arity) ,An)) )
	(t (error "unknown *codetyp* : g-put-structure"))
 )
)

(defun g-put-list (An)
 (d_xreg_assoc An y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-put_list ,(1- An))) )
	((eq *codetyp* 'uwam) `((put_list ,An)) )
	(t (error "unknown *codetyp* : g-put-list"))
 )
)

;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  C O N T R O L - instructions	EXE/CALL/PROCCED Type
;;;;;;

(defun g-call (Permcnt Predicate Arity)
 (let ( (PermAri (intern (format nil "~a/~a" Predicate Arity))) )
 (setq y-x-usage-list nil)
  (cond	((eq *codetyp* 'bwam)  `((wpm-call ,PermAri ,PermCnt)) )
	((eq *codetyp* 'uwam)  `((call ,PermAri ,Permcnt)) )
	(t (error "unknown *codetyp* : g-call"))
  )
 )
)

(defun g-execute (Predicate Arity)
 (let ( (PermAri (intern (format nil "~a/~a" Predicate Arity))) )
 (setq y-x-usage-list nil)
  (cond	((eq *codetyp* 'bwam) `((wpm-execute ,PermAri)) )
	((eq *codetyp* 'uwam) `((execute ,PermAri)) )
	(t (error "unknown *codetyp* : g-execute"))
  )
 )
)

(defun g-proceed()
 (cond	((eq *codetyp* 'bwam) `((wpm-proceed)) )
	((eq *codetyp* 'uwam) `((proceed)) )
	(t (error "unknown *codetyp* : g-proceed"))
 )
)

(defun g-deallo-exe (Predicate Arity)
 (let ( (PermAri (intern (format nil "~a/~a" Predicate Arity))) )
  (cond ((eq *codetyp* 'bwam) `((wpm-deallocate) (wpm-execute ,PermAri)) )
  	((eq *codetyp* 'uwam) `((deallocate) (execute ,PermAri)) )
	(t (error "unknown *codetyp* : g-deallo-exe"))
  )
 )
)

(defun g-deallocate ()
  (cond ((eq *codetyp* 'bwam) `((wpm-deallocate)))
  	((eq *codetyp* 'uwam) `((deallocate)))
	(t (error "unknown *codetyp* : g-deallo-exe"))
  )
)


;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  C O N T R O L - instructions	ALLOCATE/TRY/RETRY/TRUST
;;;;;;

(defun g-allocate (perms)
 (cond	((eq *codetyp* 'bwam) `((wpm-allocate ,perms)) )
	((eq *codetyp* 'uwam) `((allocate ,perms)) ) 
	(t (error "unknown *codetyp* : g-allocate"))
 )
)

(defun g-try-me-else (Label Arity)
 (cond	((eq *codetyp* 'bwam) `((wpm-try_me_else ,Label ,Arity))  )
	((eq *codetyp* 'uwam) `((try_me_else ,Label ,Arity))  )
	(t (error "unknown *codetyp* : g-try-me-else"))
 )
)

(defun g-retry-me-else (Label Arity)
 (cond	((eq *codetyp* 'bwam) `((wpm-retry_me_else ,Label)) )
	((eq *codetyp* 'uwam) `((retry_me_else ,Label ,Arity)) )
	(t (error "unknown *codetyp* : g-retry-me-else"))
 )
)

(defun g-trust-me-else-fail (Arity)
 (cond	((eq *codetyp* 'bwam) `((wpm-trust_me_else)) )
	((eq *codetyp* 'uwam) `((trust_me_else_fail ,Arity)) )
	(t (error "unknown *codetyp* : g-trust-me-else-fail"))
 )
)

(defun g-fail ()
 (cond	((eq *codetyp* 'bwam) `((wpm-fail)) )
	((eq *codetyp* 'uwam) `((fail)) )
	(t (error "unknown *codetyp* : g-trust-me-else-fail"))
 )
)

(defun g-cl-func (no name)
 (setq y-x-usage-list nil)
  (cond	((eq *codetyp* 'bwam)  `((wpm-call-lisp ,name ,no)))
	((eq *codetyp* 'uwam)  `((cl-func ,name ,no )) )
	(t (error "unknown *codetyp* : g-lispfunc"))
  )
)

(defun g-cl-pred (no name)
 (setq y-x-usage-list nil)
  (cond	((eq *codetyp* 'bwam)  `((wpm-call-lisp ,name ,no)))
	((eq *codetyp* 'uwam)  `((cl-pred ,name ,no )) )
	(t (error "unknown *codetyp* : g-lispcall"))
  )
)

(defun g-cl-extra (no name)
 (setq y-x-usage-list nil)
  (cond	((eq *codetyp* 'bwam)  `((wpm-call-lisp ,name ,no)))
	((eq *codetyp* 'uwam)  `((cl-extra ,name ,no )) )
	(t (error "unknown *codetyp* : g-lispcall"))
  )
)
  
(defun g-cl-relf (no name)
 (setq y-x-usage-list nil)
  (cond	((eq *codetyp* 'bwam)  `((wpm-call-lisp ,name ,no)))
	((eq *codetyp* 'uwam)  `((cl-relf ,name ,no)) )
	(t (error "unknown *codetyp* : g-lispcall"))
  )
)
;;;;;;  -----------------------------------------------------------------------
;;;;;;
;;;;;;  U N I F Y - instructions for structures/lists
;;;;;;

(defun g-unify-variable-temp (Xn)
 (d_xreg_assoc Xn y-x-usage-list)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_x_variable ,Xn)) )
	((eq *codetyp* 'uwam) `((unify_variable_temp ,Xn)) ) 
	((eq *codetyp* 'hwam) `((uni_xar ,Xn)) ) 
	(t (error "unknown *codetyp* : g-unify-variable-temp"))
 )
)

(defun g-unify-variable-perm (Yn)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_y_variable ,Yn)) )
	((eq *codetyp* 'uwam) `((unify_variable_perm ,Yn)) ) 
	((eq *codetyp* 'hwam) `((uni_yar ,Yn)) ) 
	(t (error "unknown *codetyp* : g-unify-variable-perm"))
 )
)

(defun g-unify-value-temp (Xn)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_x_value ,Xn)) )
	((eq *codetyp* 'uwam) `((unify_value_temp ,Xn)) ) 
	((eq *codetyp* 'hwam) `((uni_xal ,Xn)) ) 
	(t (error "unknown *codetyp* : g-unify-value-temp"))
 )
)

(defun g-unify-value-perm (Yn)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_y_value ,Yn)) )
	((eq *codetyp* 'uwam) `((unify_value_perm ,Yn)) ) 
	((eq *codetyp* 'hwam) `((uni_yal ,Yn)) ) 
	(t (error "unknown *codetyp* : g-unify-value-perm"))
 )
)

; uni_loc_xal Xn == unify_x_local_value ANL WAM
(defun g-unify-local-value-temp (Xn)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_x_local_value ,Xn)) )
	((eq *codetyp* 'uwam) `((unify_local_value_temp ,Xn)) ) 
	((eq *codetyp* 'hwam) `((uni_loc_xal ,Xn)) ) 
	(t (error "unknown *codetyp* : g-unify-local-value-temp"))
 )
)

(defun g-unify-local-value-perm (Yn)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_y_local_value ,Yn)) )
	((eq *codetyp* 'uwam) `((unify_local_value_perm ,Yn)) ) 
	((eq *codetyp* 'hwam) `((uni_loc_yal ,Yn)) ) 
	(t (error "unknown *codetyp* : g-unify-local-value-perm"))
 )
)

(defun g-unify-nil ()
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_nil)) )
	((eq *codetyp* 'uwam) `((unify_nil)) ) 
	((eq *codetyp* 'hwam) `((uni_nil)) ) 
	(t (error "unknown *codetyp* : g-unify-nil"))
 )
)

(defun g-unify-constant (C)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_constant ,C)) )
	((eq *codetyp* 'uwam) `((unify_constant ,C)) ) 
	((eq *codetyp* 'hwam) `((uni_con ,C)) ) 
	(t (error "unknown *codetyp* : g-unify-constant"))
 )
)

(defun g-unify-void (n)
 (cond	((eq *codetyp* 'bwam) `((wpm-unify_void ,n)) )
	((eq *codetyp* 'uwam) `((unify_void ,n)) ) 
	((eq *codetyp* 'hwam) `((uni_void ,n)) ) 
	(t (error "unknown *codetyp* : g-unify-void"))
 )
)

; uni_void
