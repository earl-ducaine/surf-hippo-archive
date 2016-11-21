;;;;;; Code Generator for Classified Datafun (CHUNKy version)
;;;;;; DFKI / arCtec
;;;;;; Hans - Guenther HEIN
;;;;;; APR 90
;;;;;; Version 0.5

(defvar *lureg* nil)

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-TAIL-PERM:
;;;;;;	stelle: argument place being processed.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  arg:    actually the variable to be handled.
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;  
;;;;;; Output: List of instructions for that permanent variable.
;;;;;;  

(defun code-gen-tail-perm (stelle perms arg chknr litnr)
 (declare (special y-x-usage-list))
 (let* (
;         (local-var-descr (s-cg-local-var-descr arg))
	  (local-var-occurence (s-cg-local_var_occurrence arg))
	  (local-var-saveness (s-cg-local_var_saveness arg))
	  (perm-descr (get_perm_descr arg perms))
	  (perm-y-nr (s-cg-perm_y_nr perm-descr))
	  (perm-last-ckliteral (s-cg-perm_last_literal perm-descr))
;	  (perm-use-head (s-cg-perm_use_head perm-descr))
       )
	(cond ((firstreuse-p local-var-occurence)
			 (g-put-variable-perm perm-y-nr stelle)
	       ) ; occurrence = first or reused

	      ((eq local-var-occurence 'nonfirst)
	   	(cond ((or (eq local-var-saveness 'safe)(eq local-var-saveness 'global))
			(let ( (y-in-x (is-y-in-x perm-y-nr y-x-usage-list)))
			  (cond (y-in-x ; used safe and already in Xreg ?
				 (g-movesd  ; direct move
                                   (sel-x-from-usage-entry y-in-x) stelle )
                                )
				(t (g-put-value-perm perm-y-nr stelle)  )
			  ) ; cond
                        ) ; let
		       ) ; eq

	      ((eq local-var-saveness 'unsafe)
		(cond ((equal (list chknr litnr) perm-last-ckliteral)
			          (g-put-unsafe-value-perm perm-y-nr stelle)
	               ) ; equal litnr perm-last-ckliteral

		       (t
			(let ( (y-in-x (is-y-in-x perm-y-nr y-x-usage-list)))
			  (cond (y-in-x
				 (g-movesd (sel-x-from-usage-entry y-in-x) 
                                           stelle)
                                )
				(t (g-put-value-perm perm-y-nr stelle)  )
			  ) ; cond
			) ; let
		       ) ; true
	) ; cond
       ) ; eq local-var-safeness 'unsafe

      (t (error "code-gen-tail-perm"))
   ) ; cond
  ); occurence = nonfirst
      (t (error "code-gen-tail-perm"))
     ) ; cond
    ) ; let
) ; defun
		

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-TAIL-TEMP:
;;;;;;	stelle: argument place being processed.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  arg:    actually the variable to be handled.
;;;;;;  
;;;;;; Output: List of instructions for that permanent variable.
;;;;;;  

(defun code-gen-tail-temp (stelle temps arg)
 (let* (
;          (local-var-descr (s-cg-local-var-descr arg))
	  (local-var-occurence (s-cg-local_var_occurrence arg))
;	  (local-var-saveness (s-cg-local_var_saveness arg))
	 (temp-descr (get_temp_descr arg temps))
	  (temp-x-nr (s-cg-temp_x_nr temp-descr))
	  (temp-use-head (s-cg-temp_use_head temp-descr))
;	  (temp-use-literal (s-cg-temp_use_premise temp-descr))
       )
	(cond ((firstreuse-p local-var-occurence)
		(g-put-variable-temp temp-x-nr stelle) )
	      ((eq local-var-occurence 'nonfirst)
		(cond ((or (eq temp-x-nr stelle) (member stelle temp-use-head))
                       nil) ; do not generate code
                      (t (g-put-value-temp temp-x-nr stelle) )
		)
	       )
	      (t (error "code-gen-tail-temp"))
	)
 ) ; let
) ; defun

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-TAIL-ARG:
;;;;;;	stelle: argument place being processed.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  arg:    constant/nil/permanent/temporary variable
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;  
;;;;;; Output: List of instructions for the argument
;;;;;;  

(defun code-gen-tail-arg (stelle perms temps arg chknr litnr)
 (cond	((null arg) (g-put-nil stelle) )
	((arg-const-p arg) (g-put-constant arg stelle))
	((arg-var-p arg) 
	 (if (eq (s-cg-local_var_class arg) 'perm)
		(code-gen-tail-perm stelle perms arg chknr litnr)
		(code-gen-tail-temp stelle temps arg))
	)
	(t (error "code-gen-tail-arg"))
 ) ; cond
) ; defun


;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-TAIL:
;;;;;;	stelle: argument place being processed.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  arity:  arity of the predicate
;;;;;;  permcnt:no. of permanent variable needed after the call-instruction
;;;;;;  arg:    constant/nil/permanent/temporary variable
;;;;;;  fac:    the predicate with its arguments
;;;;;;  callexeflg: produce a call (t) or execute (NIL)
;;;;;;  deallocflg: if T a deallocate is generated.
;;;;;;              set to T if it is the last literal to be processed.
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;  
;;;;;; Output: List of instructions for the literal
;;;;;;  

(defun code-gen-tail ( perms temps arity permcnt fac callexeflg deallocflg 
		       chknr litnr arg_seq )
 (let* ( (predicate_name (s-cg-functor fac))
         (predicate_args (s-cg-arglist_classification fac))
	)
 (do* (	lc*code 
        (arg_seqlist arg_seq (cdr arg_seqlist))
        (lc*stelle (car arg_seqlist) (car arg_seqlist))
        (lc*nxtarg (if lc*stelle (nth (1- lc*stelle) predicate_args))
                   (if lc*stelle (nth (1- lc*stelle) predicate_args))) )
      ( (null lc*stelle)					; termin. expr
	(if callexeflg						; return result
         (doappend lc*code (g-call permcnt predicate_name arity))
         (if deallocflg
	  (doappend lc*code (g-deallo-exe predicate_name arity))
	  (doappend lc*code (g-execute predicate_name arity))
         )
	)
      )
      
      (addcode lc*code (code-gen-tail-arg lc*stelle
					  perms
					  temps
					  lc*nxtarg 
					  chknr
					  litnr )
      ) ; addcode
  ) ; do*
 ) ; let*
) ; defun


;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-HEAD-PERM:
;;;;;;	stelle: argument place being processed.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  arg:    actually the variable to be handled.
;;;;;;  
;;;;;; Output: List of instructions for that permanent variable.
;;;;;;  

(defun code-gen-head-perm (stelle perms arg)
 (let* ( 
;         (local-var-descr (s-cg-local-var-descr arg))
	  (local-var-occurence (s-cg-local_var_occurrence arg))
;	  (local-var-saveness (s-cg-local_var_saveness arg))
	 (perm-descr (get_perm_descr arg perms))
	  (perm-y-nr (s-cg-perm_y_nr perm-descr))
;	  (perm-last-ckliteral (s-cg-perm_last_literal perm-descr))
	  (perm-use-head (s-cg-perm_use_head perm-descr))
       )
	(cond ((firstreuse-p local-var-occurence)
				       (g-get-variable-perm perm-y-nr stelle)
	       ) ; occurrence = first or reused
	      ((eq local-var-occurence 'nonfirst)
		     (cond ((null perm-use-head) 
					(g-get-value-perm perm-y-nr stelle))
			   (t (g-get-value-temp 
				(apply #'min perm-use-head) stelle))
                     )
	      ); occurence = nonfirst
	      (t (error "code-gen-tail-perm"))
	)
    ) ; let
) ; defun


;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-HEAD-TEMP:
;;;;;;	stelle: argument place being processed.
;;;;;;  temps:  the global info about the temporary variables in the clause.
;;;;;;  arg:    actually the variable to be handled.
;;;;;;  
;;;;;; Output: List of instructions for that temporary variable.
;;;;;;  

(defun code-gen-head-temp (stelle temps arg)
 (let* (
;         (local-var-descr (s-cg-local-var-descr arg))
	  (local-var-occurence (s-cg-local_var_occurrence arg))
;	  (local-var-saveness (s-cg-local_var_saveness arg))
	 (temp-descr (get_temp_descr arg temps))
	  (temp-x-nr (s-cg-temp_x_nr temp-descr))
	  (temp-use-head (s-cg-temp_use_head temp-descr))
	  (temp-use-literal (s-cg-temp_use_premise temp-descr))
       )

	(cond 
	      ((firstreuse-p local-var-occurence)
		(cond 	((eq temp-x-nr stelle) NIL) ; no code necessary
			(t (g-get-variable-temp temp-x-nr stelle) )
                ) ; cond
               ) ; eq
	      ((eq local-var-occurence 'nonfirst)
		(cond	((and (member stelle temp-use-literal)
	 	              (member stelle temp-use-head))
                         NIL ; no code necessary
                        )
			( t (g-get-value-temp temp-x-nr stelle) )
		) ; cond
	       ) ; eq
	      ((null temp-use-literal) NIL) ; no code necessary
	      (t (error "code-gen-head-temp"))
	) ; cond
 ) ; let
) ; defun

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-HEAD-ARG
;;;;;;	stelle: argument place being processed.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  arg:    argument to be handled 
;;;;;; Output: List of instructions for that temporary variable.
;;;;;;  

(defun code-gen-head-arg (stelle perms temps arg)
  (cond	((null arg) (g-get-nil stelle))
	((arg-const-p arg) (g-get-constant arg stelle))
	((arg-var-p arg) 
	 (if (eq (s-cg-local_var_class arg) 'perm)
		(code-gen-head-perm stelle perms arg)
		(code-gen-head-temp stelle temps arg)
	 ) ; if
	) ; arg-var-p
	(t (error "code-gen-head-arg"))
  ) ; cond
) ; defun

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-HEAD:
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  fac:    the predicate with its arguments
;;;;;;
;;;;;; Output: List of instructions for that temporary variable.
;;;;;;  

(defun code-gen-head (perms temps fac arg_seq)
(if (null arg_seq) nil
 (let* (
;        (predicate_name (s-cg-functor fac))
         (predicate_args (s-cg-arglist_classification fac))
       )
 (do* ( lc*code
	(arg_seqlist arg_seq (cdr arg_seqlist))
        (lc*stelle (car arg_seqlist) (car arg_seqlist))
        (lc*nxtarg (nth (1- lc*stelle) predicate_args) 
                   (if lc*stelle (nth (1- lc*stelle) predicate_args))) )
      ( (null lc*stelle)					; termin. expr
        lc*code							; result expr
      )
  (addcode lc*code (code-gen-head-arg lc*stelle perms temps lc*nxtarg ))
 ) ; do*
 ) ; let*
) ; if
) ; defun



;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-CHUNK:
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  chnk : the chunk which is to be processed
;;;;;;  callexeflg: produce a call (t) or execute (NIL)
;;;;;;  deallocflg: if T a deallocate is generated.
;;;;;;              set to T if it is the last literal to be processed.
;;;;;;  chknr:  The chunknumber..(start with 1)
;;;;;;
;;;;;; Output: List of instructions for a normal chunk
;;;;;;  

(defun code-gen-chunk ( perms temps chunk callexeflg deallocflg chknr)
 (declare (special y-x-usage-list))
 (declare (special *lureg*))
 (setq y-x-usage-list nil)
 (setq *lureg* (s-cg-chunk_lu_reg (s-cg-chunk_descr chunk)))
 (do* ( (chkentries (if (eq (s-cg-chunk_id chunk) 'chunk)
			 (s-cg-chunk-bd_cgcl chunk)
                         (error "code-gen-chunk:that's no chunk"))
        )
	(lc*chkdescr (s-cg-chunk_descr chunk))
	(lc*vpul (s-cg-chunk_vpul lc*chkdescr))
	(lc*x chkentries (cdr lc*x))
	(lc*litnr 1 (1+ lc*litnr))
        lc*code
        (lc*actual (first lc*x)(first lc*x))
	(lc*usrlitflg nil)
      )
      ((null lc*x)
	(if (null lc*usrlitflg) ; no usrlit in chunk
         (if deallocflg
	  (doappend lc*code (doappend (g-deallocate)(g-proceed)))
	  (doappend lc*code (g-proceed))
         )
	 lc*code
	) ; if
      )

      (cond ((null lc*actual) (addcode lc*code (g-put-nil 1)))
            ((atom lc*actual) (if (eq lc*actual 'unknown)
				(addcode lc*code (g-fail))
				(addcode lc*code (g-put-constant lc*actual 1))
			      )
	    )
	    ((cg-inst-p lc*actual)
		(addcode lc*code
		       (doappend
			(code-gen-put-structlist lc*actual 1)
			(code-gen-uni-args (cg-s-inst-funargs lc*actual) 
					perms temps chknr lc*litnr)
		       )
		)
	    )
	    ((arg-var-p lc*actual)
		(addcode lc*code
		 (code-gen-tail-arg 1 perms temps lc*actual chknr lc*litnr)
		)
	    )
            ((eq (s-cg-usrlit_id lc*actual) 'is)
		(addcode lc*code
		 (code-gen-is (cadr lc*actual) (caddr lc*actual)
				 perms temps chknr lc*litnr lc*vpul
                                 (and (null callexeflg) (null (cdr lc*x)))
                 )
                )
            )
	    ((eq (s-cg-usrlit_id lc*actual) 'refl-xreg)
		(addcode lc*code
                 (code-gen-refl-xreg perms temps (cadr lc*actual) 
					chknr lc*litnr)
		)
	    )
	    ((eq (s-cg-usrlit_id lc*actual) 'usrlit)
		(addcode lc*code
		 (let ((litdescr (s-cg-literal_descr lc*actual)))
		  (setq lc*usrlitflg t)
		  (code-gen-tail perms temps 
			(s-cg-arity litdescr)
			(s-cg-env_size litdescr)
			(s-cg-fac_list lc*actual)
			callexeflg deallocflg chknr lc*litnr 
			(s-cg-arg_seq litdescr)
		  )
		 ) ; let
		)
	    ) ; eq usrlit
	   ((cg-lispcall-p lc*actual) 
		(addcode lc*code
		 (let ((litdescr (s-cg-literal_descr lc*actual)))
		  (setq lc*usrlitflg t)
		  (code-gen-cl lc*actual perms temps 
			(s-cg-arity litdescr)
			(s-cg-env_size litdescr)
			(s-cg-fac_list lc*actual)
			callexeflg deallocflg chknr lc*litnr 
			(s-cg-arg_seq litdescr)
		  )
		 ) ; let
		)
		) ; eq .. cl
	    (t (error "code-gen-chunk"))
      ) ; cond
 ) ; do*
) ; defun code-gen-chunk
 

;;;;;;  lu_reg: last X register to be used by Classifier
;;;;;;  vpulist: y/reg argumentlist places

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-HDCHUNK:
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  chnk : the chunk which is to be processed
;;;;;;  callexeflg: produce a call (t) or execute (NIL)
;;;;;;  deallocflg: if T a deallocate is generated.
;;;;;;              set to T if it is the last literal to be processed.
;;;;;;  chknr:  The chunknumber..(start with 1)
;;;;;;
;;;;;; Output: List of instructions for a head chunk
;;;;;;  

(defun code-gen-hdchunk ( perms temps chunk callexeflg deallocflg chknr)
 (declare (special y-x-usage-list))
 (declare (special *lureg*))
 (setq y-x-usage-list nil)
 (setq *lureg* (s-cg-chunk_lu_reg (s-cg-chunk_descr chunk)))
 (do* ( (chkbody (if (eq (s-cg-chunk_id chunk) 'chunk)
			 (s-cg-chunk_hd_cgfpl chunk)
                         (error "code-gen-chunk:that's no chunk"))
        )
	(chkheadusrlit (s-cg-chunk_head_literal chunk))
	(chkheaddescr  (s-cg-literal_descr chkheadusrlit))
	(headfac (if (eq (s-cg-usrlit_id chkheadusrlit) 'usrlit)
		     (s-cg-fac_list chkheadusrlit)
                     (error "hdchunk: head is no usrlit"))
        )
	(lc*chkdescr (s-cg-chunk_descr chunk))
;	(lc*lu_reg (s-cg-chunk_lu_reg lc*chkdescr))
	(lc*vpul (s-cg-chunk_vpul lc*chkdescr))
	(lc*x chkbody (cdr lc*x))
	(lc*litnr 1 (1+ lc*litnr))
        (lc*code 
	 (code-gen-head perms temps headfac (s-cg-arg_seq chkheaddescr))
	)
        (lc*actual (first lc*x)(first lc*x))
	(lc*usrlitflg nil)
      )
      ((null lc*x) 
	(if (null lc*usrlitflg) ; no usrlit in chunk
         (if deallocflg
	  (doappend lc*code (doappend (g-deallocate)(g-proceed)))
	  (doappend lc*code (g-proceed))
         )
	 lc*code
	) ; if
      )

      (cond ((null lc*actual) (addcode lc*code (g-put-nil 1)))
            ((atom lc*actual) (if (eq lc*actual 'unknown)
				(addcode lc*code (g-fail))
				(addcode lc*code (g-put-constant lc*actual 1))
			      )
	    )
	    ((arg-var-p lc*actual)
		(addcode lc*code
		 (code-gen-tail-arg 1 perms temps lc*actual chknr lc*litnr)
		)
	    )

            ((eq (s-cg-usrlit_id lc*actual) 'is)
		(addcode lc*code
		 (code-gen-is (cadr lc*actual) (caddr lc*actual)
				 perms temps chknr lc*litnr lc*vpul
                                 (and (null callexeflg) (null (cdr lc*x)))
                 )
                )
            )

	    ((eq (s-cg-usrlit_id lc*actual) 'refl-xreg)
		(addcode lc*code
                (code-gen-tail-arg 1 perms temps (cadr lc*actual) chknr lc*litnr)))

	    ((eq (s-cg-usrlit_id lc*actual) 'usrlit)
		(addcode lc*code
		 (let ((litdescr (s-cg-literal_descr lc*actual)))
		  (setq lc*usrlitflg t)
		  (code-gen-tail perms temps 
			(s-cg-arity litdescr)
			(s-cg-env_size litdescr)
			(s-cg-fac_list lc*actual)
			callexeflg deallocflg chknr lc*litnr 
			(s-cg-arg_seq litdescr)
		  )
		 ) ; let
		) ; addcode
	    ) ; eq usrlit

	    ((cg-inst-p lc*actual)
		(addcode lc*code
		       (doappend
			(code-gen-put-structlist lc*actual 1)
			(code-gen-uni-args (cg-s-inst-funargs lc*actual) 
					perms temps chknr lc*litnr)
		       )
		)
	    )

	   ((cg-lispcall-p lc*actual)
		(addcode lc*code
		 (let ((litdescr (s-cg-literal_descr lc*actual)))
		  (setq lc*usrlitflg t)
		  (code-gen-cl lc*actual perms temps 
			(s-cg-arity litdescr)
			(s-cg-env_size litdescr)
			(s-cg-fac_list lc*actual)
			callexeflg deallocflg chknr lc*litnr 
			(s-cg-arg_seq litdescr)
		  )
		 ) ; let
		) ; addcode
		) ; eq .. cl

	    (t (error "code-gen-hdchunk"))
      ) ; cond
 ) ; do*
) ; defun code-gen-chunk
 

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-CC:
;;;;;; classified_clause: a classified clause is part of a procedure.
;;;;;;			  see EBNF syntax for Classified Datafun.
;;;;;;				"clause_classification"
;;;;;;
;;;;;; Output: List of instructions for the classified clause
;;;;;;  

(defun code-gen-cc (classified_clause)
(let* (	(lc*a_c_type (s-cg-clause_typ classified_clause))
	(lc*a_c_perm_var_list (s-cg-perm_var_list classified_clause))
	(lc*a_c_temp_var_list (s-cg-temp_var_list classified_clause))
	(lc*a_c_chunks 
		(s-cg-chunks classified_clause))
	(lc*hd_chunk (first lc*a_c_chunks))
	(lc*head_lit (s-cg-chunk_head_literal lc*hd_chunk))
	(lc*headdescr (s-cg-literal_descr lc*head_lit))
;	(lc*hd_chunk_descr (s-cg-chunk_descr lc*hd_chunk)) 
;;;;;;;;	(lc*literal_descr (s-cg-literal_descr classified_clause))
	(lc*restchunks (rest lc*a_c_chunks ))
     )
     (cond ((eq lc*a_c_type 'rel0)
		 (doappend
		  (doappend
			(code-gen-head	lc*a_c_perm_var_list
					lc*a_c_temp_var_list
					(s-cg-fac_list lc*head_lit)
					(s-cg-arg_seq lc*headdescr)
			)
		        (g-put-constant 'true 1)
                  )
		  (g-proceed)
                 )
	   )
	   ((or (eq lc*a_c_type 'fun1den) (eq lc*a_c_type 'fun1eva))
			(code-gen-hdchunk lc*a_c_perm_var_list
					  lc*a_c_temp_var_list
					  lc*hd_chunk
					  NIL ; generate execute call
					  NIL ; generate no dealloc
					  1 ; chunknr.
		 	)
					
	   )
	   ((or (eq lc*a_c_type 'fun*den) (eq lc*a_c_type 'fun*eva))
	     (let ( (lc*envsize  (s-cg-env_size lc*headdescr)) )
		(doappend
		(doappend
		        (g-allocate lc*envsize)
			(code-gen-hdchunk lc*a_c_perm_var_list
					  lc*a_c_temp_var_list
					  lc*hd_chunk
					  T   ; generate execute call
					  NIL ; generate no dealloc
					  1 ; chunknr.
		 	)
                ) ; 1st doappend
		      (do* ( (x lc*restchunks (cdr x))
			     (lc*codetail nil)
			     (chk_nr 2 (1+ chk_nr))  )
			   ((null x) lc*codetail)
			   (addcode lc*codetail (code-gen-chunk 
						 lc*a_c_perm_var_list
						 lc*a_c_temp_var_list
						 (car x)
						 (cdr x) ; flag call/exe
						 (null (cdr x))
						 chk_nr
						)
			   ) ; addcode
		      ) ; do
		) ; 2nd doappend
	      ) ; let
	   ) ; fun*den or fun*eva
	(t (error "error in code-gen"))
     ) ; cond
  ) ; let
) ; defun


;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-PROC:
;;;;;; procedure: a classified_procedure 
;;;;;;			  see EBNF syntax for Classified Datafun.
;;;;;;				"clause_classification"
;;;;;;
;;;;;; Output: List of instructions for the procedure.
;;;;;;         side-effect: put the propertylist under the tag: 'procedure

#|
(defun code-gen-proc (procedure)
 (let* ( (id_proc (s-cg-proc-id procedure))
	 (no_of_clauses (s-cg-clause_count procedure))
	 (procedure_arity (s-cg-arity-of-proc procedure))
	 (list_of_clauses (s-cg-clause_classifications procedure))
	 (pred_arity (s-cg-procedure_name procedure))
       )
	(if (not (eq id_proc 'proc)) (error "PROC id expected"))
	(if (eq no_of_clauses 1)
		(putniv  pred_arity 'procedure (code-gen-cc (first list_of_clauses)) )
	    	(putniv pred_arity 'procedure
		 (doappend
		  (g-try-me-else 0 procedure_arity)
		 (do* ( lc*code
			(lc*x list_of_clauses (cdr lc*x))
			(lc*actual_clause (car lc*x) (car lc*x))
			(lc*flg_last_clause (null (cdr lc*x))(null (cdr lc*x)))
			(lc*flg_next_is_last (null (cddr lc*x)) (null (cddr lc*x)))
			(lc*label 0 (1+ lc*label)) )
		     ((null lc*actual_clause) lc*code)
		 (setq lc*code (append lc*code (code-gen-cc lc*actual_clause)))
		 (if (not lc*flg_last_clause) 
			(progn 	(setq lc*code (append lc*code (list lc*label)))
				(if lc*flg_next_is_last
					(setq lc*code (append lc*code
					  (g-trust-me-else-fail 
                                                   procedure_arity)) )
					(setq lc*code (append lc*code
					  (g-retry-me-else 
					    (1+ lc*label) procedure_arity)) )
				) ; if
			) ; progn
		 ) ; if	
		 ) ; do
		) ; doappend
	       ) ; putniv
 	) ; if
 ) ; let*
) ; defun
|#


(defvar *idxlocal*)

(defun code-gen-proc (procedure)
 ; INDEXING -- added by real-fun
 (let* ( (id_proc (s-cg-proc-id procedure))
         (no_of_clauses (s-cg-clause_count procedure))
         (procedure_arity (s-cg-arity-of-proc procedure))
         (list_of_clauses (s-cg-clause_classifications procedure))
         (pred_arity (s-cg-procedure_name procedure))
       )
        (if (not (eq id_proc 'proc)) (error "PROC id expected"))
        (if (eq no_of_clauses 1)
                (putniv  pred_arity 'procedure (code-gen-cc (first list_of_clauses)) )
                (putniv pred_arity 'procedure
;;;;;;;;;; indexing -> append index-header
                 (append
                  (setq *idxlocal*
                  (if (idx) (cdr (mapcan2
                                  #'(lambda (x)
                                            (icg.mk-header
                                             x procedure_arity))
                                  (iif.mk-tree procedure)))))
                 (doappend
;;;;;;;;;; indexing -> no try-me-else instruction
;;;;;;;;;;             but label for first clause
                  (if (not *idxlocal*)
                      (g-try-me-else 0 procedure_arity)
                      '(1))
                 (do* ( lc*code
                        (lc*x list_of_clauses (cdr lc*x))
                        (lc*actual_clause (car lc*x) (car lc*x))
                        (lc*flg_last_clause (null (cdr lc*x))(null (cdr lc*x)))
                        (lc*flg_next_is_last (null (cddr lc*x)) (null (cddr lc*x
)))
;;;;;;;;;; indexing -> other label's
                        (lc*label (if (not *idxlocal*)
                                      0 2) (1+ lc*label)))
                     ((null lc*actual_clause) lc*code)
                 (setq lc*code (append lc*code (code-gen-cc lc*actual_clause)))
                 (if (not lc*flg_last_clause)
                        (progn  (setq lc*code (append lc*code (list lc*label)))
                                (if lc*flg_next_is_last
                                        (setq lc*code (append lc*code
;;;;;;;;;; indexing -> no trust-me-else-fail instruction
                                          (if (not *idxlocal*)
                                              (g-trust-me-else-fail
                                                   procedure_arity))) )
                                        (setq lc*code (append lc*code
;;;;;;;;;; indexing -> no retry-me-else instruction
                                          (if (not *idxlocal*)
                                              (g-retry-me-else
                                            (1+ lc*label) procedure_arity))) )
                                ) ; if
                        ) ; progn
                 ) ; if
                 ) ; do
                ) ; doappend
                ) ; append
               ) ; putniv
        ) ; if
 ) ; let*
) ; defun


(defun code-gen-refl-xreg (perms temps arg chknr litnr)
  (cond	((null arg) (g-get-nil 1))
	((arg-const-p arg) (g-get-constant arg 1))
	((arg-var-p arg) 
	 (if (eq (s-cg-local_var_class arg) 'perm)
		(code-gen-refl-xreg-perm perms arg chknr litnr)
		(code-gen-refl-xreg-temp temps arg)
	 ) ; if
	) ; arg-var-p
	(t (error "code-gen-refl-xreg-arg"))
  ) ; cond
) ; defun

(defun code-gen-refl-xreg-perm (perms arg chknr litnr)
 (let* (
;         (local-var-descr (s-cg-local-var-descr arg))
	  (local-var-occurence (s-cg-local_var_occurrence arg))
	  (local-var-saveness (s-cg-local_var_saveness arg))
	 (perm-descr (get_perm_descr arg perms))
	  (perm-y-nr (s-cg-perm_y_nr perm-descr))
	  (perm-last-ckliteral (s-cg-perm_last_literal perm-descr))
;	  (perm-use-head (s-cg-perm_use_head perm-descr))
       )

	(cond ((firstreuse-p local-var-occurence)
				       (g-get-variable-perm perm-y-nr 1)
	       ) ; occurrence = first or reused
	      ((eq local-var-occurence 'nonfirst)
					(if (and 
					     (eq local-var-saveness 'unsafe)
					     (equal (list chknr litnr)
						    perm-last-ckliteral
					     )
					    )
					   (doappend 
						(g-get-value-perm 
							perm-y-nr 1)
						(g-put-unsafe-value-perm
							perm-y-nr 1)
					   )
					   (g-get-value-perm perm-y-nr 1)
					) ; if
                     ) ; nonfirst
	      (t (error "code-gen-tail-perm"))
	)
    ) ; let
) ; defun


(defun code-gen-refl-xreg-temp (temps arg)
 (let* (
;         (local-var-descr (s-cg-local-var-descr arg))
	  (local-var-occurence (s-cg-local_var_occurrence arg))
;	  (local-var-saveness (s-cg-local_var_saveness arg))
	 (temp-descr (get_temp_descr arg temps))
	  (temp-x-nr (s-cg-temp_x_nr temp-descr))
;	  (temp-use-head (s-cg-temp_use_head temp-descr))
;	  (temp-use-literal (s-cg-temp_use_premise temp-descr))
       )

	(cond  ((firstreuse-p local-var-occurence)
		(cond 	((eq temp-x-nr 1) NIL) ; no code necessary
			(t (g-get-variable-temp temp-x-nr 1) )
                ) ; cond
               ) ; eq
	      ((eq local-var-occurence 'nonfirst)
			(g-get-value-temp temp-x-nr 1) )
	      (t (error "code-gen-refl-xreg-temp"))
	) ; cond
 ) ; let
) ; defun



;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-CL:
;;;;;;	stelle: argument place being processed.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the tempory variables in the clause.
;;;;;;  arity:  arity of the predicate
;;;;;;  permcnt:no. of permanent variable needed after the call-instruction,
;;;;;;          cannot be used within a lispcall
;;;;;;  arg:    constant/nil/permanent/temporary variable
;;;;;;  fac:    the predicate with its arguments
;;;;;;  callexeflg: produce a call (t) or execute (NIL)
;;;;;;  deallocflg: if T a deallocate is generated.
;;;;;;              set to T if it is the last literal to be processed.
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;  
;;;;;; Output: List of instructions for the literal
;;;;;;  

(defun code-select-cl-call (actual arity lispcall_name)
 (cond ((eq actual 'cl-func) (g-cl-func arity lispcall_name))
       ((eq actual 'cl-pred) (g-cl-pred arity lispcall_name))
       ((eq actual 'cl-extra) (g-cl-extra arity lispcall_name))
       ((eq actual 'cl-relf) (g-cl-relf arity lispcall_name))
       (t (error "unknown flavor of lisp-call"))
 )
)

(defun code-gen-cl ( actual perms temps arity permcnt fac callexeflg deallocflg 
		       chknr litnr arg_seq )
 (let* ( (lispcall_name (s-cg-functor fac))
         (lispcall_args (s-cg-arglist_classification fac))
       )
 (null permcnt) ; um compiler auszutricksen
 (do* (	lc*code
        (arg_seqlist arg_seq (cdr arg_seqlist))
        (lc*stelle (car arg_seqlist) (car arg_seqlist))
        (lc*nxtarg (if lc*stelle (nth (1- lc*stelle) lispcall_args))
                   (if lc*stelle (nth (1- lc*stelle) lispcall_args))) )
      ( (null lc*stelle)					; termin. expr
	(if callexeflg						; return result
         (doappend lc*code (code-select-cl-call (car actual) arity lispcall_name ))
         (if deallocflg
	   (doappend
	    (doappend (doappend lc*code (g-deallocate))
			      (code-select-cl-call (car actual) arity lispcall_name)
            )
            (g-proceed)
	   )
	   (doappend (doappend lc*code (code-select-cl-call (car actual) arity lispcall_name))
                     (g-proceed)
           )
	 ) ; if deallocflg
	) ; if callexeflg
      )      
      (addcode lc*code (code-gen-tail-arg lc*stelle
					  perms
					  temps
					  lc*nxtarg 
					  chknr
					  litnr )
      ) ; addcode
  ) ; do*
 ) ; let*
) ; defun

(princ "Codegenerator 0.53, 19.Sep.90")
(terpri)
