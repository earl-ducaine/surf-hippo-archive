; ***********************************************************************
; *                             CGIS.TEX                                *
; ***********************************************************************
; This file handles most of the is codegeneration.

; Comment the following line out, if it is already defined
(defun <> (x y) (not (= x y)))

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-UNI-PERM:
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  arg:    actually the variable to be handled.
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;
;;;;;; Output: List of instructions for unifying permanent variable

(defun code-gen-uni-perm ( perms arg chknr litnr)
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
			 (g-unify-variable-perm perm-y-nr)
	       ) ; occurrence = first or reused

	      ((eq local-var-occurence 'nonfirst)
	   	(cond ((secure local-var-saveness)
			(let ( (y-in-x (is-y-in-x perm-y-nr y-x-usage-list)))
			  (cond (y-in-x ; used safe and already in Xreg ?
				 (g-unify-value-temp  ; direct unify
                                   (sel-x-from-usage-entry y-in-x))
                                )
				(t (g-unify-value-perm perm-y-nr )  )
			  ) ; cond
                        ) ; let
		       ) ; eq

	      ((eq local-var-saveness 'unsafe)
		(cond ((equal (list chknr litnr) perm-last-ckliteral)
			          (g-unify-local-value-perm perm-y-nr)
	               ) ; equal litnr perm-last-ckliteral

		       (t
			(let ( (y-in-x (is-y-in-x perm-y-nr y-x-usage-list)))
			  (cond (y-in-x
				 (g-unify-value-temp 
					   (sel-x-from-usage-entry y-in-x))
                                )
				(t (g-unify-value-perm perm-y-nr)  )
			  ) ; cond
			) ; let
		       ) ; true
	) ; cond
       ) ; eq local-var-safeness 'unsafe

      (t (error "code-uni-tail-perm"))
   ) ; cond
  ); occurence = nonfirst
      (t (error "code-uni-tail-perm"))
     ) ; cond
    ) ; let
) ; defun

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-UNI-TEMP
;;;;;;  temps:  the global info about the temporary variables in the clause.
;;;;;;  arg:    actually the variable to be handled.
;;;;;;
;;;;;; Output: List of instructions for unifying temporary variable

(defun code-gen-uni-temp (temps arg)
 (let* (
	  (local-var-occurence (s-cg-local_var_occurrence arg))
	 (temp-descr (get_temp_descr arg temps))
	  (temp-x-nr (s-cg-temp_x_nr temp-descr))
       )
	(cond ((firstreuse-p local-var-occurence)
		(g-unify-variable-temp temp-x-nr) )
	      ((eq local-var-occurence 'nonfirst)
                      (g-unify-value-temp temp-x-nr)
	      )
	      (t (error "code-gen-uni-temp"))
	)
 ) ; let
) ; defun

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-UNI-ARGS:
;;;;;;  args:   the arguments where unify code is to be generated.
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the temporary variables in the clause.
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;
;;;;;; Output: List of instructions for unifying the arguments

(defun code-gen-uni-args (args perms temps chknr litnr)
(declare (special *lureg*))
(do* ( lc*code
      (lc*args args (cdr lc*args))
      (arg (car lc*args) (car lc*args))
     )
    ((null lc*args) lc*code)
  
 (cond	((null arg) (addcode lc*code (g-unify-nil)) )
	((arg-const-p arg) (addcode lc*code (g-unify-constant arg)))
	((arg-var-p arg) 
	 (if (eq (s-cg-local_var_class arg) 'perm)
		(addcode lc*code (code-gen-uni-perm perms arg chknr litnr))
		(addcode lc*code (code-gen-uni-temp temps arg)))
	)
	(t (error "code-gen-uni-args"))
 ) ; cond
) ; do*
) ; defun


;;;;;; ------------------------------------------------------------------------
;;;;;; GETLUREG:
;;;;;; Output: The next free X variable which is not yet occupied

(defun getlureg()
 (setq *lureg* (1+ *lureg*))
)

;;;;;; ------------------------------------------------------------------------
;;;;;; USEVALREG: (to be used for (is <Y1> <Y2>) )
;;;;;;  putin1: to bet set to t, if it should be put in X1 (pseudochunk)
;;;;;;  vpul  : the chunkdescription
;;;;;;  y1name: first Y variable
;;;;;;  y2name: second Y variable

(defun usevalreg (putin1 vpul y1name y2name)
 (if putin1 1
 (let ((vorschlag nil))
  (if y1name (setq vorschlag (caadr (assoc y1name vpul :test #'equal)))
   (if y2name (setq vorschlag (caadr (assoc y2name vpul :test #'equal))))
  )
  (if (null vorschlag) (getlureg) vorschlag)
  ) ; let
 ) ; if putin1
) ; defun


;;;;;; ------------------------------------------------------------------------
;;;;;; RPUTINXY: (Further code optimizations possible ?)
;;;;;;  putin1: to be set to t, if X-variable must be transfered to X1
;;;;;;  x     : Register with is to be transfered.

(defun rputinxy (putin1 x)
 (if (and putin1 (<> x 1))
	    (g-movesd x 1)
	    nil
        )
)


;;;;;; ------------------------------------------------------------------------
;;;;;; RPUTIN1X* : (Further code optimizations possible ?)
;;;;;;  putin1: to be set to t, if X-variable must be transfered to X1
;;;;;;  x1    : Register with is to be transfered.


(defun rputin1x* (putin1 x1)
	(if (and putin1 (<> x1 1))
	    (g-movesd x1 1)
	    nil
        )
)

;;;;;; ------------------------------------------------------------------------
;;;;;; RPUTIN1XX : (Further code optimizations possible ?)
;;;;;;  putin1: to be set to t, if X-variable must be transfered to X1
;;;;;;  x1    : Register with is to be transfered.
;;;;;;  x2    : Register with is to be transfered.
;;;;;; is used when (is var1 var2) are both X variables.

(defun rputin1xx (putin1 x1 x2)
	(if (and putin1 (<> x1 1) (<> x2 1))
	    (g-movesd x1 1)
	    nil
        )
)

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-IS
;;;;;; More than 30 cases must be distinguished. If you intend to extend the
;;;;;; WAM you may have specialised instructions and the function gets MUCH
;;;;;; smaller.. If you want to use the
;;;;;; NORMAL instructions (and generated fairly good code) you should
;;;;;; do it in this way.
;;;;;;  arg1:   the left hand side of the (is ..)
;;;;;;  arg2:   the right hand side of the (is ..)
;;;;;;  perms:  the global info about the permanent variables in the clause.
;;;;;;  temps:  the global info about the temporary variables in the clause.
;;;;;;  chknr:  The chunknumber in the tail of the clause.(start with 1)
;;;;;;  litnr:  The literal number in the chunk. (start with 1)
;;;;;;  vpul:   chunkdescription
;;;;;;  putin1: if the argument is to be put in register X1 (to avoid a move)
;;;;;;
;;;;;; Output: List of instructions for (is ....)



(defun code-gen-is (arg1 arg2 perms temps chknr litnr vpul putin1)
 (declare (special y-x-usage-list))
 (let* ( 


	local1-var-name local1-var-class local1-var-descr 
	local1-var-occurrence local1-var-saveness
	perm1-descr perm1-y-nr perm1-last-literal perm1-use-head
	temp1-descr temp1-x-nr temp1-use-head temp1-use-literal

	local2-var-name local2-var-class local2-var-descr 
	local2-var-occurrence local2-var-saveness
	perm2-descr perm2-y-nr perm2-last-literal perm2-use-head
	temp2-descr temp2-x-nr temp2-use-head temp2-use-literal
        valreg hilf

       )

 (if (arg-var-p arg1)
  (progn (setq local1-var-class (s-cg-local_var_class arg1))
           (if (eq local1-var-class 'perm)
              (progn
		  (setq local1-var-name (s-cg-perm_var arg1))
		  (setq local1-var-descr (s-cg-local-var-descr arg1))
		  (setq local1-var-occurrence (s-cg-local_var_occurrence arg1))
		  (setq local1-var-saveness (s-cg-local_var_saveness arg1))
		  (setq perm1-descr (get_perm_descr arg1 perms))
		  (setq perm1-y-nr (s-cg-perm_y_nr perm1-descr)) 
		  (setq perm1-last-literal (s-cg-perm_last_literal perm1-descr))
		  (setq perm1-use-head (s-cg-perm_use_head perm1-descr))
	      )

	      (progn
		  (setq local1-var-name (s-cg-temp_var arg1))
		  (setq local1-var-descr (s-cg-local-var-descr arg1))
		  (setq local1-var-occurrence (s-cg-local_var_occurrence arg1))
		  (setq local1-var-saveness (s-cg-local_var_saveness arg1))
		  (setq temp1-descr (get_temp_descr arg1 temps))
		  (setq temp1-x-nr (s-cg-temp_x_nr temp1-descr))
		  (setq temp1-use-head (s-cg-temp_use_head temp1-descr))
		  (setq temp1-use-literal (s-cg-temp_use_premise temp1-descr))
	      )
	   ) ; if  .. 'perm
  )
 ) ; if arg-var-p arg1


 (if (arg-var-p arg2)
  (progn (setq local2-var-class (s-cg-local_var_class arg2))
           (if (eq local2-var-class 'perm)
              (progn
		  (setq local2-var-name (s-cg-perm_var arg2))
		  (setq local2-var-descr (s-cg-local-var-descr arg2))
		  (setq local2-var-occurrence (s-cg-local_var_occurrence arg2))
		  (setq local2-var-saveness (s-cg-local_var_saveness arg2))
		  (setq perm2-descr (get_perm_descr arg2 perms))
		  (setq perm2-y-nr (s-cg-perm_y_nr perm2-descr)) 
		  (setq perm2-last-literal (s-cg-perm_last_literal perm2-descr))
		  (setq perm2-use-head (s-cg-perm_use_head perm2-descr))
	      )

	      (progn
		  (setq local2-var-name (s-cg-temp_var arg2))
		  (setq local2-var-descr (s-cg-local-var-descr arg2))
		  (setq local2-var-occurrence (s-cg-local_var_occurrence arg2))
		  (setq local2-var-saveness (s-cg-local_var_saveness arg2))
		  (setq temp2-descr (get_temp_descr arg2 temps))
		  (setq temp2-x-nr (s-cg-temp_x_nr temp2-descr))
		  (setq temp2-use-head (s-cg-temp_use_head temp2-descr))
		  (setq temp2-use-literal (s-cg-temp_use_premise temp2-descr))
	      )
	   ) ; if  .. 'perm
  )
 ) ; if arg-var-p arg1


 (cond	
	((cg-inst-p arg1)
	 (cond ((eq local2-var-class 'temp) 
		(doappend
		 (if (firstreuse-p local2-var-occurrence)
			(code-gen-get-structlist arg1 temp2-x-nr)
			(code-gen-get-structlist arg1 temp2-x-nr)
		 )
		(code-gen-uni-args (cg-s-inst-funargs arg1) 
					perms temps chknr litnr)
	       ) ; doappend
	      ) ; eq .. temp
	      ((eq local2-var-class 'perm)
		(doappend
			(code-gen-tail-perm (setq hilf (getlureg))
						perms arg2 chknr litnr)
		 (doappend		 
		 (if (firstreuse-p local2-var-occurrence)
			(code-gen-put-structlist arg1 hilf)
			(code-gen-get-structlist arg1 hilf)
		 ) ; if
		(code-gen-uni-args (cg-s-inst-funargs arg1)
					perms temps chknr litnr)
	        ) ; doappend
	       ) ; doappend
	     ) ; eq .. perm
	 ) ; cond
	) ; .... cg-inst-p arg1

	((cg-inst-p arg2)
	 (cond ((eq local1-var-class 'temp) 
		(doappend
		 (if (firstreuse-p local1-var-occurrence)
			(code-gen-put-structlist arg2 temp1-x-nr)
			(code-gen-get-structlist arg2 temp1-x-nr)
		 )
		(code-gen-uni-args (cg-s-inst-funargs arg2)
					perms temps chknr litnr)
	       ) ; doappend
	      ) ; eq .. temp
	      ((eq local1-var-class 'perm)
		(doappend
			(code-gen-tail-perm (setq hilf (getlureg))
						perms arg1 chknr litnr)
		 (doappend		 
		 (if (firstreuse-p local1-var-occurrence)
			(code-gen-get-structlist arg2 hilf)
			(code-gen-get-structlist arg2 hilf)
		 ) ; if
		(code-gen-uni-args (cg-s-inst-funargs arg2)
					perms temps chknr litnr)
	        ) ; doappend
	       ) ; doappend
	     ) ; eq .. perm
	 ) ; cond
	) ; .... cg-inst-p arg1

	((and (eq local1-var-class 'temp) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'temp) (firstreuse-p local2-var-occurrence)
        )
        (doappend (g-get-variable-temp temp1-x-nr temp2-x-nr) ;	|1|
	          (rputin1xx putin1 temp1-x-nr temp2-x-nr)
        )
       )
       ((and (eq local1-var-class 'temp) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'temp) (eq local2-var-occurrence 'nonfirst)
        )
	(doappend (g-put-value-temp temp2-x-nr temp1-x-nr) ; 	|2|
	          (rputin1xx putin1 temp1-x-nr temp2-x-nr)
        )
       )
       ((and (eq local1-var-class 'temp) (eq local1-var-occurrence 'nonfirst)
	     (eq local2-var-class 'temp) (firstreuse-p local2-var-occurrence)
       
        )
        (doappend (g-put-value-temp temp1-x-nr temp2-x-nr) ;	|3|
	          (rputin1xx putin1 temp1-x-nr temp2-x-nr)
        )
       )
       ((and (eq local1-var-class 'temp) (eq local1-var-occurrence 'nonfirst)
	     (eq local2-var-class 'temp) (eq local2-var-occurrence 'nonfirst)
       
        )
        (doappend (g-get-value-temp temp1-x-nr temp2-x-nr) ;	|4|
		  (rputin1xx putin1 temp1-x-nr temp2-x-nr)
	)
       )


       ((and (eq local1-var-class 'perm) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'temp) (firstreuse-p local2-var-occurrence)
        )
	(doappend					; |5|
		 (g-put-variable-perm perm1-y-nr temp2-x-nr)
	 	 (rputin1x* putin1 temp2-x-nr)
	)
       )

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (secure local1-var-saveness)
	     (eq local2-var-class 'temp) (firstreuse-p local2-var-occurrence)
        )

	(doappend 
	(let ( (y-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (g-movesd  ; direct move
                                 (sel-x-from-usage-entry y-in-x) temp2-x-nr )
                              )
		(t	 (g-put-value-perm perm1-y-nr temp2-x-nr)  )
	  ) ; cond
         ) ; let

        (rputin1x* putin1 temp2-x-nr )
	)
       )

       ((and (eq local1-var-class 'temp) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'perm) (firstreuse-p local2-var-occurrence)
        )
        (doappend (g-put-variable-perm perm2-y-nr temp1-x-nr) ; |7|
		  (rputin1x* putin1 temp1-x-nr )
	)
       )

       ((and (eq local1-var-class 'temp) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (secure local2-var-saveness)
       
        )
	(doappend				; 		|8|
	(let ( (y-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (g-movesd  ; direct move
                                 (sel-x-from-usage-entry y-in-x) temp1-x-nr )
                              )
		(t (g-put-value-perm perm2-y-nr temp1-x-nr) )
	  ) ; cond
         ) ; let
        (rputin1x* putin1 temp1-x-nr )
	) ; doappend
       )

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (eq local1-var-saveness 'unsafe)
	     (eq local2-var-class 'temp) (firstreuse-p local2-var-occurrence)
       
        )
	(doappend 
	 (if (equal (list chknr litnr) perm1-last-literal)
		(g-put-unsafe-value perm1-y-nr temp2-x-nr) ; 	|9|
; else
	(let ( (y-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (g-movesd  ; direct move
                                 (sel-x-from-usage-entry y-in-x) temp2-x-nr )
                              )
		(t (g-put-value-perm perm1-y-nr temp2-x-nr) )
	  ) ; cond
         ) ; let
; endelse
	) ; if
	(rputinxy putin1 temp2-x-nr )
       ) ; doappend
       ) ; and

       ((and (eq local1-var-class 'temp) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (eq local2-var-saveness 'unsafe)
       
        )
	(doappend 
	 (if (equal (list chknr litnr) perm2-last-literal)
		(g-put-unsafe-value perm2-y-nr temp1-x-nr) ; 	|10|
; else
	(let ( (y-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (g-movesd  ; direct move
                                 (sel-x-from-usage-entry y-in-x) temp1-x-nr )
                              )
		(t (g-put-value-perm perm2-y-nr temp1-x-nr) )
	  ) ; cond
         ) ; let
; endelse
	) ; if
	(rputinxy putin1 temp1-x-nr)
       ) ; doappend
       ) ; and

       ((and (eq local1-var-class 'perm) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'temp) (eq local2-var-occurrence 'nonfirst)
       
        )
	(doappend (g-get-variable-perm perm1-y-nr temp2-x-nr) ;	|11|
		(rputinxy putin1 temp2-x-nr)
	)
       )

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (secure local1-var-saveness)
	     (eq local2-var-class 'temp) (eq local2-var-occurrence 'nonfirst)
       
        )

	(doappend						; |12|
	         (let ( (y-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
		  (cond (y-in-x ; used safe and already in Xreg ?
			 (g-get-value-temp  ; direct move
                                  (sel-x-from-usage-entry y-in-x) temp2-x-nr )
                               )
			(t (g-get-value-perm perm1-y-nr temp2-x-nr)  )
		  ) ; cond
                ) ; let
		(rputinxy putin1 temp2-x-nr)
	)
       )

       ((and (eq local1-var-class 'temp) (eq local1-var-occurrence 'nonfirst)
	     (eq local2-var-class 'perm) (firstreuse-p local2-var-occurrence)
       
        )
	(doappend (g-get-variable-perm perm2-y-nr temp1-x-nr) ;		|13|
		  (rputinxy putin1 temp1-x-nr)
	)
       )

       ((and (eq local1-var-class 'temp) (eq local1-var-occurrence 'nonfirst)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (secure local2-var-saveness)
       
        )
	(doappend						; |14|

	         (let ( (y-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
		  (cond (y-in-x ; used safe and already in Xreg ?
			 (g-get-value-temp  
                                  (sel-x-from-usage-entry y-in-x) temp1-x-nr )
                               )
			(t
				(g-get-value-perm perm2-y-nr temp1-x-nr)  )
		  ) ; cond
                ) ; let
	  (rputinxy putin1 temp1-x-nr)
	)
       )


       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (eq local1-var-saveness 'unsafe)
	     (eq local2-var-class 'temp) (eq local2-var-occurrence 'nonfirst)
       
        )

		 					;	|15|
	 (if (equal (list chknr litnr) perm1-last-literal)
	       (progn
		(setq valreg (usevalreg putin1 vpul local1-var-name nil ))
		(doappend (doappend (g-put-unsafe-value-perm perm1-y-nr valreg) 
			            (g-get-value-temp temp2-x-nr valreg)
			  )
		 (rputinxy putin1 valreg)
                ) ; doappend
	       ) ;progn
; else
	(let ( (y-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (doappend 
		  (g-get-value-temp  ; direct move
                                 (sel-x-from-usage-entry y-in-x) temp2-x-nr )
		 (rputinxy putin1 temp2-x-nr)
		 ) ; doappend
		) ; y-in-x
		(t
		  (progn (setq valreg (usevalreg putin1 vpul local1-var-name nil ))
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-value-temp temp2-x-nr valreg)
				   )
		 	 	   (rputinxy putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond y-in-x
         ) ; let
; endelse
	) ; if
        ) ; and


       ((and (eq local1-var-class 'temp) (eq local1-var-occurrence 'nonfirst)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (eq local2-var-saveness 'unsafe)
        )

;									|16|
	 (if (equal (list chknr litnr) perm2-last-literal)
	       (progn
		(setq valreg (usevalreg putin1 vpul local2-var-name nil ))
		(doappend (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) 
			            (g-get-value-temp temp1-x-nr valreg)
			  )
		 (rputinxy putin1 valreg)
                ) ; doappend
	       ) ;progn
; else
	(let ( (y-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (doappend 
		  (g-get-value-temp  ; direct move
                                 (sel-x-from-usage-entry y-in-x) temp1-x-nr )
		 (rputinxy putin1 temp1-x-nr)
		 ) ; doappend
		) ; y-in-x
		(t
		  (progn (setq valreg (usevalreg putin1 vpul local2-var-name nil ))
			 (doappend (doappend 
					(g-put-value-perm perm2-y-nr valreg)
			 		(g-get-value-temp temp1-x-nr valreg)
				   )
		 	 	   (rputinxy putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond y-in-x
         ) ; let
; endelse
	) ; if
) ; |16|

       ((and (eq local1-var-class 'perm) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'perm) (firstreuse-p local2-var-occurrence)
       
        )
	(progn (setq valreg (usevalreg putin1 vpul local1-var-name local2-var-name))
        (doappend (g-put-variable-perm perm1-y-nr valreg) ;		|17|
		  (g-get-variable-perm perm2-y-nr valreg)
	)
        )
       )

       ((and (eq local1-var-class 'perm) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (secure local2-var-saveness)
       
        )
;									|18|
	(let ( (y-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (progn (setq valreg (sel-x-from-usage-entry y-in-x))
		  (doappend
		   (g-get-variable-perm perm1-y-nr valreg)
		   (rputin1x* putin1 valreg)
		  ) ; doappend
		 ) ; prog
		) ; y-in-x
		(t
		  (progn (setq valreg (usevalreg putin1 vpul 
					local1-var-name local1-var-name)
			 )
			 (doappend (doappend 
					(g-put-value-perm perm2-y-nr valreg)
			 		(g-get-variable-perm perm1-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond y-in-x
         ) ; let
	) ; and


       ((and (eq local1-var-class 'perm) (firstreuse-p local1-var-occurrence)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (eq local2-var-saveness 'unsafe)
       
        )
;									|19|
	 (if (equal (list chknr litnr) perm2-last-literal)
	       (progn
		(setq valreg (usevalreg putin1 vpul local2-var-name nil ))
		(doappend (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) 
			            (g-get-variable-perm perm1-y-nr valreg)
			  )
		 (rputin1x* putin1 valreg)
                ) ; doappend
	       ) ;progn
; else
	(let ( (y-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (progn (setq valreg (sel-x-from-usage-entry y-in-x))
		  (doappend
		   (g-get-variable-perm perm1-y-nr valreg)
		   (rputin1x* putin1 valreg)
		  ) ; doappend
		 ) ; prog
		) ; y-in-x
		(t
		  (progn (setq valreg (usevalreg putin1 vpul
					 local1-var-name local2-var-name ))
			 (doappend (doappend 
					(g-put-value-perm perm2-y-nr valreg)
			 		(g-get-variable-perm perm1-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond y-in-x
         ) ; let
; endelse
	) ; if
       ) ; and

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (secure local1-var-saveness)
	     (eq local2-var-class 'perm) (firstreuse-p local2-var-occurrence)
       
        )

;									|20|

	(let ( (y-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (progn (setq valreg (sel-x-from-usage-entry y-in-x))
		  (doappend
		   (g-get-variable-perm perm2-y-nr valreg)
		   (rputin1x* putin1 valreg)
		  ) ; doappend
		 ) ; prog
		) ; y-in-x
		(t
		  (progn (setq valreg (usevalreg putin1 vpul 
					local1-var-name local2-var-name)
			 )
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-variable-perm perm2-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond y-in-x
         ) ; let
       ) ; and




       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (eq local1-var-saveness 'unsafe)
	     (eq local2-var-class 'perm) (firstreuse-p local2-var-occurrence)
       
        )

;									|21|
	 (if (equal (list chknr litnr) perm1-last-literal)
	       (progn
		(setq valreg (usevalreg putin1 vpul local1-var-name 
				local2-var-name ))
		(doappend (doappend (g-put-unsafe-value-perm perm1-y-nr valreg) 
			            (g-get-variable-perm perm2-y-nr valreg)
			  )
		 (rputin1x* putin1 valreg)
                ) ; doappend
	       ) ;progn
; else
	(let ( (y-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
	  (cond (y-in-x ; used safe and already in Xreg ?
		 (progn (setq valreg (sel-x-from-usage-entry y-in-x))
		  (doappend
		   (g-get-variable-perm perm2-y-nr valreg)
		   (rputin1x* putin1 valreg)
		  ) ; doappend
		 ) ; prog
		) ; y-in-x
		(t
		  (progn (setq valreg (usevalreg putin1 vpul
					 local1-var-name local2-var-name ))
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-variable-perm perm2-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond y-in-x
         ) ; let
; endelse
	) ; if
       ) ; and


       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (secure local1-var-saveness)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (secure local2-var-saveness)
       
        )


	(let (	(y1-in-x (is-y-in-x perm1-y-nr y-x-usage-list))
		(y2-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
	  (cond ((and y1-in-x y2-in-x)					; |22|
		 (doappend
		  (g-get-value-temp 
		 		(setq valreg (sel-x-from-usage-entry y1-in-x))
		 		(sel-x-from-usage-entry y2-in-x)
		  )
		  (rputin1x* putin1 valreg)
		 )
		)
		( y1-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y1-in-x))
		   (doappend
		    (g-get-value-perm perm2-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)
		( y2-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y2-in-x))
		   (doappend
		    (g-get-value-perm perm1-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)

		(t (progn 
			 (setq valreg (usevalreg putin1 vpul local1-var-name 
					local2-var-name ))
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-value-perm perm2-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond ...
         ) ; let
; endelse
	) ; and

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (secure local1-var-saveness)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (eq local2-var-saveness 'unsafe)
       
        )
	 (if (equal (list chknr litnr) perm2-last-literal)
	       (progn							; |23|
		(setq valreg (usevalreg putin1 vpul local1-var-name 
				local2-var-name ))
		(doappend (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) 
			  (let ((y1-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
				(if y1-in-x (g-get-value-temp 
					     (sel-x-from-usage-entry y1-in-x)
					     valreg
					    )
					    (g-get-value-perm perm1-y-nr valreg)
				)
			  )
			  ) ; doappend
		 (rputin1x* putin1 valreg)
		)
                ) ; progn  --- ifend 
 ; else

	(let (	(y1-in-x (is-y-in-x perm1-y-nr y-x-usage-list))
		(y2-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))

	  (cond ((and y1-in-x y2-in-x)	
		 (doappend
		  (g-get-value-temp 
		 		(setq valreg (sel-x-from-usage-entry y1-in-x))
		 		(sel-x-from-usage-entry y2-in-x)
		  )
		  (rputin1x* putin1 valreg)
		 )
		)
		( y1-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y1-in-x))
		   (doappend
		    (g-get-value-perm perm2-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)
		( y2-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y2-in-x))
		   (doappend
		    (g-get-value-perm perm1-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)

		(t (progn
			 (setq valreg (usevalreg putin1 vpul local1-var-name 
					local2-var-name ))
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-value-perm perm2-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond ...
         ) ; let
; endelse
	) ; if
	) ; and


       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (eq local1-var-saveness 'unsafe)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (secure local2-var-saveness)
       
        )
        (pprint y-x-usage-list)
	 (if (equal (list chknr litnr) perm1-last-literal)
	       (progn							; |24|
		(setq valreg (usevalreg putin1 vpul local1-var-name 
				local2-var-name ))
		(doappend (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) 
			  (let ((y2-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
				(if y2-in-x (g-get-value-temp 
					     (sel-x-from-usage-entry y2-in-x)
					     valreg
					    )
					    (g-get-value-perm perm2-y-nr valreg)
				)
			  )
			  ) ; doappend
		 (rputin1x* putin1 valreg)
		)
                ) ; progn  --- ifend 
 ; else

	(let (	(y1-in-x (is-y-in-x perm1-y-nr y-x-usage-list))
		(y2-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))

	  (cond ((and y1-in-x y2-in-x)	
		 (doappend
		  (g-get-value-temp 
		 		(setq valreg (sel-x-from-usage-entry y1-in-x))
		 		(sel-x-from-usage-entry y2-in-x)
		  )
		  (rputin1x* putin1 valreg)
		 )
		)
		( y1-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y1-in-x))
		   (doappend
		    (g-get-value-perm perm2-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)
		( y2-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y2-in-x))
		   (doappend
		    (g-get-value-perm perm1-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)

		(t (progn
			 (setq valreg (usevalreg putin1 vpul local1-var-name 
					local2-var-name ))
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-value-perm perm2-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond ...
         ) ; let
; endelse
	) ; if
	) ; and

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (eq local1-var-saveness 'unsafe)
	     (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (eq local2-var-saveness 'unsafe)
       
        )

(cond
	( (and 	(equal (list chknr litnr) perm1-last-literal)
		(equal (list chknr litnr) perm2-last-literal)
	  )
	       (progn							; |25|
		(setq valreg (usevalreg putin1 vpul local1-var-name 
				local2-var-name ))
		(doappend
		(doappend (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) 
				    (g-put-unsafe-value-perm perm1-y-nr valreg) 
			  )
			  (g-get-value-perm perm2-y-nr valreg)
		)
		 (rputin1x* putin1 valreg)
		)
	       )
	)

	( (equal (list chknr litnr) perm1-last-literal)
	       (progn
		(setq valreg (usevalreg putin1 vpul local1-var-name 
				local2-var-name ))
		(doappend (doappend (g-put-unsafe-value-perm perm1-y-nr valreg) 
			  (let ((y2-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))
				(if y2-in-x (g-get-value-temp 
					     (sel-x-from-usage-entry y2-in-x)
					     valreg
					    )
					    (g-get-value-perm perm2-y-nr valreg)
				)
			  )
			  ) ; doappend
		 (rputin1x* putin1 valreg)
		)
                ) ; progn
	)

	( (equal (list chknr litnr) perm2-last-literal)
	       (progn
		(setq valreg (usevalreg putin1 vpul local1-var-name 
				local2-var-name ))
		(doappend (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) 
			  (let ((y1-in-x (is-y-in-x perm1-y-nr y-x-usage-list)))
				(if y1-in-x (g-get-value-temp 
					     (sel-x-from-usage-entry y1-in-x)
					     valreg
					    )
					    (g-get-value-perm perm1-y-nr valreg)
				)
			  )
			  ) ; doappend
		 (rputin1x* putin1 valreg)
		)
                ) ; progn
	)



	(t
	(let (	(y1-in-x (is-y-in-x perm1-y-nr y-x-usage-list))
		(y2-in-x (is-y-in-x perm2-y-nr y-x-usage-list)))

	  (cond ((and y1-in-x y2-in-x)	
		 (doappend
		  (g-get-value-temp 
		 		(setq valreg (sel-x-from-usage-entry y1-in-x))
		 		(sel-x-from-usage-entry y2-in-x)
		  )
		  (rputin1x* putin1 valreg)
		 )
		)
		( y1-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y1-in-x))
		   (doappend
		    (g-get-value-perm perm2-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)
		( y2-in-x
		  (progn (setq valreg (sel-x-from-usage-entry y2-in-x))
		   (doappend
		    (g-get-value-perm perm1-y-nr valreg)
 	 	    (rputin1x* putin1 valreg)
		   )
		  )
		)

		(t (progn
			 (setq valreg (usevalreg putin1 vpul local1-var-name 
					local2-var-name ))
			 (doappend (doappend 
					(g-put-value-perm perm1-y-nr valreg)
			 		(g-get-value-perm perm2-y-nr valreg)
				   )
		 	 	   (rputin1x* putin1 valreg)
			 )
		  ) ; progn
		) ; true
	  ) ; cond ...
         ) ; let
; endelse
	) ; T
	) ; cond
      ) ; and


       ((and (eq local1-var-class 'temp) (firstreuse-p local1-var-occurrence)
	     (atom arg2)
       
        )
         (doappend (if (null arg2) (g-put-nil temp1-x-nr) ;		|26|
			(g-put-constant arg2 temp1-x-nr))
		  (rputin1x* putin1 temp1-x-nr)
         )
       )
       ((and (eq local1-var-class 'temp) (eq local1-var-occurrence 'nonfirst)
	     (atom arg2)
       
        )
         (doappend (if (null arg2) (g-get-nil temp1-x-nr) ;		|27|
			(g-get-constant arg2 temp1-x-nr))
		   (rputin1x* putin1 temp1-x-nr)
	 )
       )
       ((and (eq local2-var-class 'temp) (firstreuse-p local2-var-occurrence)
	     (atom arg1)
       
        )
         (doappend (if (null arg1) (g-put-nil temp2-x-nr) ;		|28|
			(g-put-constant arg1 temp2-x-nr))
		   (rputin1x* putin1 temp2-x-nr)
         )
	)

       ((and (eq local2-var-class 'temp) (eq local2-var-occurrence 'nonfirst)
	     (atom arg1)       
        )
         (doappend (if (null arg1) (g-get-nil temp2-x-nr) ;		|29|
			(g-get-constant arg1 temp2-x-nr))
		   (rputin1x* putin1 temp2-x-nr)
	 )
       )

       ((and (eq local1-var-class 'perm) (firstreuse-p local1-var-occurrence)
	     (atom arg2)
       
        )
	(progn (setq valreg (usevalreg  putin1 vpul local1-var-name nil))
        (doappend (g-put-variable-perm perm1-y-nr valreg)
		(if (null arg2) (g-get-nil valreg) ;			|30|
			      (g-get-constant arg2 valreg))
        )
        ) ; progn
       )

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (secure local1-var-saveness)
	     (atom arg2)
       
        )
	(progn (setq valreg (usevalreg  putin1 vpul local1-var-name nil))
	        (doappend (g-put-value-perm perm1-y-nr valreg)
			(if (null arg2) (g-get-nil valreg) ;		|31|
			      (g-get-constant arg2 valreg))

        )
       )
       )

       ((and (eq local1-var-class 'perm) (eq local1-var-occurrence 'nonfirst)
	     (eq local1-var-saveness 'unsafe)
	     (atom arg2)
       
        )
	(progn (setq valreg (usevalreg  putin1 vpul local1-var-name nil))
	        (doappend (g-put-unsafe-value-perm perm1-y-nr valreg) ;	|32|
	         (if (null arg2) (g-get-nil valreg) ;
			      (g-get-constant arg2 valreg))
	        )
        )
       )

       ((and (eq local2-var-class 'perm) (firstreuse-p local2-var-occurrence)
	     (atom arg1)
       
        )
	(progn (setq valreg (usevalreg  putin1 vpul local2-var-name nil))
	        (doappend (g-put-variable-perm perm2-y-nr valreg)
			  (if (null arg1) (g-get-nil valreg) ;		|32a|
			      (g-get-constant arg1 valreg))
        	)
        )
       )
       ((and (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (secure local2-var-saveness)
	     (atom arg1)
        )
	(progn (setq valreg (usevalreg  putin1 vpul local1-var-name nil))
        (doappend (g-put-value-perm perm2-y-nr valreg)
		(if (null arg1) (g-get-nil valreg) ;			|33|
			      (g-get-constant arg1 valreg))
	      
        )
       )
       )
       ((and (eq local2-var-class 'perm) (eq local2-var-occurrence 'nonfirst)
	     (eq local2-var-saveness 'unsafe)
	     (atom arg1)
        )
	(progn (setq valreg (usevalreg  putin1 vpul local1-var-name nil))
        (doappend (g-put-unsafe-value-perm perm2-y-nr valreg) ;		|34|
	      (if (null arg2) (g-get-nil valreg) ;
			      (g-get-constant arg1 valreg))
        )
       )
       )
       (t (error "error in code-gen-is"))
    ) ; cond
 ) ; let*
) ; defun
