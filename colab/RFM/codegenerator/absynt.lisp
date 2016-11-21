; (declare (special *hghdebug*))
(defvar *hgdebug* nil)
(defvar inst-proc)

(defun cr-return()							; !TST!
 (princ "<Return>")							; !TST!
 (read-line) )								; !TST!

;(defmacro test (x)							; !TST!
; (if *hgdebug* (progn (pprint x) (cr-return) (print "->") 		; !TST!
;	(pprint (eval x)) (cr-return)) ))				; !TST!

;;;;;; Code Generator for Classified Datalog - Abstract syntax definitions
;;;;;;
;;;;;; based on "EBNF syntax for Classified Datalog" by Thomas Krause  
;;;;;; DFKI / arCtec
;;;;;; Hans - Guenther HEIN
;;;;;; OKT/NOV 89

;;;;;; To see some examples, place (setq *hgdebug* t) in front of an	; !TST!
;;;;;; entrypoint marked in the source!					; !TST!

;;;;;; Test examples are for the following procedure (1 clause):	; !TST!
;;;;;;             ((p _a _b alpha _b)					; !TST!
;;;;;;              (q1 _b _c )						; !TST!
;;;;;;              (q2 _b _c beta)					; !TST!
;;;;;;              (q3 _a _e _e))					; !TST!
;;;;;; The classified version is below:					; !TST!
									; !TST!
(SETQ inst-proc '							; !TST!
 (proc likes/2								; !TST!
       7								; !TST!
       (fun*eva ((_pers2 (1 (2) 2)))					; !TST!
                ((_pers1 (1 (1) (1))))					; !TST!
                (chunk							; !TST!
                  ((usrlit						; !TST!
                     (likes						; !TST!
                       (_pers1 (first safe temp))			; !TST!
                       (_pers2 (first safe perm))			; !TST!
                     )							; !TST!
                     (2 1 (2))						; !TST!
                   )							; !TST!
                   (usrlit						; !TST!
                    (likes						; !TST!
                      (_pers1 (nonfirst safe temp))			; !TST!
                      mary						; !TST!
                    )							; !TST!
                    (2 1 (2))						; !TST!
                   )							; !TST!
                   )							; !TST!
                   nil							; !TST!
                 )							; !TST!
                 (chunk 						; !TST!
                   ((usrlit						; !TST!
                      (likes						; !TST!
                        (_pers2 (nonfirst safe perm))			; !TST!
                        mary						; !TST!
                      )							; !TST!
                      (2 0 (2 1))					; !TST!
                    )							; !TST!
                   ); usrlit						; !TST!
                   (( _pers2 (1)))					; !TST!
                 )							; !TST!
       )								; !TST!
       (rul0 nil							; !TST!
	     nil							; !TST!
	     (chunk							; !TST!
              ((usrlit (likes john mary) (2 0 (1 2))))			; !TST!
              nil							; !TST!
             )								; !TST!
       )								; !TST!
       (fun1den nil							; !TST!
		nil							; !TST!
		(chunk							; !TST!
		 ((usrlit (likes john mary) (2 0 (1 2))) true )		; !TST!
                 nil							; !TST!
                )							; !TST!
       )								; !TST!
       (fun1den nil							; !TST!
                ((_pers1 (1 (1) nil))					; !TST!
                 (_pers2 (2 (2) nil))					; !TST!
                )							; !TST!
                (chunk							; !TST!
                 ((usrlit						; !TST!
                   (likes						; !TST!
                     (_pers1 (first safe temp))				; !TST!
                     (_pers2 (first safe temp))				; !TST!
                   )							; !TST!
                   (2 0 (1 2)))						; !TST!
                  (is (_pers1 (nonfirst safe temp)) john)		; !TST!
                  (is (_pers2 (nonfirst safe temp)) mary)		; !TST!
                 )							; !TST!
                 nil							; !TST!
                ) ; chunk						; !TST!
       )								; !TST!
       (fun1eva nil							; !TST!
                ((_pers1 (1 (1) nil))					; !TST!
                 (_pers2 (2 (2) (2)))					; !TST!
                )							; !TST!
                (chunk							; !TST!
                  ((usrlit						; !TST!
                     (likes						; !TST!
                      (_pers1 (first safe temp))			; !TST!
                      (_pers2 (first safe temp))			; !TST!
                     )							; !TST!
                     (2 0 (1))						; !TST!
                    )							; !TST!
                    (is (_pers1 (nonfirst safe temp)) john )		; !TST!
                    (usrlit (sex female					; !TST!
                                 (_pers2 (nonfirst safe temp)))		; !TST!
                            (2 0 (1))))					; !TST!
                     nil)						; !TST!
       )								; !TST!
       (fun*den nil							; !TST!
                ((_pers1 (1 (1) (1)))					; !TST!
                 (_pers2 (2 (2) nil))					; !TST!
                )							; !TST!
                (chunk							; !TST!
                 ((usrlit						; !TST!
                     (likes						; !TST!
                        (_pers1 (first safe temp))			; !TST!
                        (_pers2 (first safe temp))			; !TST!
                      )							; !TST!
                      (2 0 (2))						; !TST!
                  )							; !TST!
                  (usrlit						; !TST!
                     (likes						; !TST!
                       (_pers1 (nonfirst safe temp))			; !TST!
                       mary						; !TST!
                     )							; !TST!
                     (2 0 (2))						; !TST!
                  )							; !TST!
               ) ; usrlit						; !TST!
              nil ) ; chunk						; !TST!
              (chunk ((refl-xreg true)) nil )				; !TST!
       )								; !TST!
       (fun1den nil							; !TST!
                nil							; !TST!
                (chunk							; !TST!
                 ((usrlit (likes john mary)				; !TST!
                          (2 0 (1 2))					; !TST!
                  )							; !TST!
                  unknown 						; !TST!
                 )							; !TST!
                 nil )							; !TST!
       )								; !TST!
 )									; !TST!
) ; SETQ                              					; !TST!
      

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle:
;;;;;;  <classified_procedure> ::= (proc <procedure_name> <clause_count>
;;;;;;				         { <clause_classification> }+

;;;;;; used to see if it is a proc
(defmacro s-cg-proc-id (proc)
 `(car ,proc))
;(test (s-cg-proc-id inst-proc))						; !TST!

;;;;;; to get the <procedure_name>, actually an atom of name and arity
(defmacro s-cg-procedure_name (proc)
 `(cadr ,proc))
;(test (s-cg-procedure_name inst-proc))					; !TST!

;;;;;; to get clause_count
(defmacro s-cg-clause_count (proc)
 `(caddr ,proc))
;(test (s-cg-clause_count inst-proc))					; !TST!

;;;;;; to get the list of clause_classifications
(defmacro s-cg-clause_classifications (procedure)
 `(cddddr ,procedure))
;(test (s-cg-clause_classifications inst-proc))					; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-cc (first (s-cg-clause_classifications	; !TST!
; inst-proc)))))							; !TST!


;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle:
;;;;;;  clause_classification ::=( <clause_typ> <perm_var_list>
;;;;;;				   <temp_var_list> {<chunks>}+ )

;;;;;; to get <clause_typ>
(defmacro s-cg-clause_typ (classified_clause)
 `(car ,classified_clause))
;(test (s-cg-clause_typ inst-cc))					; !TST!

;;;;;; to get the <perm_var_list>
(defmacro s-cg-perm_var_list (classified_clause)
 `(cadr ,classified_clause))
;(test (s-cg-perm_var_list inst-cc))					; !TST!

;;;;;; to get the <temp_var_list>
(defmacro s-cg-temp_var_list (classified_clause)
 `(caddr ,classified_clause))
;(test (s-cg-temp_var_list inst-cc))					; !TST!

;;;;;; to get the { <literal_classification> }+
(defmacro s-cg-chunks (classified_clause)
 `(cdddr ,classified_clause))
;(test (s-cg-chunks inst-cc))						; !TST!


; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-chunk (first (s-cg-chunks		; !TST!
;		(first (s-cg-clause_classifications inst-proc)))))))		; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle:
;;;;;;  <head_chunk_fact> ::= (chunk (<head_literal> { <chunks_guard> }* )
;;;;;;                               <chunk_descr>)
;;;;;;  <head_chunk_rule> ::= (chunk (<head_literal> { <chunks_guard> }* 
;;;;;;                                               <first_premise_literal>)
;;;;;;                               chunk_descr)
;;;;;;  <body_chunk> ::=    (chunk ( {<chunk_guard>}* call_literal) chunk_descr)


(defmacro s-cg-chunk_id (chunk)
 `(car ,chunk))
;(test (s-cg-chunk_id inst-chunk))					; !TST!

(defmacro s-cg-chunk_descr (chunk)
 `(caddr ,chunk))
;(test (s-cg-chunk_descr inst-chunk))					; !TST!

(defmacro s-cg-chunk_lu_reg (chk_descr)
 `(car ,chk_descr))
;(test (s-cg-chunk_lu_reg (s-cg-chunk_descr inst-chunk)))		; !TST!

(defmacro s-cg-chunk_vpul (chk_descr)
 `(cadr ,chk_descr))
;(test (s-cg-chunk_vpul (s-cg-chunk_descr inst-chunk)))			; !TST!

(defmacro s-cg-chunk_head_literal (chunk)
 `(car (cadr ,chunk)))
;(test (s-cg-chunk_head_literal inst-chunk))				; !TST!

(defmacro s-cg-chunk_hd_cgfpl (chunk)
 `(cdr (cadr ,chunk)))
;(test (s-cg-chunk_hd_cgfpl inst-chunk))					; !TST!

(defmacro s-cg-chunk-bd_cgcl (chunk)
 `(cadr ,chunk))
;(test (s-cg-chunk-bd_cgcl inst-chunk))					; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-lc (s-cg-chunk_head_literal		; !TST!; 
;                       (first (s-cg-chunks				; !TST!
;			(first 						; !TST!
;                         (s-cg-clause_classifications inst-proc))))))))	; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle:
;;;;;;  <literal_classification> ::= ( usrlit
;;;;;;                                 ( <functor> <arglist_classification> ) 
;;;;;;                                                             ; abbrev: FAC
;;;;;;				        <literal_descr> )

(defmacro s-cg-usrlit_id ( literal_classification )
 `(car ,literal_classification ))
;(test (s-cg-usrlit_id inst-lc))						; !TST!

(defmacro s-cg-fac_list ( literal_classification )
 `(cadr ,literal_classification) )
;(test (s-cg-fac_list inst-lc))						; !TST!

(defmacro s-cg-literal_descr ( literal_classification )
 `(caddr ,literal_classification) )
;(test (s-cg-literal_descr inst-lc))					; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-ld (s-cg-literal_descr 	
;		     (s-cg-chunk_head_literal				; !TST!
;                       (first (s-cg-chunks				; !TST!
;			(first (s-cg-clause_classifications inst-proc)) ; !TST!
;                     )))))))						; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle:
;;;;;;  <literal_descr> ::= ( <arity> <env_size> <arg_seq> ) )
;;;;;;

(defmacro s-cg-arity (literal_descr)
 `(car ,literal_descr))
;(test (s-cg-arity inst-ld))						; !TST!

(defmacro s-cg-env_size (literal_descr)
 `(cadr ,literal_descr))
;(test (s-cg-env_size inst-ld))						; !TST!

(defmacro s-cg-arg_seq (literal_descr)
 `(caddr ,literal_descr))
;(test (s-cg-arg_seq inst-ld))						; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-fac (s-cg-fac_list 			; !TST!
;		     (s-cg-chunk_head_literal				; !TST!;              (first (s-cg-chunks				; !TST!
;			(first (s-cg-clause_classifications inst-proc)) ; !TST!
;		       )))))))						; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle:
;;;;;;  CG-internal definition FAC ::= (functor arglist_classification)

(defmacro s-cg-functor (fac)
 `(car ,fac))
;(test (s-cg-functor inst-fac))						; !TST!

(defmacro s-cg-arglist_classification (fac)
 `(cdr ,fac))
;(test (s-cg-arglist_classification inst-fac))				; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-arg1 (first (s-cg-arglist_classification	; !TST!
;		    (s-cg-fac_list 					; !TST!
;		     (s-cg-chunk_head_literal				; !TST!;                       (first (s-cg-chunks			       ; !TST!
;		      (first (s-cg-clause_classifications inst-proc))   ; !TST!
;		     )))))))))						; !TST!


;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle: <arglist_classification> ::=
;;;;;;				{ <constant_classification> |
;;;;;;				  <variable_classification> }
;;;;;;			     <variable> ::=
;;;;;;				(<variable> <local_var_descr>)
;;;;;;			     <constant_classification> ::=
;;;;;;				  constant_name 
;;;;;;  		     <local_var_descr> ::=
;;;;;;				(<occurence> <saveness> <var_class>)

;;;;;;  get the <local_var_descr> from <variable_classification>
(defmacro s-cg-local-var-descr (variable-classi)
 `(cadr ,variable-classi))
;(test (s-cg-local-var-descr inst-arg1))					; !TST!

;;;;;; get the <occurence> from the <variable_classification>
(defmacro s-cg-local_var_occurrence (var_classi)
	`(caadr ,var_classi))
;(test (s-cg-local_var_occurrence inst-arg1))				; !TST!

;;;;;; get the <saveness> from the <variable_classification>
(defmacro s-cg-local_var_saveness (var_classi)
	`(cadadr ,var_classi))
;(test (s-cg-local_var_saveness inst-arg1))				; !TST!

;;;;;; get the <var_class> of the <variable_classification>
(defmacro s-cg-local_var_class (var_classi)
	`(car (cddadr ,var_classi)))
;(test ( s-cg-local_var_class inst-arg1))				; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-perm_var_list_entry  (first		; !TST!
;  (s-cg-perm_var_list (first 						; !TST!
;	(s-cg-clause_classifications inst-proc)))))))			; !TST!
;(if *hgdebug* (test (setq inst-temp_var_list_entry (first		; !TST!
;  (s-cg-temp_var_list (first 						; !TST!
;		(s-cg-clause_classifications inst-proc)))))))		; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle: <perm_var_list> entry ::=
;;;;;; 				(<variable> <perm_descr>
;;;;;;       and  to handle: <temp_var_list> entry ::=
;;;;;; 				(<variable> <temp_descr>
;;;;;;

;;;;;; get the <variable> name of a <perm_var_list> entry
(defmacro s-cg-perm_var (perm_var_list_entry)
 `(car ,perm_var_list_entry))
;(test (s-cg-perm_var inst-perm_var_list_entry))				; !TST!

;;;;;; get <perm_descr> of an entry of <perm_var_list>
(defmacro s-cg-perm_descr (perm_var_list_entry)
 `(cadr ,perm_var_list_entry))
;(test (s-cg-perm_descr inst-perm_var_list_entry))			; !TST!

;;;;;; get the <variable> name of a <temp_var_list> entry
(defmacro s-cg-temp_var (temp_var_list_entry)
 `(car ,temp_var_list_entry))
;(test (s-cg-temp_var inst-temp_var_list_entry))				; !TST!

;;;;;; get <temp_descr> of an entry of <temp_var_list>
(defmacro s-cg-temp_descr (temp_var_list_entry)
 `(cadr ,temp_var_list_entry))
;(test (s-cg-temp_descr inst-temp_var_list_entry))			; !TST!

; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-perm_descr  (s-cg-perm_descr (first	; !TST!
;  (s-cg-perm_var_list (first 						; !TST!
;	(s-cg-clause_classifications inst-proc))))))))			; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle: <perm_descr> ::=
;;;;;;                                 (<Y-reg_nr> <use_head> <last_literal> )

;;;;;; get the <Y-reg_nr> from a <perm_descr>
(defmacro s-cg-perm_y_nr (perm_descr)
	`(car ,perm_descr))
;(test (s-cg-perm_y_nr inst-perm_descr))					; !TST!

;;;;;; get the <use_head> list of the <perm_descr> 
(defmacro s-cg-perm_use_head (perm_descr)
	`(cadr ,perm_descr))
;(test (s-cg-perm_use_head inst-perm_descr))				; !TST!

;;;;;; get the <last_literal> of the <perm_descr>
(defmacro s-cg-perm_last_literal (perm_descr)
	`(caddr ,perm_descr))
;(test (s-cg-perm_last_literal inst-perm_descr))				; !TST!


; Entrypoint for *hgdebug*						; !TST!
;(if *hgdebug* (test (setq inst-temp_descr  (s-cg-temp_descr (first	; !TST!
;  (s-cg-temp_var_list (first 						; !TST!
;	(s-cg-clause_classifications inst-proc))))))))			; !TST!
;(if *hgdebug* (test (setq inst-perm_descr  (s-cg-perm_descr (first	; !TST!
;  (s-cg-perm_var_list (first 						; !TST!
;	(s-cg-clause_classifications inst-proc))))))))			; !TST!

;;;;;;  ----------------------------------------------------------------------
;;;;;;  Selectors to handle: <temp_descr> ::=
;;;;;; 				(<X-reg_nr> <use_head> <use_premise>)

;;;;;; get the <X-reg_nr> of <temp_descr>
(defmacro s-cg-temp_x_nr (temp_descr)
	`(car ,temp_descr))
;(test (s-cg-temp_x_nr inst-temp_descr))					; !TST!	

;;;;;; get the <use_head> list of the <temp_descr>
(defmacro s-cg-temp_use_head (temp_descr)
	`(cadr ,temp_descr))
;(test (s-cg-temp_use_head inst-temp_descr))				; !TST!	

;;;;;; get <use_premise> of <temp_descr>
(defmacro s-cg-temp_use_premise (temp_descr)
	`(caddr ,temp_descr))
;(test (s-cg-temp_use_premise inst-temp_descr))				; !TST!	

;---------------------------end_of_selectors-----------------------------------

;;;;;;  ----------------------------------------------------------------------
;;;;;; Access functions which return the perm/temp - descr when a variable
;;;;;; is given to them as a <variable_classification> list.

; Input:  (Variable CLASSIFICATION ) Output: (Y-reg_nr last_literal use_head)
(defun get_perm_descr (arg_var perms)
	(s-cg-perm_descr (assoc (s-cg-perm_var arg_var) perms :test #'equal) ))
;(test (get_perm_descr '((vari c) (first unsafe perm))			; !TST!
;  (s-cg-perm_var_list (first 						; !TST!
;	(s-cg-clause_classifications inst-proc)))))			; !TST!

; Input:  (Variable CLASSIFICATION ) Output: (X-reg_nr use_head use_premise)
(defun get_temp_descr (arg_var temps)
	(s-cg-temp_descr (assoc (s-cg-temp_var arg_var) temps :test #'equal) ))
;(test (get_perm_descr '((vari e) (first unsafe temp))			; !TST!
;  (s-cg-temp_var_list 							; !TST!
;	(first (s-cg-clause_classifications inst-proc)))))		; !TST!


;;;;;;------------------------------------------------------------------------
;;;;;; Predicates to determine the type of an argument
;;;;;;

;;;;;; true if the argument is a NIL pointer
(defmacro arg-nil-p (arg) 
  `(null ,arg))

;;;;;; true iff the argument is a constant
(defmacro arg-const-p (arg)
  `(atom ,arg))

;;;;;; true iff the arugment is a variable
(defmacro arg-var-p (arg) 
	`(and 	(consp ,arg) (consp (car ,arg))
		(eq (car (car ,arg)) 'vari)
        )
)

;;;;;;
;;;;;;  Use this code if you handle the variables as _a and not as (vari a) !
;;;;;;
;	(defmacro arg-var-p (arg) 
;		(and 	(consp ,arg) (atom (car ,arg)) 
;			(equal (subseq (symbol-name (car ,arg)) 0 1) "_")
;	        )
;	)


;;;;;;------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; the arity is needed as a number, uses a long searchpath !        ; !TODO!
(defmacro s-cg-arity-of-proc (proc)
 `(s-cg-arity (s-cg-literal_descr 		
 	       (s-cg-chunk_head_literal	
		(first (s-cg-chunks
		 (first (s-cg-clause_classifications ,proc))))))))
;(test (s-cg-arity-of-proc inst-proc))					; !TST!

; true iff the argument is a LISP external call
(defmacro cg-lispcall-p (arg)
	`(and 	(consp ,arg) 
                (or (eq (car ,arg) 'cl-func)
                    (eq (car ,arg) 'cl-pred)
                    (eq (car ,arg) 'cl-extra)
                    (eq (car ,arg) 'cl-relf)
                )
		(consp (cadr ,arg))
        )
)

(defmacro cg-lispcall-fun (arg)
 `(caadr ,arg))

(defmacro cg-lispcall-args (arg)
 `(cdadr ,arg))

; true iff the argument is a structure
(defmacro cg-inst-p (arg)
	`(and 	(consp ,arg) (eq (car ,arg) 'inst)
		(consp (cadr ,arg))
        )
)

(defmacro cg-s-inst-functor (arg)
 `(caadr ,arg))

(defmacro cg-s-inst-funargs (arg)
 `(cdadr ,arg))

(defmacro secure (y)
 `(or (eq ,y 'safe) (eq ,y 'global)))

(defmacro firstreuse-p (x)
 `(or (eq ,x 'first) (eq ,x 'reuse))
)

(defmacro code-gen-put-structlist (actual reg)
`(if (eq (cg-s-inst-functor ,actual) 'cns)
     (g-put-list ,reg)
     (g-put-structure (cg-s-inst-functor ,actual)
                      (length (cg-s-inst-funargs ,actual))
                      ,reg
     )
 )
)

(defmacro code-gen-get-structlist (actual reg)
`(if (eq (cg-s-inst-functor ,actual) 'cns)
     (g-get-list ,reg)
     (g-get-structure (cg-s-inst-functor ,actual)
                      (length (cg-s-inst-funargs ,actual))
                      ,reg
     )
 )
)


;;;;;; doappend : LIST x LIST --> LIST
;;;;;; Alternative : nconc fuer Geschwindigkeitsfetischisten
(defmacro doappend (x y)
 `(append ,x ,y))

;;;;;; addcode : doappend and addcode are the only places where append is used.
;;;;;; for tuning this may be replaced by nconc
(defmacro addcode (codelist instructions)
 `(setq ,codelist (append ,codelist ,instructions)))

