
;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-PROC:
;;;;;; procedure: a classified_procedure
;;;;;;                    see EBNF syntax for Classified Datafun.
;;;;;;                          "clause_classification"
;;;;;;
;;;;;; Output: List of instructions for the procedure.
;;;;;;         side-effect: put the propertylist under the tag: 'procedure


(defvar *idxlocal*)

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

