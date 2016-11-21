;;; Changes ... the following functions are patched!


;;; classify ...

;;;                CLASSIFY-DB
;;;      Klassifizierung der Datenbasis
;;;
;;; Eingabe: <list*of*predicates> Liste der Praedikatname/Aritaet der DB
;;;
;;; Wert:    Klassifizierte Datenbasis:
;;;          (classified_proc1 ... classified_procN)
;;;
;;; Die Datenbasis befindet sich auf Property-Listen. Jede Prozedur, also
;;; Praedikatname/Aritaet, ist eine Property-Liste; unter dem Tag 'clause

; changed by MH ,16.04.91
;;; beziehungsweise 'forw-clause

;;; stehen die KLauseln einer Prozedur zu einer Liste zusammengefasst.
;;; Die Funktion CLASSIFY-DB klassifiziert nacheinander die Prozeduren, die 
;;; durch list*of*predicates gekennzeichnet sind.


(defun classify-db (list*of*predicates) ; Klassifizeirung  der Datenbank
  (cond ((null list*of*predicates) nil)
        (t (let ((lc*procedure*name (s-first  list*of*predicates)))
             (cons (classify-procedure (get  lc*procedure*name 'clause) lc*procedure*name)
                 (classify-db (s-rest list*of*predicates)))))))


(defun classify-forw-db (list*of*predicates) ; Klassifizeirung  der Datenbank, forw- Bereich
  (cond ((null list*of*predicates) nil)
        (t (let ((lc*procedure*name (s-first  list*of*predicates)))
             (cons (classify-procedure (get  lc*procedure*name 'forw-clause) lc*procedure*name)
                 (classify-forw-db (s-rest list*of*predicates)))))))


;;; the codegenerator 

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-PROC:
;;;;;; procedure: a classified_procedure 
;;;;;;			  see EBNF syntax for Classified .
;;;;;;				"Clause_classification"
;;;;;;
;;;;;; Output: List of instructions for the procedure.
;;;;;;         side-effect: put the propertylist under the tag: 'procedure


(defun code-gen-proc (procedure)
 (let* ( (id_proc (s-cg-proc-id procedure))
	 (no_of_clauses (s-cg-clause_count procedure))
	 (procedure_arity (s-cg-arity-of-proc procedure))
	 (list_of_clauses (s-cg-clause_classifications procedure))
	 (pred_arity (s-cg-procedure_name procedure))
	 )
	(if (not (eq id_proc 'proc)) (error "PROC id expected"))
	(if (eq no_of_clauses 1)
		(putniv  pred_arity 
			 'procedure
			 (code-gen-cc (first list_of_clauses)) )
	    	(putniv pred_arity 
			'procedure
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
					(setq lc*code (append lc*code (g-trust-me-else-fail 
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

;;;;;; ------------------------------------------------------------------------
;;;;;; CODE-GEN-FORW-PROC:
;;;;;; procedure: a classified_procedure 
;;;;;;			  see EBNF syntax for Classified .
;;;;;;				"Clause_classification"
;;;;;;
;;;;;; Output: List of instructions for the procedure.
;;;;;;         side-effect: put the propertylist under the tag: 'forw-procedure

(defun code-gen-forw-proc (procedure)
 (let* ( (id_proc (s-cg-proc-id procedure))
	 (no_of_clauses (s-cg-clause_count procedure))
	 (procedure_arity (s-cg-arity-of-proc procedure))
	 (list_of_clauses (s-cg-clause_classifications procedure))
	 (pred_arity (s-cg-procedure_name procedure))
	 )
	(if (not (eq id_proc 'proc)) (error "PROC id expected"))
	(if (eq no_of_clauses 1)
		(putniv  pred_arity 
			 'forw-procedure
			 (code-gen-cc (first list_of_clauses)) )
	    	(putniv pred_arity 
			'forw-procedure
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
					(setq lc*code (append lc*code (g-trust-me-else-fail 
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

;;;; misceangelous ... 

;;;;;;;;;; a patch of wandel to handle strings

(defun wandel (liste)
 (cond  ((null liste) nil)
        ((numberp liste) liste)
        ((atom liste) (if (stringp liste)
			  liste
			(cond ((equal (subseq (symbol-name liste) 0 1 ) "_")
			       `(vari
				 ,(intern (subseq (symbol-name liste) 1 ))
                                 )
			       )
			      (t liste))
			)
        )
        ( t (cons (wandel (car liste))(wandel (cdr liste)))  )
 )
)



(defun read-db (database)
" Reads a database into the hash-table ( put / get ) ..
  Append the names of the clauses at  *predicates* (hn or ft) and *forward-preds* (forw) "
 (do* ( (lc*restclauses database (cdr lc*restclauses))
;;; changed by MH 15.04.91  .. to get the forw- Clauses 
        (lc*type (caar lc*restclauses)(caar lc*restclauses))

        (lc*clause (if (member lc*type '(hn ft forw)) 
	                     (cdar lc*restclauses)
                             (car  lc*restclauses)
                   )
		    (if (member lc*type '(hn ft forw)) 
			     (cdar lc*restclauses)
                             (car  lc*restclauses)
		    )
        )
;;;;; 
	        
        ; HGH some work must be done , dirty hack !
	(lc*head (sel-head-n-a lc*clause)(sel-head-n-a lc*clause))
	(lc*pred (sel-predicate-n-a lc*head)(sel-predicate-n-a lc*head))
	(lc*length (1- (length lc*head)) (1- (length lc*head)))
	(lc*intern (p_a2p/a lc*pred lc*length)
                   (p_a2p/a lc*pred lc*length))
      )
      ( (null lc*restclauses) (list *forward-preds* *predicates*) )

    (cond ((member lc*type `(hn ft))
	   (if (member lc*intern *predicates*)
	       (putniv  lc*intern 'clause (append (get lc*intern 'clause) 
						  (list (wandel lc*clause))
						  )
			)
	     (progn (putniv  lc*intern 'clause (list (wandel lc*clause)) )
		    (setq *predicates* (cons lc*intern *predicates*))
		    )
	     ) ; if
	   ) ; case 1
	  ((member lc*type `(forw))
	   (if (member lc*intern *forward-preds*)
	       (putniv  lc*intern 'forw-clause (append (get lc*intern 'forw-clause)
						       (list (wandel lc*clause))
						       )
			)
	     (progn (putniv  lc*intern 'forw-clause (list (wandel lc*clause)) )
		    (setq *forward-preds* (cons lc*intern *forward-preds*))
		    )
	     ) ; if
	   ) ; case 2
	  ( t (error "Unknown type of clause " lc*type)))
   ) ; do*
) ; defun




(defun loco-forw (db)
  "Reads the horizoned-database (only forw-clauses).
returns the new compiled forw-clauses.
The indexing is included."


  (let ((b nil)
	(old-forward-preds *forward-preds*)
	(result nil))
    (setf *forward-preds* nil)
    (read-db db)
    (setq b (classify-forw-db *forward-preds*))
;;;;;;;;;    (mapcar #'code-gen-forw-proc  b)
;;;;;;;;      <------- indexing ------>
    (mapcar #'(lambda (x) (append
			   (mapcan2
			    #'(lambda (y)
				(icg.mk-header y (s-cg-arity-of-proc x)))
			    (iif.mk-tree x))
			   (code-gen-forw-proc x)))
	    b)
    (setf result *forward-preds*)
    (setf *forward-preds* (union old-forward-preds *forward-preds*))
    result
    )
  )


(defun forward2forw (forward-rule)
" transforms a clause :(HN (FORWARD <trigger> <conclusion>) <body>)
  into a forw-clause  :(forw (<trigger>) <body>) "
  (if (and (eq (car forward-rule) 'hn)
	   (eq (caadr forward-rule) 'forward))
      (append `(forw ,(second (cadr forward-rule)))      ;;; <triger>
	      (cddr forward-rule))
    nil))




;;;; gama   ... the WAM emulator
(defvar *gwam-silent* nil "If not nil, nothing is printed but the spyer")

;;; patched to be silent ...
(definstr has-failed () () :standard (gwam.has-failed
  (when (not *gwam-silent*)(gformat "unknown~%")) ; "No (more) solutions"
  (throw 'halt nil)))

;;;;  cl-pred is deleted. the original bug is removed
;;;;  printword is deleted. not in use.

(defun printmseg (loc1 loc2) ; patched to show the retain regs
  (gterpri)
  (do ((i loc1 (1+ i)))
      ((> i loc2))
    (printmloc i)
    (if (eql i (address (reg E)))    (gformat " <== E"))
    (if (eql i (address (reg B)))    (gformat " <== B"))
    (if (eql i (address (reg H)))    (gformat " <== H"))
    (if (eql i (address (reg HB)))   (gformat " <== HB"))
    (if (eql i (address (reg S)))    (gformat " <== S"))
    (if (eql i (address (reg RENV))) (gformat " <== RENV"))
    (if (eql i (address (reg Rtop))) (gformat " <== Rtop"))
    (if (eql i (address (reg AN)))   (gformat " <== AN"))
    (if (eql i (address (reg ON)))   (gformat " <== ON")))
  (gterpri))




	
(defun show-value-of-something ()   ;; patched to show the retain stack
  (declare (special *registers*))
  (gformat "~% Value of? ")
  (case (step-get-char)
    ((#\?)(value-help)
          (show-value-of-something))
    ((#\A #\a)
    (gformat "~% Number of argument registers: ")
     (dotimes (i (read))
       (gformat "~% A(~a) = ~a" (1+ i) (argument-reg (1+ i))))
     (gterpri))
    ((#\H #\h)
     (printmseg start-of-heap (address (reg H))))
    ((#\S #\s)
     (printmseg start-of-stack (address (reg A))))
    ((#\R #\r)
     (gterpri)
     (dolist (x *registers*)
       (gformat "   Reg ~a = ~a ~%" x (get x 'reg-value)))
     (gterpri))
    ((#\N #\n)
     (printmseg (- (address (reg RENV)) regs-in-rs-env) (max (address (reg Rtop)) (address (reg AN)))))   
    (t (gformat "~% Unknown input, type ? for help "))))

(defun value-help ()
  (gformat
"~%    All commands consist of one character.

     ?                  Output this Help-Menu.
     A,a                Output n (to be read) argument registers
                          A(0)..A(n-1).
     H,h                Output Heap.
     R,r                Output all registers exept argumentregisters.
     S,s                Output stack.
     N,n             Output Retain_Stack.
"))


;;; the structure of word ... 

(defmacro make-word (&key (tag :empty) (value nil))
  `(list ,tag ,value))

(defun word-p(word)
  (and (listp word)
       (= (length word) 2)
       (char= #\: (char (format nil "~s" (car word)) 0))))

(defmacro word-equal(w1 w2)
  `(equal ,w1 ,w2))