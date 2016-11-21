;-------------------------------------------------------------------------
;
; Forward top level in COLAB
;
;   Interpreter commands 
;   by Thomas Labisch
;
;   Compiler commands 
;   by Martin Harm
;
; September 1991
;
;-------------------------------------------------------------------------
(defvar *magic-rules* nil)
(defvar *magic-seeds* nil)
(defvar *forward-rules* nil)
(defvar *forward-preds* nil)
(defvar *derived-factbase* nil)
(defvar *fc-strategies* nil)
(defvar *fw-interpreter-mode* t)
(defvar *fw-spy* nil)
(defvar *backward-preds* nil)
(defvar *up-preds* nil)
(defvar *rl-preds* nil)
(defvar *automatic-mode* t)
(defvar *fw-prelude* nil)

;-------------------- to be used in connection with TAXON ----------------
(defvar *tx-concepts* nil  "All the concepts, defined in TAXON")
(defvar *tx-attributes* nil "All attributes, defined in TAXON, used with any concept")
;-------------------------------------------------------------------------

(defvar *default-rules-strategies* nil)
(setq *default-rules-strategies*
    (our-fs:logdir-file :fw-strategies "fc-strategies" :suffixl '("rf" nil)))

(setq *col-list-of-forward*
'(
  ;(?                  "" nil colab-questionmark        nil "Forward Help Utility")
  (a0                 "" nil colab-a0-forward          nil "Assert a clause at the beginning of the database")
  (a0fact             "" nil colab-a0fact-forward      nil "Assert a fact at the beginning of the factbase")
  (a0rl               "" nil colab-a0rl-forward        nil "Assert a rl-clause at the beginning of the database")
  (a0up               "" nil colab-a0up-forward        nil "Assert an up-clause at the beginning of the database")
  (automatic          "" nil colab-automatic-forward   nil "Activate automatic mode")
  (az                 "" nil colab-az-forward          nil "Assert a clause at the end of the database")
  (azfact             "" nil colab-azfact-forward      nil "Assert a fact at the end of the factbase")
  (azrl               "" nil colab-azrl-forward        nil "Assert a rl-clause at the end of the database")
  (azup               "" nil colab-azup-forward        nil "Assert an up-clause at the end of the database")
  (compile-strategies "" nil colab-fwcompilestrat      nil "Compile the actual strategies")
  ;(consult            "" nil colab-consult             nil "Consult the databases")
  (consult-facts      "" nil colab-consultf-forward    nil "Consult a file with facts")
  (consult-rules      "" nil colab-consultr-forward    nil "Consult a file with rules")
  (consult-strategies "" nil colab-consults-forward    nil "Consult new strategies")
  ;(destroy            "" nil colab-destroy             nil "Destroy the FORWARD rulebase and Facts")
  (destroy-rules      "" nil colab-destroyr-forward    nil "Destroy the rulebases")
  (destroy-facts      "" nil colab-destroyf-forward    nil "Destroy the factbase")
  (destroy-magic      "" nil colab-destroym-forward    nil "Destroy magic rules and seeds")
  (eval               "" nil colab-seminaive-forward   nil "Bottom-up-evaluation")
  (raete-eval         "" nil colab-fw-fam-forward      nil "Starts the FAM with the actuell Facts")
  (fw-compile         "" nil colab-fwcompile-forward   nil "Compile all databases")
  (fw-compile-raete   "" nil colab-fwcompile-raete     nil "Compile rules into FAM")
  (fw-compile-rules   "" nil colab-fwcompiler-forward  nil "Compile all rules")
  (fw-compile-facts   "" nil colab-fwcompilef-forward  nil "Compile facts")
  (fw-emul            "" nil colab-fwemul-forward      nil "Forward emulator mode")
  (fw-inter           "" nil colab-fwinter-forward     nil "Forward interpreter mode")
  (fw-transform       "" nil colab-fwtransform-forward nil "Horizontal transformation to forward clauses")
  ;(help               "" nil colab-cmd-help            nil "Forward help utility")
  ;(hornify            "" nil colab-hornify-forward     nil "Hornify all database")
  (hornify-up         "" nil colab-hornifyup-forward  nil "Hornify the up-database")
  ;(listing            "" nil colab-listing             nil "List the databases")
  (l                  "" nil colab-list-forward        nil "List the Forward databases")
  (list-facts         "" nil colab-lfacts-forward      nil "List factbase")
  (lf                 "" nil colab-lfacts-forward      nil "List factbase")
  (list-forward       "" nil colab-lforward-forward    nil "List forward-rules")
  (list-magic         "" nil colab-lmagic-forward      nil "List magic rules and facts")
  (lm                 "" nil colab-lmagic-forward      nil "List magic rules and seeds")
  (list-strategies    "" nil colab-lstrategies-forward nil "List the fc-strategies")
  (ls                 "" nil colab-lstrategies-forward nil "List the fc-strategies")
  (list-rules         "" nil colab-lrules-forward      nil "List the rules")
  (lr                 "" nil colab-lrules-forward      nil "List the rules")
  (magic-eval         "" nil colab-magiceval-forward   nil "Magic Sets evaluation")
  (magic-query        "" nil colab-magicsemi-forward   nil "Bottom-up for magic")
  (magic-transform    "" nil colab-magicsets-forward   nil "Magic Sets transformation")
  (noautomatic        "" nil colab-noautomatic-forward nil "Deactivate automatic mode")
  (nospy              "" nil colab-nospy-forward       nil "Unset trace mode")
  (q                  "" nil colab-rfquery-forward     nil "Shortcut of rf-query")
  ;(replace            "" nil colab-replace             nil "Replace the databases")
  (replace-facts      "" nil colab-replacef-forward    nil "Replace the factbase")
  (replace-rules      "" nil colab-replacer-forward    nil "Replace the rule database")
  (replace-strategies "" nil colab-replaces-forward    nil "Replace the strategies")
  (reset-strategies   "" nil colab-resets-forward      nil "Reset the fc-strategies")
  (rf-query           "" nil colab-rfquery-forward     nil "Query to the RELFUN subsystem")
  (rule-compile       "" nil colab-rule-compile        nil "Compile into the FAM (Magic,Semi)")
  (rx                 "" nil colab-rx-forward          nil "Remove a clause from the database")
  (rxfact             "" nil colab-rxfact-forward      nil "Remove a fact from the factbase")
  (rxrl               "" nil colab-rxrl-forward        nil "Remove a rl-clause from the database")
  (rxup               "" nil colab-rxup-forward        nil "Remove an up-clause from the database")
  (split-rules        "" nil colab-splitrules-forward  nil "Split rl-rules into hn- and up-rules")
  (spy                "" nil colab-spy-forward         nil "Set trace mode")
  ;("" nil  nil "")
  ;(df-enum    "" nil colab-dfenum-forward    nil "")
  ;(bf-enum    "" nil colab-bfenum-forward    nil "")
  ;(df-all     "" nil colab-dfall-forward     nil "")
  ;(bf-all     "" nil colab-bfall-forward     nil "")
  ))

(defun colab-rule-compile(userline)
  "Dummy fct."
  (declare (ignore userline))
  (format t "ERROR: Function not yet implemented"))

(defun forward-else (userline)
  "Error if no cmd is spezified"
  (format t "forward: unknown user command ~A" userline))

(defun colab-a0-forward (userline)
  (let* ((rule (car userline))
	 (tag (car rule)))
	(if (or (eq tag 'attrterm)
		(eq tag 'fact))
	    (setq *factbase* (cons (cons 'hn
					 (cdr rule))
				   *factbase*))
	    (treat-rule rule nil))))

(defun colab-a0fact-forward (userline)
  (setq *factbase* (cons (cons 'hn userline) *factbase*)))

(defun colab-a0rl-forward (userline)
  (treat-rule (cons 'rl userline) nil))

(defun colab-a0up-forward (userline)
  (treat-rule (cons 'up userline) nil))

(defun colab-az-forward (userline)
  (let* ((rule (car userline))
	 (tag (car rule)))
	(if (or (eq tag 'attrterm)
		(eq tag 'fact))
	    (setq *factbase* (append *factbase*
				     (list (cons 'hn
						 (cdr rule)))))
	    (treat-rule rule t))))

(defun colab-azrl-forward (userline)
  (treat-rule (cons 'rl userline) t))

(defun colab-azup-forward (userline)
  (treat-rule (cons 'up userline) t))

(defun colab-azfact-forward (userline)
   (setq *factbase* (append *factbase* (list (cons 'hn userline)))))

(defun treat-rule (rule az)
  (let ((tag (car rule)))
       (cond ((eq tag 'rl)
	      (update-list-and-base rule
				    '*rl-preds*
				    (append *backward-preds*
					    *up-preds*)
				    az))
	     ((eq tag 'up)
	      (update-list-and-base rule
				    '*up-preds*
				    (append *backward-preds*
					    *rl-preds*)
				    az))
	     ;((eq tag 'hn))
	     ;((eq tag 'ft))
	     (t 
	      (princ "Unexpected Knowledge-Item in Forward")))))

(defun update-list-and-base (rule list others az) ;;; changed by MH to handle multiple conclusions
  (let* (; (pred (caadr rule))
	 (concs (cadr rule))                 ;; inserted
	 (preds (if (atom (car concs))    
		    (list (car concs))       ;; only one conclusion
		  (mapcar #'car concs)))     ;; several conclusions
	 )
    (cond (
	   ;(member pred
           ;       others
           ;       :test #'equal)
	   (intersection preds
			 others
			     :test #'equal)
	   (format t "~A already used" preds))
	  (t
	   (if az
	       (setq *rule-database* (append *rule-database*
					     (list rule)))
	     (setq *rule-database* (cons rule
					 *rule-database*)))
	   (set list (union  preds             ;; changed
			     (eval list)
			     :test #'equal))))))

(defun colab-automatic-forward (userline)
  (declare (ignore userline))
  (setq *automatic-mode* t))

(defun colab-consult-forward (userline) 
  (colab-az-forward (list userline)))
#|
  (let ((pred (caadr userline)))
       (cond ((eq (car userline) 'up)
	      (update-list-and-base 
	      (cond ((member pred
			     (append *backward-preds*
				     *rl-preds*)
			     :test #'equal)
		     (format "~A already used" pred))
		    (t
		     (setq *rule-database* (append *rule-database*
						   (list userline)))
		     (setq *up-preds* (union (list pred)
					     *up-preds*
					     :test #'equal)))))
	     ((eq (car userline) 'rl)
	      (cond ((member pred
			     (append *backward-preds*
				     *up-preds*)
			     :test #'equal)
		     (format "~A already used" pred))
		    (t
		     (setq *rule-database* (append *rule-database*
						   (list userline)))
		     (setq *rl-preds* (union (list pred)
					     *rl-preds*
					     :test #'equal)))))
	     ((or (eq (car userline) 'fact)
		  (eq (car userline) 'attr-term))
              (cond ((member pred
                             (append *backward-preds*
                                     *up-preds*)
                             :test #'equal)
                     (format "~A already used" pred))
                    (t
		     (setq *factbase* (append *factbase*
					      (list (cons 'hn 
							  (rest userline)))))
		     (setq *backward-preds* (union (list pred)
						   *backward-preds*
						   :test #'equal)))))
	     (t 
	      (princ "Unexpected Knowledge-Item in Forward")
	      (terpri)))))

|#


(defun colab-consultf-forward (userline)
  (forward-cmd-consult userline '*factbase*))

(defun colab-consultr-forward (userline)
  (forward-cmd-consult userline '*rule-database*))

(defun colab-consults-forward (userline)
  (forward-cmd-consult userline '*fc-strategies*))

(defun forward-cmd-consult (userline db &rest rest)
  (let* ((filename1 (col-make-pathname (fw-extension (car userline) ".rf")))
	 (filename2 (col-make-pathname (fw-extension (car userline) ".col")))
	 (filename (if (probe-file filename1)
		       filename1
		     (if (probe-file filename2)
			 filename2
		       nil))))
    (if filename
	(let ((db1 (car rest)))
	  (set db 
	       (append (eval db)
		       (mapcar-not-nil (select1 db) 
				       (rfi-cmd-consult-1 filename))))
	  (if db1 
	      (set db1
		   (append (eval db1)
			   (mapcar-not-nil (select1 db1)
					   (rfi-cmd-consult-1 filename))))))
      (col-error "(forward-cmd-consult): " 
		 filename 
		 " file doesn't exist!"))))

(defun fw-extension (file extension
                      &aux (filename (cond ((stringp file) file)
                                           ((symbolp file)
                                            (string-downcase (string file)))
                                           ((pathnamep file)
                                            (namestring file))
                                           (t ""))))
  (let ((slash (position #\/ filename :from-end t))
        (point (position #\. filename :from-end t)))
    (cond ((string/= filename "")
           (cond ((or (and slash point (< slash point))
                      (and (null slash) point))
                  (concatenate 'string (subseq filename 0 point) extension))
                 ((or (and slash point (> slash point))
                      (null point))
                  (concatenate 'string filename extension))))
          (T (col-error "(fw-extension): "
                        (format nil "~A" file)
                        " filename must be of type string or symbol!")))))



(defun select1 (db)
  (cond ((eq db '*factbase*)
         #'select-facts)
        ((eq db '*rule-database*)
         #'select-rl-or-up)
        (t
         #'select-id)))

(defun select-facts (x)
  (if (or (eq (car x) 'fact)
          (eq (car x) 'attrterm))
      (cons 'hn (rest x))
      nil))

(defun select-rl-or-up (x)
  (if (or (eq (car x) 'rl)
          (eq (car x) 'up))
      x
      nil))

(defun select-id (x)
  x)

(defun fw-select-clauses(userline db)
  "Returns the matching clauses of db with userline.
Userline should contain:
 - nothing --> db
 - predicate names eg. (proc1 proc2) --> all clauses with proc_i in the head
 - tags in a list eg. ((hn) (rl))    --> all clauses of one type
"
  (cond ((null (car userline)) db)
	((atom (car userline)) ;; a predicate name
	 (union (mapcar-not-nil #'(lambda(r)(if (member (car userline) (pred_rule r))
						r
					      nil))
				db)
		(if (null (cdr userline))
		    nil
		  (fw-select-clauses (cdr userline) db))
		:test #'equal))
	((and (atom (caar userline))
	      (= (length (car userline)) 1)
	      (member (caar userline) '(hn ft rl up)))  ;; a tag in a list, no doubt about it!
	 (union (mapcar-not-nil #'(lambda(r)(if (eq (car r) (caar userline))
						r
					      nil))
				db)
		(if (null (cdr userline))
		    nil
		  (fw-select-clauses (cdr userline) db))
		:test #'equal))
  	(t nil)))  ;; return nothing on shit
	 

(defun colab-destroy-forward (userline)
  ;(destroy-forward-wam)
  ;(destroy-facts-wam)
  (colab-destroyr-forward userline)
  (colab-destroyf-forward userline)
  (setq *backward-preds* nil))

; Diese Funktion loescht nur die Regeln und laesst die Fakten.
; Sie erleichtert das Entwickeln von Regelbasen, weil sie in hybriden
; Wissensbanken das zeitaufwendige Einlesen der Fakten nach TAXON
; vermeidet, wenn man nur Fehler in den Regeln vermeiden will
(defun colab-destroyr-forward (userline)
  ;(destroy-forward-wam)
  ;(destroy-facts-wam)
  (setq *up-rulebase* nil)
  (setq *rule-database* nil)
  (setq *hn-rulebase* nil)
  (setq *forward-rules* nil)
  (setq *up-preds* nil)
  (setq *rl-preds* nil)
  (colab-destroym-forward userline))


(defun colab-destroyf-forward (userline)
  (declare (ignore userline))
  ;(forget-procedure-use userline)
  (setq *factbase* nil)
  (setq *derived-factbase* nil))

(defun colab-destroym-forward (userline)
  (declare (ignore userline))
  (remprops)
  (setq *magic-seeds* nil)
  (setq *magic-rules* nil))

(defun colab-fwcompile-forward (userline)
 "Splits *rule-database*.
Adds the 'add-data/data' concepts and than forward-transforms *up-rulebase* 
into *forward-rules*.
Compiles-forward *forward-rules*.
Compiles *factbase* + ('add-data/data' --> *hn-rulebase*)"
 
  (setf *tx-concepts* (tx 'get-concept-names))
  
  (split-rulebase (fw-select-clauses userline *rule-database*))
  (setf *forward-rules* (fw-transform-db (impl2expl_db *up-rulebase* *tx-concepts*)))
  (compile-forw-db *forward-rules*)
  (compile-db (union *factbase* (impl2expl_db *hn-rulebase* *tx-concepts*) :test #'equal)))

(defun colab-fwcompilef-forward (userline)
"Compiles only the *factbase*"
  (compile-db (fw-select-clauses userline *factbase*)))

(defun colab-fwcompiler-forward (userline)
 "Splits *rule-database* and forward-transforms *up-rulebase* 
into *forward-rules*.
Compiles-forward *forward-rules*.
Compiles *hn-rulebase*"
 
  (setf *tx-concepts* (tx 'get-concept-names))
  
  (split-rulebase (fw-select-clauses userline *rule-database*))
  (setf *forward-rules* (fw-transform-db *up-rulebase*))
  (compile-forw-db *forward-rules*)
  (compile-db  (impl2expl_db *hn-rulebase* *tx-concepts*)))

(defun colab-fwcompilestrat (userline)
  "Compiles the database in *fc-strategies*"
  (declare (ignore userline))
  (compile-db *fc-strategies*))

(defun colab-fwemul-forward (userline)
  (declare (ignore userline))
  (setq *fw-interpreter-mode* nil))

(defun colab-fwinter-forward (userline)
  (declare (ignore userline))
  (setq *fw-interpreter-mode* t))

(defun colab-fwtransform-forward (userline) 
  (declare (ignore userline))
  (setf *tx-concepts* (tx 'get-concept-names))
  (or *up-rulebase*
      (colab-splitrules-forward nil))
  (setq *forward-rules* (fw-transform-db (impl2expl_db *up-rulebase* *tx-concepts*))))

(defun colab-hornifyup-forward(userline)
  "Hornifys *up-rulebase*"
  (declare (ignore userline))
  (setf *up-rulebase* (hornify-rulebase *up-rulebase* nil)))

(defun colab-splitrules-forward (userline)
  "Splits *rule-database*"
  (split-rulebase (fw-select-clauses userline *rule-database*)))       

(defun colab-help-forward (userline)
  (let ((*print-case* :downcase))
       (cond ((null userline)
              (mapcar #'(lambda (x)
                          (format t "~A ~20T :  ~A ~%" (car x) (sixth x)))
                      *col-list-of-forward*))
       (t
        (let ((x (assoc (car userline) *col-list-of-forward* :test #'equal)))
       (cond ((null x)
        (print "Invalid Command in Forward!")
        (terpri))
       (t
        (format t "~A ~20T :  ~A ~%" (car x) (sixth x)))))))))


(defun colab-list-forward (userline)
  (colab-lfacts-forward userline)
  (colab-lrules-forward userline))

(defun colab-listfw-forward (userline)
  (forward-cmd-l userline *up-rulebase*))

(defun colab-lrules-forward (userline)
  (terpri)
  (princ "Rules:")
  (terpri)
  (forward-cmd-l userline *rule-database*))

(defun colab-lfacts-forward (userline)
  (terpri)
  (princ "Entire Facts:")
  (terpri)
  (forward-cmd-l userline *factbase*)
  (terpri)
  (princ "Derived Facts:")
  (terpri)
  (forward-cmd-l userline *derived-factbase*))

(defun colab-lforward-forward (userline)
  (forward-cmd-l userline *forward-rules*))

(defun colab-lmagic-forward (userline)
  (forward-cmd-l userline (append *magic-seeds* *magic-rules*)))

(defun colab-lstrategies-forward (userline)
  (forward-cmd-l userline *fc-strategies*))

(defun forward-cmd-l (x db)
  (cond ((null x) (rf-pprint-db db))
        ((atom x) (rfi-cmd-l-1 (mk-pairpattern x 'id) db))
        (t (rfi-cmd-l-1 x db))))

(defun colab-magiceval-forward (userline)
  (if (null *magic-rules*) (remprops))
  (forward-cmd-magiceval userline))

(defun colab-magicsemi-forward (userline)
  (forward-cmd-magicsemi userline))

(defun colab-magicsets-forward (userline) 
  (if (null *magic-rules*) (remprops))
  (forward-cmd-magictrans userline))

(defun colab-noautomatic-forward (userline)
  (declare (ignore userline))
  (setq *automatic-mode* nil))

(defun colab-nospy-forward (userline)
  (declare (ignore userline))
  (setq *fw-spy* nil)
;  (setq *rfi-spy* nil)
;  (setq *emu-debug* nil)
  )

(defun colab-replacef-forward (userline)
  (colab-destroyf-forward userline)
  (forward-cmd-consult userline '*factbase*))

(defun colab-replacer-forward (userline)
  (colab-destroyr-forward userline)
  (forward-cmd-consult userline '*rule-database*))

(defun colab-replaces-forward (userline)
  (setq *fc-strategies* nil)
  (if (null userline)
      (forward-cmd-consult (list *default-rules-strategies*) '*fc-strategies*)
      (forward-cmd-consult userline '*fc-strategies*)))

;(defun colab-replaceu-forward (userline)
;  (setq *fw-database* nil)
;  (forward-cmd-consult userline '*fw-database*))

(defun colab-resets-forward (userline)
  (declare (ignore userline))
  (colab-replaces-forward nil))
  
(defun rl2hn (predicate db)
  (mapcar-not-nil #'(lambda (x)
                     (if (null predicate)
                         (cons 'hn (cdr x))
                         (if (eq (first (second x)) predicate)
                             (cons 'hn (cdr x)))))
                  db))



(defun colab-rx-forward (userline)
  (if *forward-rules*
      (setq *forward-rules* 
	    (rxfw (generate-forward (car userline)) *forward-rules*)))
  (rx (car userline)))             
  
(defun colab-rxfact-forward (userline)
  (rx (cons 'fact userline))) 

(defun colab-rxrl-forward (userline)
  (if *forward-rules*
      (setq *forward-rules* 
	    (rxfw (generate-forward (cons 'rl userline)) *forward-rules*)))
  (rx (cons 'rl userline)))        
  
(defun colab-rxup-forward (userline)
  (if *forward-rules*
      (setq *forward-rules* 
	    (rxfw (generate-forward (cons 'up userline)) *forward-rules*)))
  (rx (cons 'up userline))) 
  
(defun rxfw (rules db)
  (cond ((null rules) db)
  (t
   (rxfw (cdr rules)
	 (remove (car rules) db :test #'equal)))))

(defun rx (rule)             
  (cond ((fact-p rule)
	 (setq *factbase* (remove (cons 'hn (cdr rule))
				  *factbase*
				  :test #'equal)))
	(t (if *up-rulebase* 
	       (rx-base-and-lists rule 
				  '*up-rulebase* 
				  '*up-preds*
				  'up))
	   (if *rule-database* 
	       (rx-base-and-lists rule 
				  '*rule-database* 
				  '*rl-preds*
				  'rl))
	   (if *hn-rulebase* 
	       (rx-base-and-lists rule 
				  '*hn-rulebase* 
				  '*backward-preds*
				  'hn)))))

(defun fact-p (rule)
  (or (eq (car rule) 'fact)
      (eq (car rule) 'attrterm)))

(defun rx-base-and-lists(rule base lists tag)    
  "Removes <rule> from <base> and <lists>"
  (set base (if (eq tag 'hn)
		(set-difference (eval base)
				(hornify-rule rule 'hn)
				:test #'equal)
		(remove (cons tag (cdr rule))
			(eval base)
			:test #'equal)))
  (let ((all-preds (append-list-elements (mapcar #'pred_rule
						 (eval base))))
	(rule-preds (pred_rule rule)))
    (mapc #'(lambda(pred)
	      (if (not (member pred
			       all-preds
			       :test #'equal)) 
		  (set lists (remove pred
				     (eval lists)
				     :test #'equal))))
	  rule-preds)))


(defun colab-seminaive-forward (userline)
  (declare (ignore userline))
  (forward-cmd-seminaive))

;(defun colab-dfall-forward (userline))
;(defun colab-bfall-forward (userline))



(defun colab-spy-forward (userline)
  (declare (ignore userline))
  (setq *fw-spy* t)
  ;(setq *rfi-spy* t)
  ;(setq *emu-debug* t)
  )

(defun colab-rfquery-forward (userline)
  (if *automatic-mode*
      (progn (or *forward-rules*
		 (colab-fwtransform-forward nil))
	     (or *hn-rulebase*
		 (colab-splitrules-forward nil))))
  (setf userline (impl2expl_prem userline *tx-concepts*))
  (cond (*fw-interpreter-mode*
         ;;;
         ;;; use interpreter to answer query
         ;;; 
         (cond ((and-process (deanon-request 
                              (if *rfi-static* 
                                  (flatten-request userline) 
                                  userline)) 
                             '((bottom)) 
                             (list *fw-prelude*
                                   *factbase* 
                                   *forward-rules* 
                                   *fc-strategies* 
                                   *hn-rulebase*
                                   *rfi-database*) 
                             1 
                             nil)) 
                (t 
                 (rf-print '|unknown|)))) 
         ;;; 
         ;;; use emulator to answer query 
         ;;; 
        (t 
;;;;;;;;;;;;;;;(init-retain-system)
         (emulate (transform-query-for-emulator userline)))) 
  nil)

(defun mapcar-not-nil (fun l)
"A useful function, which removes nil-results as the return-values from the
 function fun, which will be called by mapcar using an element of the list l"
    (remove nil (mapcar fun l)))

(if (null *fc-strategies*)
    (forward-cmd-consult (list *default-rules-strategies*) '*fc-strategies*))

(if (null *fw-prelude*)
    (forward-cmd-consult (list 
			  (our-fs:logdir-file :fw-strategies "forward-prelude"
					      :suffixl '("rf" nil)))
			 '*fw-prelude*))

(defun set-bps (db)
  (and db
       (let ((res (set-bps (cdr db))))
            (union (list (first (second (car db))))
                   res :test #'equal))))

(defun backward-pred-p (pred)
  (member pred *backward-preds* :test #'equal))

(setq *backward-preds* (set-bps (append *fw-prelude* *rfi-database*)))

#|
;;;;;;;;;;;  --------------------------------- destroy -----------------------------------------
(defun forget-forw-procedure ( name/a)
"Removes the forw-procedure from the code of the WAM. "

  (cond ((member name/a *fw-names-rule-database*)
   (setf *fw-names-rule-database* (delete name/a *fw-names-rule-database*))
   (putniv name/a 'forw-procedure nil)
   (putniv name/a 'forw-clause nil)
   )
  ((member name/a *fw-names-fw-database*)
   (setf *fw-names-fw-database* (delete name/a *fw-names-fw-database*))
   (putniv name/a 'forw-procedure nil)
   (putniv name/a 'forw-clause nil)
   ))
  )
     


(defun forget-procedure (name/a)
"Removes the procedure from the code of the WAM. "

  (cond ((member name/a *names-factbase*)
   (setf *names-factbase* (delete name/a *names-factbase*))
   (putniv name/a 'procedure nil)
   (putniv name/a 'clause nil)
   )
  ((member name/a *names-rule-database*)
   (setf *names-rule-database* (delete name/a *names-rule-database*))
   (putniv name/a 'procedure nil)
   (putniv name/a 'clause nil)
   )
  ((member name/a *names-rfi-database*)
   (setf *names-rfi-database* (delete name/a *names-rfi-database*))
   (putniv name/a 'procedure nil)
   (putniv name/a 'clause nil)
   ))
  )
  

(defun destroy-facts-wam ()
  "Destroy the whole factbase *names-factbase* in the WAM"
  
  (dolist (xx  *names-factbase* nil)
    (putniv xx 'procedure nil)
    (putniv xx 'clause nil))
  (setf *names-factbase* nil))

(defun destroy-forward-wam ()
  "Destroy the whole forwardrules *fw-names-rule-database*, 
*fw-names-fw-database* in the WAM"
  (dolist (xx  *fw-names-rule-database* nil)
    (putniv xx 'forw-procedure nil)
    (putniv xx 'forw-clause nil))
  (setf *fw-names-rule-database* nil)
  (dolist (xx  *fw-names-fw-database* nil)
    (putniv xx 'forw-procedure nil)
    (putniv xx 'forw-clause nil))
  (setf *fw-names-rule-database* nil))


(defun destroy-rules-wam ()
  "Destroy the whole relfun rules WAM code ( *names-rfi-database* )"
  (dolist (xx  *names-rfi-database* nil)
    (putniv xx 'procedure nil)
    (putniv xx 'clause nil))
  (setf *names-rfi-database* nil))
  

|#

;;;;;;;;;;; ---------------------------------- compile ------------------------------------------  
(defun compile-forw-db (db)
  "Horizons, compiles and assembles a transformed db:
see make-forward-rule, gwam.assemble-fw-db"
    (gwam.assemble-fw-db
     (loco-forw (horizon-database (mapcar-not-nil #'forward2forw db))))
)

(defun compile-db (db)
  "Horizons,compiles and assembles db,
see loco. (global variable *predicates*"
  (loco (horizon-database db))
  (gassem (mapcan #'gwam.get-asm-code *predicates*)))
  
  
;;; the gama stuff 
(defun gwam.assemble-fw-db (list-of-preds) ; list-of-preds  is a list containg all procedure names
					   ; (of the form name/arity)
  (gassem (mapcan #'gwam.get-fw-asm-code list-of-preds)))

(defun gwam.get-fw-asm-code (name/arity)
  (append
   (list '.proc 
         '(.module forward-code)
	 name/arity
	 '(.module user))
   (copy-list (get name/arity 'forw-procedure)))) 


;;;; the fam

(defun colab-fwcompile-raete(userline)
  "Splits *rule-database* into *up-rulebase* 
Compiles-forward *up-rulebase*."
  (split-rulebase (fw-select-clauses userline *rule-database*))
  (setf *fam-backward-index* 0)
  (setf *fam-backward-rules* nil)
  (compile-forward-fam (mapcar #'(lambda (x)(colab2fam_rule x *backward-preds*))
			       *up-rulebase*))
  (compile-db *fam-backward-rules* )
  )
		       

(defun colab-fw-fam-forward(userline)
  "Simply starts the reate with *factbase*"
  (fa-forward (mapcar #'colab2fam_fact *factbase*) :trace t :dump t))



