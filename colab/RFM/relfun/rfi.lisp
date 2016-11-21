;;; file:         "rfi.lsp"

;;; last update:  see variable *rfi-version*

;;; R E L A T I O N A L / F U N C T I O N A L   I N T E R P R E T E R   (RFI)
;;;
;;;                            Harold Boley
;;;
;;;                    University of Kaiserslautern


;;; Thanks for contributions to Klaus Elsbernd, Hans-Guenther Hein,
;;; Michael Herfert, Knut Hinkelmann, Thomas Labisch, Ralph Scheubrein,
;;; Michael Sintek, Werner Stein, and Stefan Steinacker.



;;; Copyright Notice
;;;
;;; This software is distributed for non-profit and research purposes only.
;;; I retain the exclusive right of producing a commercial version from it.
;;; Non-profit redistribution of the current version or parts of the
;;; current version is permitted if this copyright notice is included
;;; unchanged.  I give no warranty of any kind for this prototype. It will be
;;; further improved as time permits.
;;;
;;; Copyright (c) 1985-1992 by Harold Boley




; -----------------------------------------------------------------------------
;
; defining the global variables
;
; -----------------------------------------------------------------------------
(defvar *rfi-version*         "4 December 1992" "date of this version")

(defvar *rfi-commands*        nil  "list with valid interpreter-commands  ")
(defvar *rfi-prelude*         nil  "predefined programs                   ")
(defvar *rfi-database*        nil  "user programs                         ")
(defvar *rfi-input-mode*      nil  "selects input mode                    ")
(defvar *rfi-machine*         nil  "specifies interpreter or emulator mode")
(defvar *rfi-ori*             nil  "stores last user-query                ")
(defvar *rf-print-width*      nil  "width of the screen                   ")
(defvar *style*               'lisp "dom = {lisp, prolog}")
(defvar *rfi-prompt*          nil  "prompt-sign                           ")
(defvar *rfi-readtable*       nil  "readtable for special symbols         ")
(defvar *rfi-script-input*    nil  "input stream for typescript           ")
(defvar *rfi-script-output*   nil  "output stream for typescript          ")
(defvar *rfi-showdepth*       nil  "length of the trace                   ")
(defvar *rfi-spy*             nil  "true if programflow is traced         ")
(defvar *rfi-standard-output* nil  "output stream                         ")
(defvar *rfi-standard-input*  nil  "input stream                          ")
(defvar *rfi-static*          nil  "true if database is flattened         ")

(defvar *emu-debug*           nil  "flag for emulator-debugger            ")
(defvar *lisp-functions*      nil  "allowed functions                     ")
(defvar *lisp-predicates*     nil  "allowed predicates                    ")
(defvar *lisp-extras*         nil  "allowed extra-logicals                ")
(defvar *relfun-extras*       nil  "allowed relfun-extensions             ");(Klaus 30.09.1990)
(defvar *rfi-help-dir*        #+Mac "document:manual:interpreter:"
			      #-Mac "document/manual/interpreter/"
			      "help directory for help texts")





(defun rfi-set-interactive-mode ()
  (setq *rfi-input-mode* 'interactive))

(defun rfi-interactive-mode-p ()
  (eq *rfi-input-mode* 'interactive))

(defun rfi-set-batch-mode ()
  (setq *rfi-input-mode* 'batch))

(defun rfi-batch-mode-p ()
  (eq *rfi-input-mode* 'batch))



(defun rfi-set-emulator-mode ()
  (setq *rfi-machine* 'emulator)
  (rfi-cmd-style (list 'style *style*)))


(defun rfi-set-interpreter-mode ()
  (setq *rfi-machine* 'interpreter)
  (rfi-cmd-style (list 'style *style*)))



(defun rfi-interpreter-mode-p ()
  (eq 'interpreter *rfi-machine*))



(defun rfi-script-mode-p ()
  (not (null *rfi-script-output*)))



; 
; set defaults
;

(defun rfi-init ()
  (if (streamp *rfi-script-input*) (close *rfi-script-input*))
  (if (streamp *rfi-script-output*) (close *rfi-script-output*))
  (setq *rfi-commands* 
	'(a0 a0ft a0hn az azft azhn builtins compile consult
	     destroy dynamic emul endscript 
	     exec extrarg deanon flatten flatter footen footer 
	     help horizon inter l 
	     lconsult bye listing listclass listcode lreplace 
	     ltell m modes more nospy normalize ori 
	     prelude relationalize rx rxft rxhn replace
	     script showdepth singlify spy 
	     static style tell untup version verti
	     fc-init break ;(Klaus 30.09.1990)
             indexing    ; INDEXING -- real-fun
	     asm assem untype uncomma hitrans ; Michael Sintek 18.02.92
	     unor ; M.S. 28.02.92
	     unlambda ; M.S. 30.04.92
	     unmacro ; M.S. 18.05.92
             ))
  
  (setq *rf-print-width*      80)
  (setq *rfi-standard-output* *standard-output*)
  (setq *rfi-standard-input*  *standard-input*)
  (setq *rfi-script-output*   nil)
  (setq *rfi-script-input*    nil)
  (rfi-set-interactive-mode)
  (if (not *rfi-readtable*)
      (progn (setq *rfi-readtable* (copy-readtable))
	     (rfi-set-syntax)))
  (if (not *lisp-functions*)
      (setq *lisp-functions*  
	    '(+ - * / 1+ 1- abs rem round sqrt expt log sin cos tan
		asin acos atan max min first rest last length time gensym
                princ-to-string date operators
		fw cn tx 
		)))
  (if (not *lisp-predicates*)
      (setq *lisp-predicates* 
	    '(< <= = /= > >= string< string<= string= string/= 
		string> string>= null atom symbolp numberp 
		integerp plusp minusp
		))) 
  (if (not *relfun-extras*)
      (setq *relfun-extras* ;(Klaus 30.09.1990)
	    '(atom-e not-atom-e
		     vari-t-e not-vari-t-e
		     )))
  (if (not *lisp-extras*)
      (setq *lisp-extras*  
	    '(load break readl relfun rf-print rf-princ rf-terpri rf-fresh-line
		   rf-pprint rf-format pretty-print wait)))
  (if (not *rfi-ori*)
      (setq *rfi-ori* '((is t nil))))
  (if (not *rfi-showdepth*)
      (setq *rfi-showdepth* 0))
  (if (not *rfi-machine*)
      (rfi-set-interpreter-mode))
  (if (not *rfi-prelude*)
      (rfi-load-prelude))
  )






; -----------------------------------------------------------------------------
;
; main-functions of the interpreter
;
; -----------------------------------------------------------------------------
(defun relfun (&optional *rfi-script-input* (*rfi-input-mode* 'interactive))
 ; (rfi-init) ; not allowed because of recursive relfun-calls
 ; in kcl *standard-input* is redirected while loading init.lsp:
 (setq *rfi-standard-input* *standard-input*)
 (rf-terpri)
 (do* ((leave-relfun nil)
       (userline (readl)))
      (leave-relfun)
  (setq userline           ; execute command/query and bind new input from user
   (catch :toploop
    (cond ((rfi-command-p userline)
           (setq leave-relfun (rfi-command userline))
           (cond (leave-relfun nil)
                 ((eq (car userline) 'ori) *rfi-ori*)
                 (t (readl))))
          (t
	   ;;;
	   ;;; query
	   ;;;
	   (setq *rfi-ori* userline)
	   (if (rfi-interpreter-mode-p)
                     ;;;
                     ;;; use interpreter to answer query
                     ;;; 
	       (cond ((and-process (deanon-request (if *rfi-static* (flatten-request userline) userline))
				   '((bottom)) 
				   (list *rfi-prelude* *rfi-database*)
				   1
				   nil))
		     (t
		      (rf-print '|unknown|)
		      (readl)))
                     ;;;
                     ;;; use emulator to answer query
                     ;;; 
	       (progn (emulate (transform-query-for-emulator userline))
		      (readl)))
	   )
    ))))
 (if (streamp *rfi-script-input*) (close *rfi-script-input*))
)


(defun rfi-command-p (userline)
  (member (car userline) *rfi-commands* :test #'equal)
)

(defun rfi-command (userline)
  (let ((com (car userline)))
    (cond ((eq com 'a0)
	   (setq *rfi-database*
		 (append (cdr userline) *rfi-database*)))
	  ((eq com 'a0ft)
	   (setq *rfi-database*
		 (cons (cons 'ft (cdr userline)) *rfi-database*)))
	  ((eq com 'a0hn)
	   (setq *rfi-database*
		 (cons (cons 'hn (cdr userline)) *rfi-database*)))
	  ((eq com 'az)
	   (setq *rfi-database*
		 (append *rfi-database* (cdr userline))))
	  ((eq com 'azft)
	   (setq *rfi-database*
		 (append *rfi-database* (list (cons 'ft (cdr userline))))))
	  ((eq com 'azhn)
	   (setq *rfi-database*
		 (append *rfi-database* (list (cons 'hn (cdr userline))))))
	  ((eq com 'version)
	   (princ *rfi-version*))
	  ((eq com 'builtins)
	   (rf-print (cons 'functions *lisp-functions*))
	   (rf-print (cons 'predicates *lisp-predicates*))
	   (rf-print (cons 'extras *lisp-extras*)))
	  ((eq com 'modes)
	   (rfi-modes userline))
					; horizontal database transformation. (TK 07.10.1990)
					; the function NORMALIZE-DATABASE has to be imported from the file NORMALIZER.LISP
	  ((eq com 'normalize)
	   (setq *rfi-database* (normalize-database *rfi-database*)))
	  ((eq com 'horizon)
	   (setq *rfi-database* (horizon-database *rfi-database*)))
	  ((eq com 'verti)
	   (rfi-cmd-compile userline) (gwam.assem))
	  ((eq com 'compile)
	   (setq *rfi-database* (horizon-database *rfi-database*))
	   (rfi-cmd-compile userline) (gwam.assem)) 
					;userline still ignored in assembling!!!!!
	  ((eq com 'consult)
	   (rfi-cmd-consult userline))
	  ((eq com 'destroy)
	   (setq *rfi-database* nil))
	  ((eq com 'dynamic)
	   (setq *rfi-static* nil))
	  ((eq com 'emul)
	   (rfi-cmd-emul))
	  ;;; command endscript defined in function readl
	  ((eq com 'exec)
	   (rfi-cmd-execute userline))
	  ((eq com 'deanon)
	   (setq *rfi-database* 
		 (deanon-database *rfi-database*)))
	  ((eq com 'flatten)
	   (setq *rfi-database*
		 (flatten-database *rfi-database*)))
	  ((eq com 'flatter)
	   (setq *rfi-database*
		 (flatten-struc-database *rfi-database*)))
	  ((eq com 'extrarg)
	   (setq *rfi-database*
		 (extrarg-database *rfi-database*)))
	  ((eq com 'relationalize)
	   (setq *rfi-database*
		 (extrarg-database (flatten-database *rfi-database*))))
	  ((eq com 'singlify)
	   (setq *rfi-database*
		 (singlify-database *rfi-database*)))
	  ((eq com 'footen)
	   (setq *rfi-database*
		 (footen-database *rfi-database*)))
	  ((eq com 'footer)
	   (setq *rfi-database*
		 (footer-database *rfi-database*)))
	  ((eq com 'help)
	   (rfi-cmd-help userline))
	  ((eq com 'inter)
	   (rfi-cmd-inter))
	  ((eq com 'prelude)
	   (rfi-cmd-l (cadr userline) *rfi-prelude*))
	  ((or (eq com 'l) (eq com 'listing))
	   (rfi-cmd-l (cadr userline) *rfi-database*))
	  ((eq com 'lconsult)
	   (rfi-cmd-lconsult userline))
	  ((eq com 'listclass)
	   (rfi-cmd-listclass userline))
	  ((eq com 'listcode)
	   (rfi-cmd-listcode userline))
	  ((eq com 'lreplace)
	   (rfi-cmd-lreplace userline))
	  ((eq com 'ltell)
	   (set (cadr userline) *rfi-database*))
	  ((or (eq com 'm) (eq com 'more))
	   (rf-print '|unknown|))
	  ((eq com 'nospy)
	   (setq *rfi-spy* nil) (setq *emu-debug* nil))
	  ((eq com 'rx)
	   (setq *rfi-database*
		 (remove (cadr userline) *rfi-database* :test #'equal)))
	  ((eq com 'rxft)
	   (setq *rfi-database*
		 (remove (cons 'ft (cdr userline)) *rfi-database* :test #'equal)))
	  ((eq com 'rxhn)
	   (setq *rfi-database*
		 (remove (cons 'hn (cdr userline)) *rfi-database* :test #'equal)))
	  ((eq com 'replace)
	   (rfi-cmd-replace userline))
	  ((eq com 'script)
	   (rfi-cmd-startscript userline))
	  ((eq com 'showdepth)
	   (rfi-cmd-showdepth userline))
	  ((eq com 'spy)
	   (setq *rfi-spy* t) (setq *emu-debug* t))
	  ((eq com 'static)
	   (setq *rfi-static* t))
	  ((eq com 'tell)
	   (rfi-cmd-tell userline))
	  ((eq com 'untup)
	   (setq *rfi-database* (untup-database *rfi-database*)))
	  ((eq com 'fc-init)
	   (fc-init userline)) ;(Klaus 30.09.1990)
	  ((eq com 'break)
	   (break))
	  ((eq com 'style) ; Michael Herfert
	   (rfi-cmd-style userline))
	  ((eq com 'indexing)
	   (idx.idx-cmd (cdr userline))) ; INDEXING -- real-fun
	  ((or (eq com 'assem) (eq com 'asm))
	   (gwam.assem-cmd (cdr userline))) ; M. Sintek
	  ((eq com 'untype)
	   (setq *rfi-database* (untype *rfi-database*))) ; M. Sintek
	  ((eq com 'uncomma)
	   (setq *rfi-database* (uncomma *rfi-database*))) ; M. Sintek
	  ((eq com 'hitrans)
	   (setq *rfi-database* (hotrans *rfi-database*))) ; M. Sintek
          ((eq com 'unor)
           (setq *rfi-database* (unor *rfi-database*))) ; M. Sintek
          ((eq com 'unlambda)
           (setq *rfi-database* (unlambda *rfi-database*))) ; M. Sintek
          ((eq com 'unmacro)
           (setq *rfi-database* (unmacro *rfi-database*))) ; M. Sintek
	  )
    
					; get new input from user afer a interpreter-command
    (cond ((and (eq com 'bye) (rfi-cmd-bye))
	   t)
	  (t nil))))


(defun horizon-database (db)
  (footen-database (normalize-database (deanon-database (flatten-struc-database
(untup-database (uncomma (hotrans (unlambda (unor (unmacro (untype db))))))))))))

; will normally be overloaded (see above):
(defun normalize-database (db) db)
(defun uncomma (db) db)
(defun hotrans (db) db)
(defun unlambda (db) db)
(defun unor (db) db)
(defun unmacro (db) db)
(defun untype (db) db)


(defun naf-ground-p (x)
  (cond ((inst-t x) t)
        ((atom x) t)
        ((vari-t x) nil)
        (t (and (naf-ground-p (car x))
                (naf-ground-p (cdr x))))))

(defun naf-inversion (x)
  (cond ((null x) t)
        ((eq x 'false) t)
        (t nil)))


(defun rf (silent &rest goals)
  (let ((*readtable* *rfi-readtable*))
    (let ((userline (read-from-string (format nil "~A" goals))))
       (let ((res (and-process
		   (deanon-request (if *rfi-static* (flatten-request userline) userline))
		   '((bottom))
		   (list *rfi-prelude* *rfi-database*)
		   1
		   silent)))
	 (cond ((eq 'tupof silent) res)
	       ((eq 'once silent)
		(and res
		     (cons (ultimate-instant (un-inst (car res)) (cadr res))
			   (reverse (answer-bindings (cadr res) (cadr res))))))
	       ((eq 'naf silent) (naf-inversion res)))))))


(defun and-process (list-of-terms environment database level silent)
  ;
  ; tracing
  ;
  (if *rfi-spy*
      (rf-pprint-spy
       (cons (cond ((not silent) 'and)
		   ((eq 'tupof silent) 'tund)
		   ((eq 'once silent) '1and)
		   ((eq 'naf silent) 'nand))
	     (mapcar #'(lambda (g)
                         (ultimate-inst-instant g environment t))
		     list-of-terms))))
  ;
  ; extract next term
  ;
  (let ((term
         (cond (list-of-terms (ultimate-opis-assoc (car list-of-terms)
                                                   environment))
              ; built-ins may be bound to log. variables (is,ecal,lisp-builtin)
               (t 'true))))
    ;
    ; select next and-operation
    ;
    (cond
          ;
          ; is term unknown?
          ;    or
          ; is right side of is-term unknown?
          ;
          ((eq 'unknown (un-is term))
           nil)
          ;
          ; no more terms left?
          ;    or
          ; is term final and no more other terms are left?
          ;
          ((or (null list-of-terms)
               (and (null (cdr list-of-terms)) (final-p term)))
           (cond ((eq 'tupof silent) (list (ultimate-instant (un-inst term)
							     environment)))
		 ((eq 'once silent) (list term environment level))
                 ((eq 'naf silent) (ultimate-assoc term environment))
                 (t (rf-pprint
		     (mk-inst (ultimate-instant (un-inst term) environment)))
                    (print-bindings environment)
                    (more-p))))
          ;
          ; is term false?
          ;
          ((eq 'false term) nil)
          ;
          ; is term final?
          ;
          ((final-p term)
           (and-process (cdr list-of-terms)
                        environment
                        database
                        level
                        silent))
          ;
          ; is right side of is final?
          ;
          ((final-is-p term)
           (let ((is-environment
                  (unify (s-patt-is term)
                         (un-inst (s-expr-is term))
                         environment)))
             (and is-environment
                  (and-process
                   (or (cdr list-of-terms)
                       (cons (mk-inst (s-patt-is term))
                             nil))
                   is-environment
                   database
                   level
                   silent))))

          ;
          ; tupof:all solutions, once:first solution, naf:negation as failure
          ;
          ((tupof-t (un-is term))
           (let ((list-of-val (and-process
			       (ultimate-inst-instant
				(s-conj-tupof (un-is term))
				environment
				t)
			       environment
			       database
			       level
			       'tupof)))
;               (and list-of-val
                     (and-process (cons (gen-is
					 term
					 (list 'inst (cons 'tup list-of-val)))
                                        (cdr list-of-terms))
                                  environment
                                  database
                                  level
                                  silent)
;		)
	   ))

          ((once-t (un-is term))
           (let ((val-env-lev (and-process
			       (ultimate-inst-instant
				(s-conj-once (un-is term))
				environment
				t)
			       environment
			       database
			       level
			       'once)))
                (and val-env-lev
                     (and-process (cons (gen-is term (car val-env-lev))
                                        (cdr list-of-terms))
                                  (cadr val-env-lev)
                                  database
                                  (caddr val-env-lev)
                                  silent))))

          ((naf-t (un-is term))
           (let ((uiiconj (ultimate-inst-instant
                           (s-conj-naf (un-is term))
                           environment
                           t)))
                (cond ((not (naf-ground-p uiiconj))
                       (rf-error "(naf): "
				 (format nil "~A" uiiconj)
				 " non-ground argument"))
                      (t (and (naf-inversion (and-process
                                              uiiconj
                                              '((bottom))
                                              database
                                              level
                                              'naf))
                              (and-process
                               (cons (gen-is term 'true)
                                     (cdr list-of-terms))
                               environment
                               database
                               level
                               silent))))))

          ;
          ; call and-process, lisp or or-process
          ;
          (t
           (let ((conjunction-goal
                  (if *rfi-static*
                      '(nil)
                      (dynamic-flattener (un-is term) 1 level))))
             (cond ((car conjunction-goal)
                    (and-process
                     (append (car conjunction-goal)
                             (cons
                              (gen-is term
                                      (cadr conjunction-goal))
                              (cdr list-of-terms)))
                     environment
                     database
                     (+ level 1)
                     silent))
                   ;
                   ; meta-call?
                   ;
                   ((ecal-t (un-is term))
                    (and-process (cons (gen-is term
                                               (ultimate-assoc
                                                (un-inst (cadr (un-is term)))
                                                environment))
                                       (cdr list-of-terms))
                                 environment
                                 database
                                 level
                                 silent))
                   ;
                   ; call lisp?
                   ;
                   ((relfun-builtin-p (car (un-is term))) ;(Klaus 30.09.1990)
                    (and-process (cons (gen-is term
                                        (relfun-exec (un-is term) environment))
                                       (cdr list-of-terms))
                                 environment
                                 database
                                 level
                                 silent))
                   ((lisp-builtin-p (car (un-is term)))
                    (and-process (cons (gen-is term
                                        (lisp-exec (un-is term) environment))
                                       (cdr list-of-terms))
                                 environment
                                 database
                                 level
                                 silent))
                   (t
                    (or-process (s-first-db database)
				(s-rest-db database)
                                database
                                (cdr list-of-terms)
                                term
                                environment
                                level
                                silent))))))))




(defun or-process
  (db-left db-right database terms-left goal environment level silent)
  (cond ((and (null db-left) (null db-right)) nil)
	((null db-left)
	 (or-process (s-first-db db-right)
		     (s-rest-db db-right)
		     database
		     terms-left
		     goal
		     environment
		     level
		     silent))
        (t
           (let ((new-environment
                  (cond ((clause-t (un-is goal))
                         (unify (de-inst (s-clause (un-is goal)))
                                (rename-variables (car db-left)
                                                  (list level))
                                environment))
                        (t
                         (unify (de-inst (un-is goal))
                                (rename-variables (s-conclusion (car db-left))
                                                  (list level))
                                environment)))))
             (cond ((null new-environment)
                    (or-process (cdr db-left)
				db-right
                                database
                                terms-left
                                goal
                                environment
                                level
                                silent))
		   (t (let* ((premises
			      (cond ((clause-t (un-is goal))
				     (list (list 'inst (car db-left))))
				    (t (s-premises (car db-left)))))
			     (cutail (member '! premises)))
			(cond (cutail
			       (let ((guards (ldiff premises cutail)))
				 (cond ((null guards)
					(and-process
					 (append-is goal
						    (car db-left)
						    (rename-variables
						     (cdr cutail)
						     (list level))
						    terms-left)
					 new-environment
					 database
					 (1+ level)
					 silent))
				       (t (let ((val-env-lev
						 (and-process
						  (append-guards
						    (cdr cutail)
                                                    (car db-left)
						    (rename-variables
						     guards
						     (list level)))
						  new-environment
						  database
						  (1+ level)
						  'once)))
					    (cond (val-env-lev
						   (and-process
						    (append-is
						     goal
						     (car db-left)
						     (cons (car val-env-lev)
							   (rename-variables
							    (cdr cutail)
							    (list level)))
						     terms-left)
						    (cadr val-env-lev)
						    database
						    (caddr val-env-lev)
						    silent))
						  (t
						   (or-process
						    (cdr db-left)
						    db-right
						    database
						    terms-left
						    goal
						    environment
						    level
						    silent))))))))
			      ((eq 'tupof silent)
				 (append
				  (and-process
				   (append-is goal
					      (car db-left)
					      (rename-variables
					       premises
					       (list level))
					      terms-left)
				   new-environment
				   database
				   (1+ level)
				   silent)
				  (or-process (cdr db-left)
					      db-right
					      database
					      terms-left
					      goal
					      environment
					      level
					      silent)))
			      (t (or
				  (and-process
				   (append-is goal
					      (car db-left)
					      (rename-variables
					       premises
					       (list level))
					      terms-left)
				   new-environment
				   database
				   (1+ level)
				   silent)
				  (or-process (cdr db-left)
					      db-right
					      database
					      terms-left
					      goal
					      environment
					      level
					      silent)))))))))))




(defun dynamic-flattener
       (term varnum level)
  (cond ((final-p term) (list nil term))
	((is-tt term)
	 (list (cons term nil)
	       (mk-inst (s-patt-is term))))
;	((ecal-t term)
;	 (let ((varstruct (legnumvar (list varnum level))))
;	   (list (cons (mk-is varstruct term)
;		       nil)
;		 varstruct)))
	((final-p (car term))
	 (let ((conjunction-goal (dynamic-flattener (cdr term) varnum level)))
	   (list (car conjunction-goal)
		 (cons (car term)
		       (cadr conjunction-goal)))))
	(t
	 (let ((varstruct (legnumvar (list varnum level)))
	       (conjunction-goal (dynamic-flattener (cdr term) (+ varnum 1) level)))
	   (list
	    (cons (mk-is varstruct (car term))
		  (car conjunction-goal))
	    (cons varstruct
		  (cadr conjunction-goal)))))))
  

(defun append-guards
       (tail assertion guards)
  (cond ((and (ft-t assertion) (null tail)) guards)
	(t (append guards '(true)))))


(defun append-is
       (goal assertion prefix suffix)
  (cond ((is-t goal) (cond ((and (not (clause-t (un-is goal))) (hn-t assertion))
			    (append prefix (cons (mk-is (s-patt-is goal) 'true)
						 suffix)))
;			   ((ft-t assertion) (append-patt (s-patt-is goal)
;							  prefix
;							  suffix))
			   (t (append-patt (s-patt-is goal)
					   prefix
					   suffix))))
;			   (t (rf-error "(append-is): undefined clause type"))))
	((and (not (clause-t goal)) (hn-t assertion) (null suffix))
	 (append prefix '(true)))
	(t (append prefix suffix))))


(defun append-patt
       (patt prefix suffix)
  (cond ((null prefix) (rf-error "(append-patt): missing foot"))
	((null (cdr prefix)) (cons (mk-is patt (car prefix))
				   suffix))
	(t
	 (cons (car prefix)
	       (append-patt patt (cdr prefix) suffix)))))



(defun unify
       (x y environment)
  (let ((x (ultimate-assoc x environment))
	(y (ultimate-assoc y environment)))
    (cond ((equal x y) environment)
	  ((or (anonymous-p x)
	       (anonymous-p y))
	   environment)
	  ((vari-t x) (cons (list x y) environment))
	  ((vari-t y) (cons (list y x) environment))
	  ((or (atom x) (atom y)) nil)
	  (t
	   (let ((new-environment (unify (car x)
					 (car y)
					 environment)))
	     (and new-environment
		  (unify-args (cdr x) (cdr y) new-environment)))))))

(defun unify-args
       (x y environment)

;------------ throw out if and when dot unification becomes forbidden ---------
  (let ((x (ultimate-assoc x environment))
	(y (ultimate-assoc y environment)))
;------------------------------------------------------------------------------

    (cond ((and (null x) (null y)) environment)

;------------ throw out if and when dot unification becomes forbidden ---------
	  ((or (anonymous-p x)
	       (anonymous-p y))
	   environment)
	  ((vari-t x) (cons (list x y) environment))
	  ((vari-t y) (cons (list y x) environment))
;------------------------------------------------------------------------------

	  ((and (bar-t x) (bar-t y))               ; both cadr's should exist
	   (unify (cadr x) (cadr y) environment))  ; both cddr's should be nil
	  ((bar-t x)                                   ; cadr of x should exist
	   (unify (cadr x) (cons 'tup y) environment)) ; its cddr should be nil
	  ((bar-t y)                                   ; cadr of y should exist
	   (unify (cons 'tup x) (cadr y) environment)) ; its cddr should be nil
	  ((or (null x) (null y)) nil)

;------------ throw out if and when dot unification becomes forbidden ---------
	  ((or (atom x) (atom y)) nil)
;------------------------------------------------------------------------------

	  (t
	   (let ((new-environment (unify (car x)
					 (car y)
					 environment)))
	     (and new-environment
		  (unify-args (cdr x) (cdr y) new-environment)))))

;------------ throw out if and when dot unification becomes forbidden ---------
)
;------------------------------------------------------------------------------

 )





(defun ultimate-assoc
       (x environment)
  (cond ((vari-t x)
	 (let ((binding (assoc x environment :test #'equal)))
	   (cond ((null binding) x)
		 (t (ultimate-assoc (cadr binding)
				    environment)))))
	(t x)))


(defun ultimate-instant
       (x environment)
  (cond ((vari-t x)
	 (let ((binding (assoc x environment :test #'equal)))
	   (cond ((null binding) x)
		 (t (ultimate-instant (cadr binding)
				      environment)))))
	((atom x) x)
	((bar-t x)                                 ;  (cddr x) should be nil
	 (let ((barinst (ultimate-instant (cadr x) environment)))
	      (cond ((tup-t barinst) (cdr barinst))
		    (t (list (car x) barinst)))))  ;  (car x) is "|"
	(t
	 (cons (ultimate-instant (car x) environment)
	       (ultimate-instant (cdr x) environment)))))



(defun ultimate-opis-assoc
       (x environment)
  (cond ((or (final-p x) (and (not (vari-t (car x)))
			      (not (eq 'is (car x))))) x)
	(t (let ((opassoc (ultimate-assoc (car x) environment)))
	     (cond ((eq 'is opassoc)
		    (mk-is (cadr x)
			   (cond ((or (final-p (caddr x))
				      (not (vari-t (car (caddr x)))))
				  (caddr x))
				 (t (cons
				     (mk-inst
				      (ultimate-assoc (car (caddr x))
						      environment))
				     (cdr (caddr x)))))))
		   (t (cons (mk-inst opassoc) (cdr x))))))))


(defun ultimate-inst-instant
       (x environment showinst)
  (cond ((vari-t x)
         (let ((binding (assoc x environment :test #'equal)))
           (cond ((null binding) x)
                 (t (cond (showinst
                           (mk-inst (ultimate-inst-instant (cadr binding)
                                                           environment
                                                           nil)))
                          (t (ultimate-inst-instant (cadr binding)
                                                    environment
                                                    nil)))))))
        ((atom x) x)
        ((inst-t x) (mk-inst
                     (ultimate-inst-instant (un-inst x) environment nil)))
        ((bar-t x)                                 ;  (cddr x) should be nil
         (let ((barinst (ultimate-inst-instant (cadr x)
                                               environment
                                               showinst)))
              (cond ((and showinst (inst-t barinst) (tup-t (un-inst barinst)))
		     (mapcar #'mk-inst (cdr (un-inst barinst))))
		    ((and (not showinst) (tup-t barinst))
		     (cdr barinst))
                    (t (list (car x) barinst)))))  ;  (car x) is "|"
        ((is-tt x)
	 (mk-is (ultimate-inst-instant (s-patt-is x) environment nil)
		(ultimate-inst-instant (s-expr-is x) environment showinst)))
        (t
         (cons (ultimate-inst-instant (car x) environment showinst)
               (ultimate-inst-instant (cdr x) environment showinst)))))



(defun rename-variables
       (term listified-level)
  (cond ((vari-t term) (append term listified-level))
	((atom term) term)
	(t
	 (cons (rename-variables (car term) listified-level)
	       (rename-variables (cdr term) listified-level)))))



(defun print-bindings (environment)
  (mapcar #'(lambda (varval)
	      (rf-pprint (list (car varval) '= (mk-inst (cadr varval)))))
	  (reverse (answer-bindings environment environment))))

(defun answer-bindings (environ-left environ)
    (and (cdr environ-left)
	 (let ((variable (caar environ-left)))
	   (cond ((null (level-of variable))
		  (cons (list (variable-name variable)
			      (ultimate-instant variable environ))
			(answer-bindings (cdr environ-left) environ)))
		 (t (answer-bindings (cdr environ-left) environ))))))



(defun more-p
       nil
  (do ((response (readl) (readl)))
      ((or (or (eq (car response) 'm) (eq (car response) 'more))
           (eq (car response) 'ori)
	   (eq (car response) 'bye)
           (not (rfi-command-p response)))
	 (if (or (eq (car response) 'm) (eq (car response) 'more)) nil response))
    (rfi-command response)))


(defun mk-inst
       (term)
  (cond ((or (atom term) (vari-t term)) term)
	(t (list 'inst term))))


(defun gen-is
       (term goal)
  (cond ((is-t term) (mk-is (s-patt-is term) goal))
	(t goal)))



(defun mk-is
       (patt expr)
  (list 'is patt expr))



(defun un-is
       (term)
  (cond ((is-t term) (s-expr-is term))
	(t term)))



(defun s-expr-is (isterm) (caddr isterm))

(defun s-expr-mis (isterm) (car (last isterm)))



(defun s-patt-is (isterm) (cadr isterm))

(defun s-patt-mis (isterm)
  (let ((amptail (member '& isterm)))
    (cond (amptail (cdr (ldiff isterm amptail)))
	  (t (list (cadr isterm))))))



(defun final-is-p
       (term)
  (and (is-t term)
       (final-p (s-expr-is term))))

(defun final-mis-p
       (term)
  (and (is-t term)
       (final-p (s-expr-mis term))))



(defun final-p
       (term)
  (or (atom term)
      (inst-t term)
      (vari-t term)))



(defun de-inst
       (x)
  (cond ((atom x) x)
	((inst-t x) (cadr x))
	(t (cons (de-inst (car x))
		 (de-inst (cdr x))))))



(defun un-inst
       (x)
  (cond ((inst-t x) (cadr x))
	(t x)))


; -----------------------------------------------------------------------------
;
; deanonymizer
;
; -----------------------------------------------------------------------------

(defun deanon-request
       (request)
  (car (name-anonymous request '(1 0) nil)))


(defun deanon-database
       (database)
  (mapcar #'deanon-clause database))


(defun deanon-clause
       (clause)
  (let ((variables (collect-variables clause)))
    (car (name-anonymous clause (nextnew '(0) variables) variables))))


(defun name-anonymous
       (term varnuml oldvars)
  (cond ((anonymous-p term)
	 (list (legnumvar varnuml) (nextnew varnuml oldvars)))
	((atom term) (list term varnuml))
	(t
	 (let ((carterm-carvarnuml
		(name-anonymous (car term) varnuml oldvars)))
	   (let ((cdrterm-cdrvarnuml
		  (name-anonymous (cdr term)
				  (cadr carterm-carvarnuml)
				  oldvars)))
	     (list (cons (car carterm-carvarnuml) (car cdrterm-cdrvarnuml))
		   (cadr cdrterm-cdrvarnuml)))))))



; -----------------------------------------------------------------------------
;
; singlifier
;
; -----------------------------------------------------------------------------


(defun singlify-database
       (database)
  (mapcar #'singlify-clause database))


(defun singlify-clause
       (clause)
 (cond ((hn-t clause)
	(cons (s-kind clause)
	      (cons (singlify-head (s-head clause))
		    (singlify-body (s-premises clause)))))
       ((ft-t clause)
	(cons (s-kind clause)
	      (cons (singlify-head (s-head clause))
		    (mk-premises (singlify-body (s-mbody clause))
				 (singlify-values (s-mfoot clause))))))
       (t (rf-error "(singlify-clause): undefined clause type"))))


(defun singlify-body (terms)
  (mapcar #'singlify-term terms))

(defun singlify-term (term)
  (cond ((or (final-p term) (final-mis-p term)) term)
        ((is-t term)
	 (cond ((lisp-builtin-p (car (s-expr-mis term)))
		(mk-is (s-patt-is term)
		       (list (car (s-expr-mis term))
			     '\|
			     (singlify-values
			      (cdr (s-expr-mis term))))))
	       (t (mk-is (cons 'tup (s-patt-mis term))
			 (singlify-nested (s-expr-mis term))))))
	((lisp-builtin-p (car term))
	 (list (car term) '\| (singlify-values (cdr term))))
	(t (list (car term) (singlify-values (cdr term))))))

(defun singlify-nested (term)
  (cond ((final-mis-p term) (list 'tup term))
        ((is-t term)
	 (cond ((lisp-builtin-p (car (s-expr-mis term)))
		(list 'tup (mk-is (s-patt-is term)
				  (list (car (s-expr-mis term))
					'\|
					(singlify-values
					 (cdr (s-expr-mis term)))))))
	       (t (mk-is (cons 'tup (s-patt-mis term))
			 (singlify-nested (s-expr-mis term))))))
	((lisp-builtin-p (car term))
	 (list 'tup (list (car term) '\| (singlify-values (cdr term)))))
	(t (list (car term) (singlify-values (cdr term))))))


(defun singlify-values (term)
  (let ((terms (singlify-value term nil)))
    (cond ((null terms) (list 'inst (list 'tup)))
	  ((null (cdr terms)) (car terms))
	  (t (cons 'appifun terms)))))

;(defun singlify-values (terms)
;  (cond ((null terms) (list 'inst (list 'tup)))
;	((null (cdr terms)) (singlify-value (car terms)))
;	(t (cons 'appifun (mapcar #'singlify-value terms)))))

(defun singlify-value (term finals)
  (cond ((and finals (null term))
	 (list (list 'inst (cons 'tup (reverse finals)))))
	((null term) nil)
	((final-p (car term))
	 (singlify-value (cdr term) (cons (un-inst (car term)) finals)))
	(finals (cons (list 'inst (cons 'tup (reverse finals)))
		      (cons (singlify-nested (car term))
			    (singlify-value (cdr term) nil))))
	(t (cons (singlify-nested (car term))
		 (singlify-value (cdr term) nil)))))

;(defun singlify-value (term)
;  (cond ((eq '\| term) term)
;	((final-p term) (list 'inst (list 'tup (un-inst term))))
;	(t (singlify-nested term))))

(defun singlify-head (term)
  (list (car term) (cons 'tup (cdr term))))





; -----------------------------------------------------------------------------
;
; extrarger
;
; -----------------------------------------------------------------------------


(defun extrarg-database
       (database)
  (mapcar #'extrarg-clause database))


(defun extrarg-clause
       (clause)
 (cond ((hn-t clause)
	(cons (s-kind clause)
	      (cons (s-head clause) (extrarg-body (s-premises clause)))))
       ((ft-t clause)
	(let ((foot (s-foot clause)))
	  (cond ((final-p foot)
		 (cons 'hn
		       (cons (extrarg-head (s-head clause) (un-inst foot))
			     (extrarg-body (s-body clause)))))
		(t (let ((newvar
			  (legnumvar
			   (nextnew '(0) (collect-variables clause)))))
		     (cons 'hn
			   (cons (extrarg-head (s-head clause) newvar)
				 (mk-premises (extrarg-body (s-body clause))
					      (extrarg-foot foot newvar)))))))))
       (t (rf-error "(extrarg-clause): undefined clause type"))))


(defun extrarg-body (terms)
  (mapcar #'extrarg-term terms))

(defun extrarg-term (term)
  (cond ((final-is-p term) term)
        ((is-t term)
	 (cond ((lisp-builtin-p (car (s-expr-is term))) term)
	       (t (cons (car (s-expr-is term))
			(cons (mk-inst (s-patt-is term))
			      (cdr (s-expr-is term)))))))
	(t term)))


(defun extrarg-head (term val) (cons (car term) (cons val (cdr term))))

(defun extrarg-foot (term val)
  (cond ((lisp-builtin-p (car term)) (mk-is val term))
	(t (cons (car term) (cons val (cdr term))))))




; -----------------------------------------------------------------------------
;
; static flattener
;
; -----------------------------------------------------------------------------


(defun flatten-request
       (request)
  (and-flattener nil request nil '(1 -1)))



(defun flatten-database
       (database)
  (mapcar #'flatten-clause database))



(defun flatten-struc-database
       (database)
  (mapcar #'flatten-struc-clause database))



(defun flatten-clause
       (clause)
  (let ((variables (collect-variables clause)))
    (cons (s-kind clause)
	  (cons (s-head clause)
		(and-flattener nil
			       (s-premises clause)
			       variables
			       (nextnew '(0) variables))))))



(defun flatten-struc-clause
       (clause)
  (let ((variables (collect-variables clause)))
    (cons (s-kind clause)
	  (let ((conjunction-conclusion-varnuml
		 (static-flattener 'h (s-conclusion clause)
				   (nextnew '(0) variables)
				   variables)))
	    (cons (cond ((cut-p clause) (list 'cut (cadr conjunction-conclusion-varnuml)))
			(t                         (cadr conjunction-conclusion-varnuml)))
		  (and-flattener t 
				 (append (car conjunction-conclusion-varnuml)
					 ; general cut should go here
					 (s-premises clause))
				 variables
				 (caddr conjunction-conclusion-varnuml)))))))



(defun and-flattener
       (struc list-of-terms oldvars varnuml)
  (cond ((null list-of-terms) nil)
	(t
	 (let ((term (car list-of-terms)))
	   (cond ((and struc (is-tt term) (not (convar-p (s-patt-is term))))
		  (let ((varstruct (legnumvar varnuml)))  ; complex is lhs gets
		    (and-flattener struc
				   (cons (mk-is varstruct ; replaced by new var
					        (mk-inst (s-patt-is term)))
					 (cons (mk-is varstruct ; using TWO is
						      (s-expr-is term))
					       (cdr list-of-terms)))
				   oldvars
				   (nextnew varnuml oldvars))))
                 ((or (and struc 
			   (or (convar-p term)
			       (convar-is-p term)
			       (flat-struc-p term)
			       (flat-struc-is-p term)))
		      (and (not struc)
			   (or (final-p term)
			       (final-is-p term)
			       (flat-p term)
			       (flat-is-p term))))
		  (cons term 
			(and-flattener struc 
				       (cdr list-of-terms)
				       oldvars
				       varnuml)))
		 (t
		  (let ((conjunction-goal-varnuml
			 (static-flattener struc
					   (un-is term)
					   varnuml
					   oldvars)))
		    (and-flattener
		     struc
		     (append (car conjunction-goal-varnuml)
			     (cons
			      (gen-is term
				      (cadr conjunction-goal-varnuml))
			      (cdr list-of-terms)))
		     oldvars
		     (caddr conjunction-goal-varnuml)))))))))



(defun convar-is-p
       (term)
  (and (is-t term)
       (convar-p (s-expr-is term))))



(defun static-flattener
       (struc term varnuml oldvars)
  (cond ((or (and struc (convar-p term)) (and (not struc) (final-p term))) (list nil term varnuml))
	((is-tt term)
	 (list (cons term nil)
	       (mk-inst (s-patt-is term))
	       varnuml))
;	((ecal-t term)
;	 (let ((varstruct (legnumvar varnuml)))
;	   (list (cons (mk-is varstruct term)
;		       nil)
;		 varstruct
;		 (nextnew varnuml oldvars))))
	((and struc (inst-t term))
	 (let ((conjunction-goal-varnuml 
		(static-flattener 'h (cadr term) varnuml oldvars)))
	   (list (car conjunction-goal-varnuml)
		 (mk-inst (cadr conjunction-goal-varnuml))
		 (caddr conjunction-goal-varnuml))))
	((or (and struc (convar-p (car term))) (and (not struc) (final-p (car term))))
	 (let ((conjunction-goal-varnuml
		(static-flattener struc (cdr term) varnuml oldvars)))
	   (list (car conjunction-goal-varnuml)
		 (cons (car term)
		       (cadr conjunction-goal-varnuml))
		 (caddr conjunction-goal-varnuml))))
	(t
	 (let ((varstruct (legnumvar varnuml))
	       (conjunction-goal-varnuml
		(static-flattener struc
				  (cdr term)
				  (nextnew varnuml oldvars)
				  oldvars)))
	   (list
	    (cons (mk-is varstruct (cond ((eq struc 'h) (mk-inst (car term))) (t (car term))))
		  (car conjunction-goal-varnuml))
	    (cons varstruct
		  (cadr conjunction-goal-varnuml))
	    (caddr conjunction-goal-varnuml))))))



(defun legnumvar (varnuml)
  (cons 'vari (cons (intern (princ-to-string (car varnuml))) (cdr varnuml))))



(defun collect-variables (clause)
  (cond ((atom clause) nil)
	((vari-t clause) (list (name-of clause)))
	(t
	 (append (collect-variables (car clause))
		 (collect-variables (cdr clause))))))



(defun nextnew
       (varnuml oldvars)
  (let ((incr (+ (car varnuml) 1)))
    (let ((incrl (cons incr (cdr varnuml))))
      (cond ((member incr oldvars)
	     (nextnew incrl oldvars))
	    (t incrl)))))



(defun flat-is-p
       (term)
  (and (is-t term)
       (flat-p (s-expr-is term))))



(defun flat-struc-is-p
       (term)
  (and (is-t term)
       (flat-struc-p (s-expr-is term))))



(defun flat-p
       (term)
  (cond ((final-p term) t)
	((or (is-tt term)
	     (ecal-t (cdr term)))
	 nil)
	((final-p (car term)) (flat-p (cdr term)))))



(defun flat-struc-p
       (term)
  (cond ((convar-p term) t)
	((and (inst-t term) (flat-struc-p (cadr term))) t)
	((or (is-tt term)
	     (ecal-t (cdr term)))
	 nil)
	((convar-p (car term)) (flat-struc-p (cdr term)))))



(defun convar-p (term)
  (or (atom term) (vari-t term)))






(defun anonymous-p (x) (eq x 'id))


(defun s-first-db (database) (car database))

(defun s-rest-db (database) (cdr database))


(defun s-kind (clause) (car clause))

(defun s-premises (clause) (cddr clause))

(defun s-head (clause) (cadr clause))

(defun s-conclusion
       (clause)
  (cond ((cut-p clause) (cadr (s-head clause)))
	(t (s-head clause))))



(defun cut-p (clause)
  (and (consp clause)
       (cut-t (s-head clause))))


(defun s-body (clause) (butlast (cddr clause)))

(defun s-mbody (clause)
  (let ((amptail (member '& (cddr clause))))
    (cond (amptail (ldiff (cddr clause) amptail))
	  (t (butlast (cddr clause))))))

(defun s-foot (clause) (car (last (cddr clause))))

(defun s-mfoot (clause)
  (let ((amptail (member '& (cddr clause))))
    (cond (amptail (cdr amptail))
	  (t (last (cddr clause))))))

(defun mk-premises (body foot) (append body (list foot)))


; -----------------------------------------------------------------------------
;
; raw (func-t) and fine (func-tt) predicates for terms with functor func
;
; -----------------------------------------------------------------------------

(defun cut-t (x) (and (consp x) (eq 'cut (car x))))
(defun cut-tt (x) (and (cut-t x) (= 2 (length x))))

(defun ecal-t (x) (and (consp x) (eq 'ecal (car x))))
(defun ecal-tt (x) (and (ecal-t x) (= 2 (length x))))

(defun is-t (x) (and (consp x) (eq 'is (car x))))
(defun is-tt (x) (and (is-t x) (= 3 (length x))))

(defun vari-t (x) (and (consp x) (eq 'vari (car x))))
(defun vari-tt (x) (and (vari-t x) (or (= 2 (length x)) (= 3 (length x)))))

(defun inst-t (x) (and (consp x) (eq 'inst (car x))))
(defun inst-tt (x) (and (inst-t x) (= 2 (length x))))

(defun uninst-t (x) (and (consp x) (eq 'uninst (car x))))
(defun uninst-tt (x) (and (uninst-t x) (= 2 (length x))))

(defun hn-t (x) (and (consp x) (eq 'hn (car x))))
(defun hn-tt (x) (and (hn-t x) (< 1 (length x))))

(defun ft-t (x) (and (consp x) (eq 'ft (car x))))
(defun ft-tt (x) (and (ft-t x) (< 2 (length x))))

(defun tup-t (x) (and (consp x) (eq 'tup (car x))))

(defun cns-t (x) (and (consp x) (eq 'cns (car x))))

(defun bar-t (x) (and (consp x) (eq '\| (car x))))

(defun clause-t (x) (and (consp x) (eq 'clause (car x))))


(defun tupof-t (x) (and (consp x) (eq 'tupof (car x))))
(defun once-t (x) (and (consp x) (eq 'once (car x))))
(defun naf-t (x) (and (consp x) (eq 'naf (car x))))

(defun s-conj-tupof (tupofterm) (cdr tupofterm))
(defun s-conj-once (onceterm) (cdr onceterm))
(defun s-conj-naf (nafterm) (cdr nafterm))

(defun s-clause (clauseterm) (cadr clauseterm))


; -----------------------------------------------------------------------------
;
; i/o functions for filesystem 
;
; -----------------------------------------------------------------------------




(defun rfi-extension (file extension
                      &aux (filename (cond ((stringp file) file)
                                           ((symbolp file)
					    (string-downcase (string file)))
                                           (t ""))))
  (let ((slash (position #\/ filename :from-end t))
        (point (position #\. filename :from-end t)))
    (cond ((string/= filename "")
           (cond ((or (and slash point (< slash point))
                      (and (null slash) point))
                  filename)
                 ((or (and slash point (> slash point))
                      (null point))
                  (concatenate 'string filename extension))))
          (T (rf-error "(rfi-extension): "
		       (format nil "~A" file)
		       " filename must be of type string or symbol!")))))



(defun rfi-load-prelude nil
  (if (probe-file "prelude.rf")
      (setq *rfi-prelude* (rfi-cmd-consult-1 "prelude.rf"))
      (rf-print "REMARK (rfi-load-prelude): no prelude.rf could be loaded")))


(defun rfi-cmd-replace (userline
                        &aux (filename (rfi-extension (cadr userline) 
						      (rf-or-rfp) )))
  (if (probe-file filename)
      (setq *rfi-database* (rfi-cmd-consult-1 filename))
    (rf-error "(rfi-cmd-replace): " filename " file doesn't exist!")))



; Default file-extension is style-dependent:

(defun rf-or-rfp ()
  ; returns the default file-extension
  (if (eq 'prolog *style*)
      ".rfp"
      ".rf" ))

(defun rfi-cmd-consult (userline)
  (let* ((filename (rfi-extension (cadr userline) (rf-or-rfp))))
    (if (probe-file filename)
	(setq *rfi-database* (append *rfi-database* 
				     (rfi-cmd-consult-1 filename)))
      (rf-error "(rfi-cmd-consult): " filename " file doesn't exist!"))))



(defun rfi-cmd-consult-1 (file)
  (normalize-consulted-db (read-db-from-file file)))


(defun read-db-from-file (file)
  (rf-princ-like-lisp (format nil "~A Reading file \"~A\" .." 
			      (if (eq *style* 'prolog) "%" ";") file))
  (if (eq *style* 'prolog)
      (pro-read-data-base file)        ; returns NIL if error detected
    (with-open-file (ifile file :direction :input)
		    (let ((*readtable* *rfi-readtable*))
		      (do ((clause nil)
			   (tmpdb nil (cons clause tmpdb)))
			  ((null (setq clause (read ifile nil nil nil)))
			   (reverse tmpdb)))))))




(defun normalize-consulted-db (db)
  (tag-clauses (unsetq-setqs db)))



(defun unsetq-setqs (l)         ; ((setq 'a ( ((a1...)...) ...)...(setq 'b (aa...))))
  (apply #'append (mapcar #'unsetq-single-setq l)))

(defun unsetq-single-setq (l1)
  (cond ((and (consp l1) (equal 'setq (car l1))) (cadr (caddr l1)))
	(t (list l1))))



(defun tag-clauses (db)
  (mapcar #'tag-single-clause db))

(defun tag-single-clause (clause)
  (cond ((consp (car clause))        ; ((p1...)...) instead of (ft/hn (p1...)...)
	 (if (endp (cdr clause))
	     (cons 'hn clause)
	     (cons 'ft clause)))
	(t clause)))





(defun rfi-cmd-tell (userline)
  (let ((filename (rfi-extension (cadr userline) (rf-or-rfp))))
    (if (probe-file filename)
	(if (rfi-yes-or-no-p "file already exists - overwrite? ")
	    (progn (delete-file filename)
		   (rfi-cmd-tell-1 filename)))
      (rfi-cmd-tell-1 filename))))




(defun rfi-cmd-tell-1 (file)
  (with-open-file (ofile file :direction :output :if-exists :supersede)
		  (let ((*rfi-script-output* nil)
			(*rfi-standard-output* ofile))
		    (rf-pprint-db *rfi-database*)
		    (fresh-line ofile))))


; ---------------------------------------------------------------------------
; 
; i/o functions for lisp-variables
;
; ---------------------------------------------------------------------------

(defun rfi-cmd-lreplace (userline)
  (if (cadr userline)
      (if (boundp (cadr userline))
	  (setq *rfi-database* (rfi-cmd-lconsult-1 (eval (cadr userline))))
	  (rf-print '|error (rfi-cmd-lreplace): lisp-variable isn't bound to anything!|))
      (rf-print '|error (rfi-cmd-lreplace): from where?|)))


(defun rfi-cmd-lconsult (userline)
  (if (cadr userline)
      (if (boundp (cadr userline))
	  (setq *rfi-database* (append *rfi-database* (rfi-cmd-lconsult-1 (eval (cadr userline)))))
	  (rf-print '|error (rfi-cmd-lconsult): lisp-variable isn't bound to anything!|))
      (rf-print '|error (rfi-cmd-lconsult): from where?|)))



(defun rfi-cmd-lconsult-1 (db)
  (normalize-consulted-db 
   (let ((*readtable* *rfi-readtable*))
     (read-from-string (write-to-string db)))))


; ---------------------------------------------------------------------------
; 
; syntax style switching and PROLOG-style command handling
; 
; ---------------------------------------------------------------------------

(defun rfi-cmd-style (userline)
  (let ((error-p nil) )
    (if (= 2 (length userline))
	(progn
	  (case (second userline)
		(lisp (setq *rfi-prompt* 
			    (if (eq *rfi-machine* 'interpreter)
				'|rfi-l> |
			      '|rfe-l> |))
		      (setq *style* 'lisp) )
		(prolog (setq *rfi-prompt* 
			      (if (eq *rfi-machine* 'interpreter)
				  '|rfi-p> |
				'|rfe-p> |))
			(setq *style* 'prolog) )
		(t (setq error-p t)) ))
      (setq error-p t) )
    (if error-p
	(progn
	  (rf-terpri)
	  (rf-princ-like-lisp "Error. Use:")
	  (rf-terpri)
	  (rf-princ-like-lisp "   style lisp")
	  (rf-terpri)
	  (rf-princ-like-lisp "or")
	  (rf-terpri)
	  (rf-princ-like-lisp "   style prolog")
	  (rf-terpri) ))))

(defun handle-rfi-cmd (cmd-as-symbol rest-of-line-as-string full-line)
  ;; Returns an rfi-command in Lisp-like syntax.
  ;; Returns NIL if syntax-error.
  (let ((error-p nil))
    (labels
     ((read-clause ()
		   (let ((result (pro-read-clause rest-of-line-as-string)))
		     (setq error-p (null result)) 
		     result ))
      (read-goal ()
		 (let ((result (pro-read-goal rest-of-line-as-string)))
		   (setq error-p (null result)) 
		   result )))
     (let ((result
	    (case cmd-as-symbol
		  (a0
		   (list 'a0 (read-clause)))
		  (az 
		   (list 'az (read-clause)))
		  ((listing l)
		   (if (string= "" rest-of-line-as-string)
		       '(listing)
		     (cons 'listing
			   (read-goal) )))
		  (rx
		   (list 'rx (read-clause)) )
		  ;; no syntax-transformation is needed, when calling 
		  ;; these commands:
		  (( builtins compile consult destroy emul endscript exec
			      horizon inter lisp listclass listcode lreplace
			      m more nospy ori replace rf break script 
			      showdepth spy tell verti)
		   (transform-string-to-single-lisp-object full-line) )
		  ;; other rfi-commands (not listed in the RFM-Manual):
		  (t
		   (transform-string-to-single-lisp-object full-line) ))))
       (if error-p	   
	   nil   
	 result )))))

; ---------------------------------------------------------------------------
; 
; pretty-printer
; 
; author: R. Scheubrein
;   date: March 1990
; 
; based on xlisp 1.6 pretty-printer and kkl-pretty-printer
; 
; ---------------------------------------------------------------------------

(defvar pp-stack* nil)
(defvar pp-istack* nil)
(defvar pp-currentpos* nil)


(setq pp-stack* nil)
(setq pp-istack* nil)
(setq pp-currentpos* nil)


(defmacro pp-push (*item *stack)
  `(setq ,*stack (cons ,*item ,*stack)))


(defmacro pp-pop (*stack)
  `(let ((top* (car ,*stack)))
     (setq ,*stack (cdr ,*stack))
     top*))


(defun pp-init ()
  (setq pp-stack* nil)
  (setq pp-istack* '(0))
  (setq pp-currentpos* 0))


; The following two functions are most important to interface lisp2pro.lsp.

(defun pp (*expr)
  (if (eq *style* 'prolog)
      (progn
	(pro-print *expr)
	(rf-terpri)
	t)
    (progn
      (pp-init)
      (pp-expr *expr)
      (pp-newline)
      t )))



(defun pp-clause (clause)
  (if (eq *style* 'prolog)
      (progn
	(pro-print clause)
	(rf-terpri)
	t )
    (progn
      (pp-init)
      (if (and (< (length clause) 3) (pp-fits clause))
	  (pp-small-clause clause)
	(pp-big-clause clause))
      (pp-newline)
      t )))



(defun pp-small-clause (c)
  (pp-expr c))


(defun pp-big-clause (c)
  (pp-expr '|(|)
	   (pp-expr (car c))
	   (pp-expr '| |)
	   (pp-expr (cadr c))
	   (if (> (length c) 2)
	       (pp-newline))
	   (do ((item (cddr c) (cdr item)))
	       ((null item))
	     (pp-expr '|    |)
	     (pp-expr (car item))
	     (if (not (null (cdr item))) (pp-newline)))
	   (pp-expr '| )|))


(defun pp-expr (*expr)
  (if (consp *expr)
      (cond ((vari-t *expr)
             (pp-vari *expr))
            ((inst-t *expr)
             (pp-list (cdr *expr) '|`| '||))
            ((uninst-t *expr)
             (pp-list (cdr *expr) '|,| '||))
            ((cut-t *expr)
             (pp-list (cdr *expr) '|!| '||))
            ((ecal-t *expr)
             (pp-list (cdr *expr) '|@| '||))
            ((is-t *expr)
             (pp-list *expr '|(| '|)|))
            (t
             (pp-list *expr '|(| '|)|)))
      (pp-prin *expr)))



(defun pp-list (*expr pre post)
  (if (pp-fits *expr)
      (pp-flat-list *expr pre post)
      (pp-broken-list *expr pre post)))


(defun pp-flat-list (*expr pre post)
  (pp-prin pre)
  (do ((item *expr (cdr item)))
      ((null item))
    (pp-expr (car item))
    (if (not (null (cdr item))) (pp-prin '| |)))
  (pp-prin post))


(defun pp-broken-list (*expr pre post)
  (pp-start pre)
  (pp-pushmargin)
  (do ((item *expr (cdr item)))
      ((null item))
    (pp-expr (car item))
    (if (not (null (cdr item))) (pp-newline)))
  (pp-popmargin)
  (pp-finish post))


(defun pp-vari (x)
  (pp-expr (variable-name x)))


(defun pp-start (pre)
  (pp-prin pre)
  (pp-push ")" pp-stack*))


(defun pp-finish (post)
  (cond ((equal ")" (pp-top pp-stack*))
	(pp-prin post))
  (t
   (pp-prin '| |)
   (pp-prin post)
   (pp-pop pp-stack*)))
(pp-pop pp-stack*))




(defun pp-pushmargin()
  (pp-push pp-currentpos* pp-istack*))

(defun pp-popmargin ()
  (pp-pop pp-istack*))


(defun pp-newline ()
  (if (equal ")" (pp-top pp-stack*)) (pp-push " " pp-stack*))
(rf-terpri)
(spaces (pp-top pp-istack*))
(setq pp-currentpos* (pp-top pp-istack*)))


(defun pp-prin (*expr)
  (if (rfi-special-symbolp *expr)
      (setq pp-currentpos* (+ pp-currentpos* (flatc *expr)))
      (setq pp-currentpos* (+ pp-currentpos* (flatsize *expr))))
  (rf-princ *expr))


(defun pp-top (*stack)
  (car *stack))

(defun spaces (n)
  (dotimes (x n) (rf-princ '| |)))

(defun flatc (*expr)
  (length (princ-to-string *expr)))

(defun pp-fits (expr)
  (< (+ pp-currentpos* (flatsize expr)) *rf-print-width*))

(defun flatsize (*expr)
  (length (prin1-to-string *expr)))








; -----------------------------------------------------------------------------
; 
; print-routines for rfi:
; 
; rf-print, rf-princ, rf-terpri, rf-fresh-line, rf-pprint, 
; rf-pprint-db, rf-pprint-spy
; 
; -----------------------------------------------------------------------------

(defun rf-error (&rest liste)
  (rf-print (intern (apply #'concatenate (cons 'string (cons "ERROR " liste)))))
  (throw :toploop (readl)))


(defun rf-print (x)
  (rf-fresh-line)
  (rf-princ x))

(defun rf-princ-like-lisp (x)
  ; prints x using princ.
  (if (rfi-script-mode-p)
      (princ x *rfi-script-output*) )
  (princ x *rfi-standard-output*) )

(defun rf-princ (x)
  (if (rfi-script-mode-p) 
      (rf-princ-1 x *rfi-script-output*))
  (rf-princ-1 x *rfi-standard-output*))

(defun rf-princ-1 (x stream)
  (let ((*print-case* :downcase))
    (if (rfi-special-symbolp x)
	(princ x stream)
	(prin1 x stream))))

(defun rf-format (&rest args)
  (when (rfi-script-mode-p) (apply #'format (cons *rfi-script-output* args)))
  (apply #'format (cons *rfi-standard-output* args)))


(defun rf-terpri ()
  (if (rfi-script-mode-p) 
      (terpri *rfi-script-output*))
  (terpri *rfi-standard-output*)
  t)



(defun rf-fresh-line ()
  (if (rfi-script-mode-p)
      (fresh-line *rfi-script-output*))
  (fresh-line *rfi-standard-output*))



(defun rf-pprint-db (db)
  (cond ((listp db)
	 (do ((clauselist db (cdr clauselist)))
	     ((endp clauselist))
	   (cond ((listp (car clauselist))
		  (rf-pprint-clause (car clauselist)))
		 (T
		  (rf-error
		   "(rf-pprint-db): "
		   (format nil "~A" (car clauselist))
		   " clause isn't a list")))))
	(T
	 (rf-error "(rf-pprint-db): " (format nil "~A" db) " database isn't a list"))))


(defun rf-pprint-clause (c)
  (pp-clause (normalize-expr c)))


(defun rf-pprint (x)
  (pp (normalize-expr x)) x)


(defun pretty-print (x)
  (rf-pprint (mk-inst x))
  x)

(defun rf-pprint-spy (x)
;  (rf-terpri)
  (let ((l (1- (length x))))
    (cond ((or (= 0 *rfi-showdepth*) (>= *rfi-showdepth* l)) 
	   (pp (normalize-expr x)))
	  (t
	   (pp (normalize-expr (append 
				(butlast x (- l *rfi-showdepth*))
				(list (intern 
				       (make-string (- l *rfi-showdepth*)
						    :initial-element #\*))))))))))


; new builtins:

(defun wait (text)
  (rf-princ text)
  (read-char))


(defun operators () ; collect all rfi-database operator names
  (remove-duplicates (mapcar #'caadr *rfi-database*)))


(defun date ()
  (multiple-value-bind (a b c d e f g h i) (get-decoded-time)
                       (list d e f c b)))


; ---------------------------------------------------------------------------
; 
; normalizing lists: no more (rfi-)dotted pairs
; 
; ---------------------------------------------------------------------------
(defun normalize-expr (x)
  (cond ((atom x) x)
	((rfi-dotted-pair-p x) (list (normalize-expr (car x)) '|.| (normalize-expr (cdr x))))
	(t (cons (normalize-expr (car x)) (normalize-expr (cdr x))))))



(defun rfi-atom (a)
  (or (atom a) (rfi-quasi-atom a)))


(defun rfi-quasi-atom (a)
  (or (inst-tt a) (uninst-tt a) (vari-tt a) (cut-tt a) (ecal-tt a) (is-tt a)))


(defun rfi-dotted-pair-p (l)
  (or
   (and (listp l) (= 1 (length l)) (not (null (cdr l))))
   (rfi-quasi-atom (cdr l))))


(defun rfi-special-symbolp (x)
  (and (not (stringp x)) (not (equal (princ-to-string x) (prin1-to-string x)))))






; -----------------------------------------------------------------------------
;
; exit to lisp
;
; -----------------------------------------------------------------------------

(defun rfi-cmd-bye ()
  (cond ;((rfi-script-mode-p)
	 ;(rf-error "(rfi-cmd-bye): " "script running!")
	 ;nil)
	;((rfi-batch-mode-p)
	 ;(rf-error "(rfi-cmd-bye): " "batchjob running!")
	 ;nil)
	(t)))





; -----------------------------------------------------------------------------
;
; script-function
;
; -----------------------------------------------------------------------------

(defun rfi-cmd-startscript (userline)
  (let ((filename (rfi-extension (cadr userline) ".script")))
    (cond ((rfi-script-mode-p)
           (rf-error "(rfi-cmd-startscript): script already running!"))
          ((probe-file filename)
           (if (rfi-yes-or-no-p (concatenate 'string filename " file already exists - overwrite? "))
               (progn (delete-file filename)
                      (rfi-cmd-startscript-1 filename))))
          (t
           (rfi-cmd-startscript-1 filename)))))



(defun rfi-cmd-startscript-1 (filename)
  (setq *rfi-script-output* (open filename :direction :output :if-exists :supersede)))



(defun rfi-cmd-endscript ()
  (if (not (rfi-script-mode-p))
      (rf-error "(rfi-cmd-endscript): no script running!")
      (rfi-cmd-endscript-1)))

(defun rfi-cmd-endscript-1 ()
  (let ((name (truename *rfi-script-output*)))
    (close *rfi-script-output*)
    (setq *rfi-script-output* nil)
    (rf-print '|script written to file |) (rf-princ name) (rf-princ '|...|)
    (rf-fresh-line)))





; -----------------------------------------------------------------------------
;
; execute-function
;
; -----------------------------------------------------------------------------
(defun rfi-cmd-execute (userline)
  (let ((infile (rfi-extension (cadr userline) ".bat")))
    (cond ;((rfi-batch-mode-p)
           ;(rf-error "(rfi-cmd-execute): batch already running!"))
          ((not (probe-file infile))
           (rf-error "(rfi-cmd-execute): " infile " batchfile doesn't exist!"))
          (t
           (rfi-cmd-execute-1 infile)))))



(defun rfi-cmd-execute-1 (infile)
  ;(rf-print '|batchjob executing...|)
  ;(rf-fresh-line)
  (relfun (open infile :direction :input) 'batch)
  ;(setq *rfi-script-input* (open infile :direction :input))
  ;(rfi-set-batch-mode)
  )


;(defun rfi-cmd-endexecute ()
;  (close *rfi-script-input*)
;  (setq *rfi-script-input*  nil)
;  (rfi-set-interactive-mode)
;  (rf-print '|batchjob done...|)
;  (rf-fresh-line))






; -----------------------------------------------------------------------------
;
; help-function
;
; -----------------------------------------------------------------------------

(defun rfi-cmd-help (userline)
  (if (= 1 (length userline))
      (rfi-cmd-help-1 "help.tex")                  ; general help text
      (rfi-cmd-help-1 (string (cadr userline)))))  ; specific help text

(defun rfi-cmd-help-1 (file)
  (let ((filename (concatenate 'string *rfi-help-dir*
                               (string-downcase (rfi-extension file ".tex")))))
       (if (probe-file filename)
           (rfi-cmd-help-3 filename)
           (rf-print '|sorry... no help available!|))))


(defun rfi-cmd-help-3 (file)
  (with-open-file (ifile file :direction :input)
        (do ((line (read-line ifile nil nil nil)
             (read-line ifile nil nil nil)))
            ((null line))
            (rfi-cmd-help-4 line)
            (terpri))))

(defun rfi-cmd-help-4 (line)
  (remove-char (delete #\} (delete #\$ line)) '(#\{) ))

(defun remove-char (line chars)
  (cond ((null chars) (remove-cmd line))
        (t (let ((pos (position (car chars) line)))
                (cond ((not pos) (remove-char line (cdr chars)))
                      (t (remove-char (subseq line 0 pos) (cdr chars))
                         (remove-char (subseq line (1+ pos)) chars))))))
)

(defun remove-cmd (line &aux (pos (position #\\ line)))
    (cond ((and pos (> pos 0))
           (rf-princ (intern (subseq line 0 pos)))
           (remove-cmd (subseq line pos)))
          ((and pos (or (string= line "\\str") (string= line "\\stmt"))))
          (t (rf-princ (intern (delete #\\ line))))))



; -----------------------------------------------------------------------------
;
; calling emulator-functions
;
; -----------------------------------------------------------------------------
(defun rfi-cmd-emul ()
  (if (emulator-available-p) (rfi-set-emulator-mode)))


; command is meaningful only if emulate is defined (emulator is loaded)
(defun emulator-available-p ()
  (cond ((fboundp 'emulate) t)
	(t (rf-error "(rfi-cmd-emul): emulator not available!")
	   nil)))


(defun rfi-cmd-inter ()
  (rfi-set-interpreter-mode))


(defun rfi-cmd-listclass (userline)          
  (if (emulator-available-p) 
      (let ((*style* 'lisp))        ; always use Lisp-syntax
	(list-class (cadr userline)))))



(defun rfi-cmd-listcode (userline)
  (if (emulator-available-p) 
      (let ((*style* 'lisp))        ; always use Lisp-syntax
	(list-code (cadr userline)))))



(defun rfi-cmd-compile (userline)
  (if (emulator-available-p) 
      (loco (select-clauses-of (cadr userline)))))


(defun select-clauses-of (procedurename)
  (cond ((null procedurename) *rfi-database*)
	(t (select-clauses-of-1 procedurename *rfi-database*))))
(defun select-clauses-of-1 (procedurename db)
  (cond ((null db) nil)
	((eq procedurename (car (s-conclusion (car db))))
	 (cons (car db) (select-clauses-of-1 procedurename (cdr db))))
	(t (select-clauses-of-1 procedurename (cdr db)))))




; -----------------------------------------------------------------------------
;
; listing-function
;
; -----------------------------------------------------------------------------
(defun rfi-cmd-l (x db)
  (cond ((null x) (rf-pprint-db db))
        ((atom x) (rfi-cmd-l-1 (mk-pairpattern x 'id) db))
	(t (rfi-cmd-l-1 x db))))

(defun rfi-cmd-l-1 (pattern database)
  (let* ((clause (car database))
	 (head (subst (gensym)                 ; one-sided unification
		      'vari                    ; simulated with gensym-terms
		      (s-conclusion clause))))
    (cond ((null database) t)
	  ((unify pattern head '((bottom)))
	   (rf-pprint-clause clause)
	   (rfi-cmd-l-1 pattern (cdr database)))
	  (t 
	   (rfi-cmd-l-1 pattern (cdr database))))))





; -----------------------------------------------------------------------------
;
; footen-function
;
; -----------------------------------------------------------------------------
(defun footen-database (db)
  (mapcar #'footen-clause db))

(defun footen-clause (clause)
  (if (and (eq 'hn (s-kind clause)) (s-premises clause))
      (cons 'ft (cons (s-head clause) (append (s-premises clause) '(true))))
      clause))





; -----------------------------------------------------------------------------
;
; footer-function
;
; -----------------------------------------------------------------------------
(defun footer-database (db)
  (mapcar #'footer-clause db))

(defun footer-clause (clause)
  (if (eq 'hn (s-kind clause))
      (cons 'ft (cons (s-head clause) (append (s-premises clause) '(true))))
      clause))





; -----------------------------------------------------------------------------
;
; preparing the query for the emulator
;
; - generate something like (ft (main _x1 .._xn) (p1 ...) ... (pm ...))
; - bindings of _x1 through _xn should be displayed  
; - database and query must be "flatter" for emulator
; - inst is removed because not active structures are allowed and so everyting 
;   is passiv
;
; -----------------------------------------------------------------------------
(defun transform-query-for-emulator (userline)
 (cdr (deanon-clause (flatten-struc-clause (cons 'ft (tuptocons1 (maingen (uncomma/body (hotrans/body (unlambda/body (unor/body (unmacro/body (untype/body userline)))))))))))))


;; (flatten-struc-clause (maingen (tuptocons1 userline))))
;; (de-inst userline)))))

;   2       1448: 18 Sep 90 Hans-Guenther H main-gen fertig
; (Message # 2: 1448 bytes)
; Date:     Tue, 18 Sep 90 21:23:47 MET DST
; From:     Hans-Guenther Hein <hein@informatik.uni-kl.de>
; To:       scheu@informatik.uni-kl.de
; Subject:  main-gen fertig
; Message-ID:  <9009182123.aa14820@inti.informatik.uni-kl.de>
; Dafuer hab' ich schnell mal eine Funktion geschrieben,
; die aus einer Anfrage die Variablen raussucht und einen Mainkopf generiert.
;;; maingen.lsp
;;;
;;;

(defun maingen (callist)
 (cons (cons 'main
              (delete-duplicates (finduservars callist) :test #'equal)
       )
  callist
 )
)

(defun finduservars (callist)
 (cond ((null callist) nil)
       ((atom callist) nil)
       ((vari-t callist) (list callist))
       (t (append (finduservars (car callist))
                  (finduservars (cdr callist))
          )
       )
 )
)

;
; Dies sieht folgendermassen aus:
;
; (rf-print (maingen '((likes (vari x) (vari y)) (likes (vari x) (vari ui))))))
; ((main (vari y) (vari x) (vari ui)) (likes (vari x) (vari y))
;  (likes (vari x) (vari ui)))
; ((MAIN (VARI Y) (VARI X) (VARI UI)) (LIKES (VARI X) (VARI Y))
;  (LIKES (VARI X) (VARI UI)))
;
; Jetzt waere nur noch abzuflachen und was weiss ich yu machen, um
; (emulate '<main var1 var2 .. varn><aufruf1 ..> .. <aufruf m>) zu callen.
;	Hansi
;





; -----------------------------------------------------------------------------
;
; access functions for variables
;
; -----------------------------------------------------------------------------



(defun level-of (var)
  (cond ((null (cddr var)) nil)
	(T (caddr var))))



(defun name-of (var) 	
  (read-from-string (symbol-name (cadr var))))



(defun variable-name (variableterm)
  (intern 
   (concatenate 'string 
		"_"
		(princ-to-string (name-of variableterm))
		(if (level-of variableterm) 
		    (concatenate 'string 
				 ":" 
				 (princ-to-string (level-of variableterm)))
		    ""))))





; -----------------------------------------------------------------------------
;
; access and test functions for rf-lists
;
; -----------------------------------------------------------------------------
(defun mk-pairpattern (x y)
  (cons x y))                 ; this may change if bars are used instead of dots





; -----------------------------------------------------------------------------
;
; calling lisp-functions from rfi
;
; -----------------------------------------------------------------------------
(defun lisp-exec (unisgoal environ)
  (let ((ultinstargs (ultimate-instant (de-inst (cdr unisgoal)) environ)))
    (cond ((lisp-extra-p (car unisgoal))
	   (mk-inst (apply (car unisgoal) ultinstargs)))
	  (t (list-to-tup-false (apply (car unisgoal)
				       (tup-to-listargs ultinstargs))
				(lisp-predicate-p (car unisgoal)))))))



(defun lisp-builtin-p (operator)
  (or (lisp-function-p operator) 
      (lisp-predicate-p operator) 
      (lisp-extra-p operator)))


(defun lisp-function-p  (operator) (member operator *lisp-functions*))
(defun lisp-predicate-p (operator) (member operator *lisp-predicates*))
(defun lisp-extra-p     (operator) (member operator *lisp-extras*))

;-------------- ;(Klaus 30.09.1990)

(defun relfun-exec (unisgoal environ)
  (list-to-value-false
   (apply (car unisgoal)
	  (ultimate-instant (cdr unisgoal) environ))
   t))

(defun list-to-value-false (x nil-to-false)
  (cond ((and (null x) nil-to-false) 'false)
	(t (mk-inst x))))


(defun relfun-builtin-p (operator)
  (or (relfun-extra-p operator)))


(defun atom-e (x)
"true, if only a Relfun atom and not nil"
  (and x (atom x)))

(defun not-atom-e (x)
  (not (atom x)))

(defun vari-t-e (x)
  (vari-t x))

(defun not-vari-t-e (x)
  (not (vari-t x)))

(defun relfun-extra-p   (operator) (member operator *relfun-extras*))



; Transforming between RELFUN tup/cns and LISP list/cons nestings
; (LISP builtins accept no non-tup/non-cns structures on any level)


(defun tup-to-listargs(l) 
  (mapcar #'tup-to-list l) )


(defun tup-to-list (x)
  (cond ((atom x) x)
	((tup-t x) (mapcar #'tup-to-list (cdr x)))
	((cns-t x) (cons (tup-to-list (cadr x)) (tup-to-list (caddr x))))
	(t (rf-error "(tup-to-list): "
		     (format nil "~A" x)
		     " structure or free variable can't be arg to LISP builtin"))))


(defun list-to-tup-false (x nil-to-false)
  (cond ((and (null x) nil-to-false) 'false)
	((and (eq t x) nil-to-false) 'true)
	(t (mk-inst (list-to-tup x)))))


(defun list-to-tup (x)
  (cond ((null x) '(tup))
	((atom x) x)
	(t (cons 'tup (mapcar #'list-to-tup x)))))








; -----------------------------------------------------------------------------
; 
; changing the copy of lisp-readtable to accept rfi-syntax
;
; -----------------------------------------------------------------------------

;; Prolog-people like var-names with underscores (e.g. a_long_name),
;; but the current version of rfi.lsp when in Lisp-style can not read
;; the corresponding name (_a_long_name):

(defun rfi-set-syntax nil
  ;; third arg. of set-macro-character:
  ;;   nil ==> terminating macro-character
  ;;   t   ==> non-terminating macro-character
  (set-macro-character #\_ 'underscore-reader t   *rfi-readtable*)
  (set-macro-character #\` 'backquote-reader  nil *rfi-readtable*)
  (set-macro-character #\, 'comma-reader      nil *rfi-readtable*)
  (set-macro-character #\: 'colon-reader      nil *rfi-readtable*)
  (set-macro-character #\@ 'ecal-reader       nil *rfi-readtable*)
;  (set-macro-character #\! 'cut-reader        nil *rfi-readtable*)
  (set-macro-character #\| 'bar-reader        nil *rfi-readtable*))



(defun underscore-reader (stream char)
  (declare (ignore char))
  (let ((x (read stream t nil t)))
    (if (numberp x)
	(list 'vari (intern (princ-to-string x)))
	(list 'vari x))))

(defun backquote-reader (stream char)
  (declare (ignore char))
  (list 'inst (read stream t nil t)))

(defun ecal-reader (stream char)
  (declare (ignore char))
  (list 'ecal (read stream t nil t)))

(defun cut-reader (stream char)
  (declare (ignore char))
  (list 'cut (read stream t nil t)))

(defun bar-reader (stream char)
  (declare (ignore stream char))
  '\|)

(defun comma-reader (stream char)
  (declare (ignore char))
  (list 'uninst (read stream t nil t)))


(defun colon-reader (stream char) ; bad hack !!
  (declare (ignore char))
  (let ((next-char (read-char stream t nil t)))
       (cond ((and (graphic-char-p next-char) (char/= next-char #\ ))
	      (unread-char next-char stream)
	      (car (multiple-value-list 
		    (read-from-string-std 
		     (format nil ":~a" (read stream t nil t))))))
	     (T '\:))))

(defun read-from-string-std (string)
  (let ((*readtable* (copy-readtable nil)))
       (read-from-string string)))



; normalen Syntax wiederherstellen
(defun rfi-reset-syntax nil
  (setq *readtable* (copy-readtable nil)))



; -----------------------------------------------------------------------------
; 
; input-function for rfi
;
; adapted from Lisplog file "specials.commonlisp.l"
;
; -----------------------------------------------------------------------------

; The caller of readl detects no difference between the two syntaxes,
; because readl always returns an expression in Lisp-like syntax.
; This is the most important point to interface pro2lisp.lsp:

(defun readl nil
; Reads input from the user.
; If prolog-style is active then the input is transformed to 
; lisp-syntax.
  (let ((usercmd nil))
    (loop
     (setq usercmd (rf-reader))
     (cond ((equal usercmd ""))         ; read again
	   ((eq *style* 'lisp)
	    (return (transform-string-to-single-lisp-object usercmd)) )
					; style is prolog:
	   (t 
	    (let* ((pair (pro-split-input usercmd))
		   (rfi-cmd-as-symbol (car pair))
		   (rest-of-input-as-string (cdr pair))
		   (rfi-cmd-p (member rfi-cmd-as-symbol
				      *rfi-commands* 
				      :test #'equal )) )
	      (cond ((null pair))  ; scanner signals error, read again
		    (rfi-cmd-p
		     (return (handle-rfi-cmd rfi-cmd-as-symbol
					     rest-of-input-as-string
					     usercmd )))
		    ;; transform Prolog-syntax to Lisp-syntax:
		    (t
		     (let ((input-as-list (pro-read-goal usercmd)))
		       (if (not (null input-as-list))  ; error at transform. ?
			   (return input-as-list) ))))))))))  ; no
  



(defun transform-string-to-single-lisp-object (s)          ; guess (or look at the name)
  (let ((*readtable* *rfi-readtable*))
    (read-from-string (concatenate 'string "(" s ")"))))



(defun rf-reader () ; returns a string.
; If prolog-style then multiple lines are separated by #\Newline.
  (do* ((line (rf-readline t) (rf-readline nil))
	(cmd  line            (if (and (string/= "" cmd)
				       (eq 'prolog *style*) )
				  (concatenate 'string 
					       cmd (string #\Newline) line ) 
				(concatenate 'string cmd line) )))
       ((complete-cmd-p cmd) cmd)))




(defun rf-readline (firsttime)
  (remove-remarks (rf-readline-1 firsttime)))

(defun rf-readline-1 (firsttime)
  (cond ;
        ; normal input
        ; 
        ((and (not (rfi-batch-mode-p)) (not (rfi-script-mode-p)))
	 (read-from-terminal firsttime))
        ;
        ; protocoll to script-file
        ;
	((and (not (rfi-batch-mode-p))      (rfi-script-mode-p))
	 (let ((inputline (read-from-terminal firsttime)))
	   (cond ((and firsttime (equal inputline "endscript"))
		  (rfi-cmd-endscript)
		  "")
		 (t
		  (if firsttime (progn (fresh-line *rfi-script-output*)
				       (princ *rfi-prompt* *rfi-script-output*)))
		  (princ inputline *rfi-script-output*)
		  (terpri *rfi-script-output*)
		  inputline))))
        ;
        ; get input form batch-file
        ; 
	((and     (rfi-batch-mode-p)   (not (rfi-script-mode-p)))
	 (let ((inputline (read-from-batch-file)))
	   (cond ((and firsttime (equal inputline "end-of-batch-file"))
		  ;(rfi-cmd-endexecute)
		  "bye")
		 (t
		  (if firsttime (progn (fresh-line *rfi-standard-output*)
				       (princ *rfi-prompt* *rfi-standard-output*)))
		  (princ inputline *rfi-standard-output*)
		  (terpri *rfi-standard-output*)
		  inputline))))
	;
	; get input from batch-file and protocoll to script-file
        ; 
	((and     (rfi-batch-mode-p)        (rfi-script-mode-p))
	 (let ((inputline (read-from-batch-file)))
	   (cond (firsttime
		  (cond ((equal inputline "end-of-batch-file")
			 ;(rfi-cmd-endexecute)
			 "bye")
			((equal inputline "endscript")
			 (fresh-line *rfi-standard-output*)
			 (princ *rfi-prompt* *rfi-standard-output*)
			 (princ inputline *rfi-standard-output*)
			 (terpri *rfi-standard-output*)
			 (rfi-cmd-endscript)
			 "")
			(t
			 (fresh-line *rfi-standard-output*)
			 (fresh-line *rfi-script-output*)
			 (princ *rfi-prompt* *rfi-standard-output*)
			 (princ *rfi-prompt* *rfi-script-output*)
			 (princ inputline *rfi-standard-output*)
			 (princ inputline *rfi-script-output*)
			 (terpri *rfi-standard-output*)
			 (terpri *rfi-script-output*)
			 inputline)))
		 (t
		  (princ inputline *rfi-standard-output*)
		  (princ inputline *rfi-script-output*)
		  (terpri *rfi-standard-output*)
		  (terpri *rfi-script-output*)
		  inputline))))))



(defun read-from-batch-file ()
  (read-line *rfi-script-input* nil "end-of-batch-file" nil))

(defun read-from-terminal (firsttime)
  (if firsttime (progn (fresh-line *rfi-standard-output*) 
		       (princ *rfi-prompt* *rfi-standard-output*))
                (princ "       ") )   ; continuation indicator
  (read-line *rfi-standard-input*))




; In Prolog-style commas and some other characters signal an uncompleted input.

(defun complete-cmd-p (x)
  (if (eq *style* 'prolog)
      (and (pro-complete-cmd-p x)
	   (= (count #\( x) (count #\) x)) )
    (= (count #\( x) (count #\) x)) ))

(defun pro-complete-cmd-p (cmd-as-str)
  ; Commas and some other characters indicate an uncompleted input.
  (if (= 0 (length cmd-as-str))
      nil  ; continue reading
    (do* ((i (1- (length cmd-as-str)) (1- i))
	  (ch (char cmd-as-str i) (char cmd-as-str i))
	  (cont-chars (list #\, #\& #\| #\-)) )
	 ;; find non-whitespace:
	 ((or (case (char cmd-as-str i)
		    ((#\Space #\Newline #\Tab)
		     nil )
		    (t t) )
	      (= i 0) )
	  (not (member ch cont-chars :test #'char=)) ))))



(defun rfi-yes-or-no-p (x)
  (if (rfi-batch-mode-p) t (yes-or-no-p x)))




; Comment characters are style-dependent:

(defun remove-remarks (x)           ; Remark starts with a ";" or "%"
  (let* ((begin-of-comment-char (if (eq 'prolog *style*)
				    #\%
				  #\; ))
	 (p (position begin-of-comment-char x)))
    (if p (subseq x 0 p) x)))






; -----------------------------------------------------------------------------
;
; untup function
; something like `(tup a b | _x) is transformed to `(cns a (cns b _x))
; this means:
;   - only passive tup structures are transformed 
;     (i.e. tups in the head and instantiated tups in the body
;           and on the left-hand side of is)
;   
;
; -----------------------------------------------------------------------------

(defun untup-database (db)
;  (mapcar #'untup-clause db))
  (cond ((null db) nil)
	((equal '(tup) (s-head (car db))) (untup-database (cdr db))) ; del (tup) def
	(t (cons (untup-clause (car db)) (untup-database (cdr db))))))


(defun untup-clause (clause)                       (tuptocons1 clause))
;  (cons (s-kind clause) 
;	(cons (if (equal '(tup) (s-head clause))
;		  '(tup)                            ; leaving (tup) as is
;		(tuptocons1 (s-head clause)))
;	      (tuptocons-passive (s-premises clause)))))


;(defun tuptocons-passive (x)
;  (cond ((is-t x)
;	  (mk-is (tuptocons1 (s-patt-is x)) (tuptocons-passive (s-expr-is x))))
;	((inst-t x) (mk-inst (tuptocons1 (un-inst x))))
;	((final-p x) x)
;	(t (cons (tuptocons-passive (car x)) (tuptocons-passive (cdr x))))))


(defun consit (lis)
 (cond ((null lis) nil)

;------------ throw out if and when dotted-tup untupping becomes forbidden-----
       ((atom lis) lis)
       ((vari-t lis) lis)
;------------------------------------------------------------------------------

       ((bar-t lis) (tuptocons1 (cadr lis)))  ; cddr should be nil
       (t (list 'cns (tuptocons1 (car lis)) (consit (cdr lis))))
 )
)


(defun tuptocons (item)
 (if (tup-t item) (tuptocons1 item) (rf-error "(tuptocons): tup expected"))
)

(defun tuptocons1 (item)
 (cond ((null item) nil)
       ((inst-t item) (mk-inst (tuptocons1 (un-inst item))))
       ((final-p item) item)
;       ((atom item) item)
;       ((vari-t item) item)
       ((tup-t item)
	(cond ((bar-t (cdr item)) (rf-error "(tuptocons1): tup with leading | not untuppable"))
	      (t (consit (cdr item)))))
       (t (cons (tuptocons1 (car item)) (tuptocons1 (cdr item))))
 )
)



; ---------------------------------------------------------------------------
; 
; define length of spy-trace: showdepth
;
; ---------------------------------------------------------------------------

(defun rfi-cmd-showdepth (userline)
  (cond ((cadr userline)
	 (if (numberp (cadr userline))
	     (setq *rfi-showdepth* (cadr userline))
	     (rf-error "(rfi-cmd-showdepth): "
		       (format nil "~A" (cadr userline))
		       " a number is needed!")))
	(t
	 (rf-print '|actual showdepth: |) (rf-princ *rfi-showdepth*))))





; -----------------------------------------------------------------------------
;
; some useful functions for debugging
;
; -----------------------------------------------------------------------------
(defun rs nil 
  (setq *readtable* (copy-readtable nil))
  (princ "The readtable is now restored form standard commonlisp")
  (terpri))
(defun lr nil (load "rfi.lsp" :print :t))



; -----------------------------------------------------------------------------
;
; initializations
;
; -----------------------------------------------------------------------------
(setq *print-length* 100)
(setq *print-level* 100)

(rfi-init) ; used for both relfun and rf



; -----------------------------------------------------------------------------
;
; COMMON LISP extending utilities
;
; -----------------------------------------------------------------------------
;(defun ncons (x) (cons x nil))



; -----------------------------------------------------------------------------
;
; print start message...
;
; -----------------------------------------------------------------------------
(terpri)
(princ "RFI - ")
(princ *rfi-version*)
(terpri)
(princ "   - functions improved:")
(terpri)
(princ "      - readl")
(terpri)
(princ "      - consult/replace")
(terpri)
(princ "      - lconsult/lreplace")
(terpri)
(princ "      - batch-command")
(terpri)
(princ "      - script-command")
(terpri)
(princ "      - listclass")
(terpri)
(princ "      - transform-query-for-emulator")
(terpri)
(princ "      - footen-command")
(terpri)
(princ "   - new functions:")
(terpri)
(princ "      - footer")
(terpri)
(princ "      - showdepth-command")
(terpri)
(princ "   - pretty-print-functions moved to *lisp-extras*")
(terpri)
(princ "   - *rfi-readtable* is modified instead of lisp *readtable*")
(terpri)
(princ "   - spy/nospy also affects compiler/emulator")
(terpri)
(princ "   - consult/replace works for files with multiple setq or with untagged clauses")
(terpri)
(princ "   - lconsult/lreplace works also with untagged clauses or expanded macros")
(terpri)
(princ "   - bar is legalized")
(terpri)
(princ "   - query for emulator is transformed to special footed-procedure 'main'")
(terpri)
(princ "   - footer: change radically hn- to ft-clauses")
(terpri)
(princ "   - showdepth: length spy-trace is displayed with stars")
(terpri)
(princ "   - PLEASE NOTE: It's enough to say compile (does horizon & verti)")
(terpri)
(princ "   - call for meta-calls is generalized and renamed to ecal (eval+call)")
(terpri)
(princ "   - spy shows backquotes of structures bound to variables etc.")
(terpri)
(princ "   - horizon/compile: flatter, normalize, footen; mode-interpreter interface")
(terpri)
(princ "   - once, naf, toplevel renamed to relfun, interface function rf")
(terpri)
(princ "   - initial cut generalized to single cut: (hn (foo _x) ... ! ...)")
(terpri)
(princ "   - bagof-like tupof: (tupof (likes john _x) `(ok _x)) RETURNS tup of (ok _x)")
(terpri)
(princ "   - access function rf complies to COLAB specification")
(terpri)
(princ "   - command deanon for databases and automatic deanonymization of requests")
(terpri)
(princ "   - COLAB partners fw, cn, tx are NOW ACCESSIBLE AS LISP ***FUNCTIONS***")
(terpri)
(princ "   - error handling improved and rf-pprint also returns its arg")
(terpri)
(princ "   - command relationalize (flatten + extrarg) puts return into FIRST arg")
(terpri)
(princ "   - command singlify transforms multi-footed clauses to single-footed ones")
(terpri)
(princ "   - commands builtins changed, version introduced")
(terpri)
(princ "   - flatter now treats complex is lhs")
(terpri)
(princ "   - database is LIST of lists, or-process has db-left/db-right args, prelude")
(terpri)
(princ "   - cns is interpreted as LISP cons, normalize command, rf-terpri returns t")
(terpri)
(princ "   - (foot-)sole cut returns proper value: (ft (foo _x) ... v !) => v'")
(terpri)
(princ "   - PROLOG-style syntax can be loaded; static-flattener uses mk-inst")
(terpri)
(princ "   - relfun made recursive for interaction breaks in batch; lisp renamed bye")
(terpri)
(princ "   - exec made recursive for recursive batch files (cf. recursive includes)")
(terpri)
(princ "   - value-returning clause primitive (clause `(tag head | body))")
(terpri)
(princ "   - integrated: PROLOG-style syntax and new horizontals (uncomma ...)")
(terpri)
(princ "   - date/time-returning date builtin (date); rfi-predicates -> operators")
(terpri)
(princ "   - report errors to boley@informatik.uni-kl.de")
(terpri)
(princ "   - start by typing (relfun)")
(terpri)
(terpri)


; Indicate that the RELFUN interpreter was loaded successfully to others
; in the same LISP image:
(pushnew :relfun *features*) ; this should stay the last expression in the file
