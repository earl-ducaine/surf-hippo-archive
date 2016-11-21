; COLAB Interface for New-RFM (with GAMA)         (c)  M.S. 10/92
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| **************************************************************************
   relfun specific primitives 
   *********************************************************************** |#

;(defvar *hn-rulebase*		nil "hn-Version of bidirctional rules")
;(defvar *rule-database*		nil "List of bidirctional rules")
;(defvar *factbase*		nil "List of hybrid ground facts")

(setq *col-list-of-relfun* ; List of commands, which relfun can interprete
  '((a0		"" nil colab-relfun-a0		nil "assert a clause to the beginning of the database")
    (a0ft	"" nil colab-relfun-a0ft	nil "assert a footed clause to the beginning of the database")
    (a0hn	"" nil colab-relfun-a0hn	nil "assert a horn clause to the beginning of the database")
    (az		"" nil colab-relfun-az		nil "assert a clause to the end of the database")
    (azft	"" nil colab-relfun-azft	nil "assert a footed clause to the end of the database")
    (azhn	"" nil colab-relfun-azhn	nil "assert a horn clause to the end of the database")
    (builtins	"" nil colab-relfun-builtins	nil "list relfun builtins")
    (modes	"" nil colab-relfun-modes	nil "")
    (horizon	"" nil colab-relfun-horizon	nil "compile horizontally")
    (verti	"" nil colab-relfun-verti	nil "compile vertically")
    (compile	"" nil colab-relfun-compile	nil "compile relfun database")
    (deanon	"" nil colab-relfun-deanon	nil "")
    (destroy	"" nil colab-relfun-destroy	nil "destroy the relfun database")
    (dynamic	"" nil colab-relfun-dynamic	nil "")
    ;(exec	"" nil col-cmd-execute		nil "")
    (extrarg	"" nil colab-relfun-extrarg	nil "")
    (flatten	"" nil colab-relfun-flatten	nil "flatten the relfun database")
    (flatter	"" nil colab-relfun-flatter	nil "")
    (footen	"" nil colab-relfun-footen	nil "")
    (footer	"" nil colab-relfun-footer	nil "")
    (listing	"" nil colab-list-relfun	nil "list only the relfun database")
    (listclass	"" nil colab-relfun-listclass	nil "list classified clauses")
    (listcode	"" nil colab-relfun-listcode	nil "list compiled code")
    (m		"" nil colab-relfun-m		nil "same as more")
    (more	"" nil colab-relfun-more	nil "more solutions")
    (nospy	"" nil colab-relfun-nospy	nil "deactivate spy mechanism")
    (prelude	"" nil colab-relfun-prelude	nil "")
    (relationalize "" nil colab-relfun-relationalize	nil "")
    (rx		"" nil colab-relfun-rx		nil "remove clause from the database")
    (rxft	"" nil colab-relfun-rxft	nil "remove footed clause from the database")
    (rxhn	"" nil colab-relfun-rxhn	nil "remove horn clause from the database")
    (script	"" nil colab-relfun-script	nil "take a script from all input and output")
    (singlify	"" nil colab-relfun-singlify	nil "")
    (showdepth	"" nil colab-relfun-showdepth	nil "show or modify length of spy mechanism")
    (spy	"" nil colab-relfun-spy		nil "activate spy mechanism")
    (static	"" nil colab-relfun-static	nil "")
    (tell	"" nil colab-relfun-tell	nil "")
    (untup	"" nil colab-relfun-untup	nil "untup the database")
    (version	"" nil colab-relfun-version	nil "print a version of relfun")

    ; new GAMA specific commands (and some others):

    
    (style	"" nil colab-relfun-style	nil "switch to prolog or lisp style")
    (asm	"" nil colab-relfun-assem	nil "assembler command invocation")
    (assem	"" nil colab-relfun-assem	nil "assembler command invocation")
    )
)



#|
(defun colab-relfun-a0 (userline)
  (setq *rfi-database* (append userline *rfi-database*)))

(defun colab-relfun-a0ft (userline)
  (setq *rfi-database* (cons (cons 'ft userline) *rfi-database*)))

(defun colab-relfun-a0hn (userline)
  (setq *rfi-database* (cons (cons 'hn userline) *rfi-database*)))

(defun colab-relfun-az (userline)
  (setq *rfi-database* (append *rfi-database* userline)))

(defun colab-relfun-azft (userline)
 (setq *rfi-database* (append *rfi-database* (list (cons 'ft userline)))))

(defun colab-relfun-azhn (userline)
 (setq *rfi-database* (append *rfi-database* (list (cons 'hn userline)))))

(defun colab-relfun-builtins (userline)
  (declare (ignore userline))
  (rf-print (cons 'functions *lisp-functions*))
  (rf-print (cons 'predicates *lisp-predicates*))
  (rf-print (cons 'extras *lisp-extras*)))

|#

(defun colab-relfun-horizon (userline)
  (declare (ignore userline))
  (setq *rfi-database* (horizon-database *rfi-database*))
  ;(setq *hn-rulebase* (horizon-database *hn-rulebase*))
  )

(defun colab-relfun-verti (userline)
  (colab-rfi-cmd-compile (cons 'verti userline)))

(defun colab-relfun-compile (userline)
  (setq *rfi-database* (horizon-database *rfi-database*))
  ;(setq *hn-rulebase* (horizon-database *hn-rulebase*))
  ;(setq *factbase* (horizon-database *factbase*))
  (colab-rfi-cmd-compile (cons 'compile userline)))

(defun colab-rfi-cmd-compile (userline)
  (when (emulator-available-p)
	(loco (colab-select-clauses-of (cadr userline)))
	(gwam.assem)))

(defun colab-select-clauses-of (procedurename)
  ; (let ((database (append *rfi-database* *hn-rulebase* *factbase*))))
  ; consider rfi-database only !!!
  (let ((database *rfi-database*))
  (cond ((null procedurename) database)
        (t (colab-select-clauses-of-1 procedurename database)))))

(defun colab-select-clauses-of-1 (procedurename db)
  (cond ((null db) nil)
        ((eq procedurename (car (s-conclusion (car db))))
         (cons (car db) (colab-select-clauses-of-1 procedurename (cdr db))))
        (t (colab-select-clauses-of-1 procedurename (cdr db)))))

#|


(defun colab-relfun-consult (userline)
  (rfi-cmd-consult (cons 'consult userline)))

(defun colab-relfun-deanon (userline)
  (declare (ignore userline))
  (setq *rfi-database* (deanon-database *rfi-database*))
  (setq *hn-rulebase* (deanon-database *hn-rulebase*)))

(defun colab-relfun-destroy (userline)
  (declare (ignore userline))
  (setq *rfi-database* nil))

(defun colab-relfun-dynamic (userline)
  (declare (ignore userline))
  (setq *rfi-static* nil))

(defun colab-relfun-extrarg (userline)
  (declare (ignore userline))
  (setq *rfi-database* (extrarg-database *rfi-database*)))

(defun colab-relfun-flatten (userline)
  (declare (ignore userline))
  (setq *rfi-database* (flatten-database *rfi-database*)))

(defun colab-relfun-flatter (userline)
  (declare (ignore userline))
  (setq *rfi-database* (flatten-struc-database *rfi-database*)))

(defun colab-relfun-footen (userline)
  (declare (ignore userline))
  (setq *rfi-database* (footen-database *rfi-database*)))

(defun colab-relfun-footer (userline)
  (declare (ignore userline))
  (setq *rfi-database* (footer-database *rfi-database*)))

(defun colab-relfun-listclass (userline)
  (rfi-cmd-listclass (cons 'listclass userline)))

(defun colab-relfun-listcode (userline)
  (rfi-cmd-listcode (cons 'listcode userline)))

(defun colab-relfun-m (userline)
  (declare (ignore userline))
  (rf-print '|unknown|))

(defun colab-relfun-modes (userline)
  (rfi-modes (cons 'modes userline)))

(defun colab-relfun-more (userline)
  (declare (ignore userline))
  (rf-print '|unknown|))

(defun colab-relfun-prelude (userline)
  (rfi-cmd-l (car userline) *rfi-prelude*))

(defun colab-relfun-nospy (userline)
  (declare (ignore userline))
  (setq *rfi-spy* nil) (setq *emu-debug* nil))

(defun colab-relfun-relationalize (userline)
  (declare (ignore userline))
  (setq *rfi-database* (extrarg-database (flatten-database *rfi-database*))))

(defun colab-relfun-replace (userline)
  (rfi-cmd-replace (cons 'replace userline)))

(defun colab-relfun-rx (userline)
  (setq *rfi-database* (remove (car userline) *rfi-database*
			       :test #'equal)))

(defun colab-relfun-rxft (userline)
  (setq *rfi-database* (remove (cons 'ft userline) *rfi-database*
			       :test #'equal)))

(defun colab-relfun-rxhn (userline)
  (setq *rfi-database* (remove (cons 'hn userline) *rfi-database*
			       :test #'equal)))

(defun colab-relfun-script (userline)
  (rfi-cmd-startscript (cons 'script userline)))

(defun colab-relfun-showdepth (userline)
  (rfi-cmd-showdepth (cons 'showdepth userline)))

(defun colab-relfun-singlify (userline)
  (declare (ignore userline))
  (setq *rfi-database* (singlify-database *rfi-database*)))

(defun colab-relfun-spy (userline)
  (declare (ignore userline))
  (setq *rfi-spy* t) (setq *emu-debug* t))

(defun colab-relfun-static (userline)
  (declare (ignore userline))
  (setq *rfi-static* t))

(defun colab-relfun-tell (userline)
  (rfi-cmd-tell (cons 'tell userline)))

(defun colab-relfun-untup (userline)
  (declare (ignore userline))
  (setq *rfi-database* (untup-database *rfi-database*)))

(defun colab-relfun-version (userline)
  (declare (ignore userline))
  (princ *rfi-version*))


(defun colab-consult-relfun (elem)
; if you got problems with your performance uncomment the following line
;  (setq *rfi-database* (nconc *rfi-database* (list elem))))
  (setq *rfi-database* (append *rfi-database* (list elem))))

(defun colab-destroy-relfun (userline)
  (declare (ignore userline))
  (setq *rfi-database* nil))


(defun colab-list-relfun (userline)
  (rfi-cmd-l (car userline) *rfi-database*))

(defun colab-list-relfun-elem (elem)
  (rf-pprint-clause elem))


(defun relfun-else (userline)
  (rf-pprint (apply #'rf userline)) ; M.S.  27 July 92
  nil)


(defun rfi-else (userline)
  (let ((mode (rfi-interpreter-mode-p)))
       (cond (mode ; use interpreter to answer query
	      (cond ((and-process (deanon-request
				   (if *rfi-static*
				       (flatten-request userline) userline))
				  '((bottom))
				  (list *rfi-prelude*
					*rfi-database*
					*hn-rulebase*
					*factbase*)
				  1
				  nil))
		    (t (princ "unknown"))))
              ;;; use emulator to answer query
             (t (emulate (transform-query-for-emulator userline)))))
  nil)

|#


(defun colab-relfun-assem (userline)
  (gwam.assem-cmd userline))


(defun colab-relfun-style (userline)
  (rfi-cmd-style (cons 'style userline)))


(defun col-prompt-print (file)
  (maplist #'(lambda (x)
	       (princ (case (read-from-string (car x))
			    (fw (if *fw-interpreter-mode*
				    "fwi"
				  "fwe") )
			    ((rfi rfi-p rfi-l) (if (eq 'lisp *style*)
						   "rfi-l"
						 "rfi-p" ))
			    ((rfe rfe-p rfe-l)  (if (eq 'lisp *style*)
						    "rfe-l"
						  "rfe-p" ))
			    (t (car x)) ))
	       (if (null (cdr x)) (princ "> ")(princ ",")))
	   (reverse *col-prompt*)))



