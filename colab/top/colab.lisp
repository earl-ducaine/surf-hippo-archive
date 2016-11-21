;;; -*- Mode: LISP; -*-
(defvar *col-tmpdb* nil)
(defun checkfile (filename)
  "check a file, if it contains only function and variable declarations"
  (print "Check variable '*col-tmpdb*' if any error occurs.")
  (with-open-file (file (col-extension filename ".lsp") :direction :input)
		  (do* ((expr (read file nil nil nil) (read file nil nil nil))
			(*tmpdb* nil (cons expr *tmpdb*)))
		       ((null expr) (reverse *tmpdb*))
		       (cond ((not (listp expr)) nil)
			     ((member (car expr) '(defun defmacro defvar)) nil)
			     (t (print expr)))
		   ))
  t
)

;;; COLAB shaell
;;; Autor Klaus Elsbernd
;;; September 1991

#| **************************************************************************
   global variables
   *********************************************************************** |#

(defvar *col-version*		"Nov 15th 1992" "Colab Version")
(defvar *col-prompt*		'() "List of current prompts")
(defvar *col-input-mode*	nil "Batch or not batch? That's the question!.")
(defvar *col-script-input*	nil  "Input stream for typescript           ")
(defvar *col-standard-output*	*standard-output* "output stream ")
(defvar *col-standard-input*	*standard-output*  "input stream ")
(defvar *col-readtable*		nil  "readtable for special symbols ")
; no longer needed, help-directory defined as keyword :col-help
; in start-colab.lsp
;(defvar *col-help-dir*		#+Mac "document:manual:interpreter:"
;				#-Mac "/home/ctec/colab/document/manual/interpreter/"
;				"help directory for help texts")

(defun colab-else (userline)
  "This function will be called, if no command can be found."
  "It will execute the function in *colab-alternate*."
  (format t "colab: invalid user command ~A" userline)
)

(defvar *colab-alternate*
  'colab-else
  "This function will be called, if no other function is specified")

(defvar *col-list-of-systems* 
  '((relfun	"rf"	nil colab-relfun	relfun-else "Relfun Subsystem Switch")
    (rfi   	"rfi"	nil colab-rfi		rfi-else "Relfun Interpreter Subsystem Switch")
    (rfe   	"rfe"	nil colab-rfe		rfi-else "Relfun Emulator Subsystem Switch")
    (forward    "fw"	nil colab-forward	forward-else "Forward Subsystem Switch")
    (taxon	"tx"	nil colab-taxon		taxon-else "Taxon Subsystem Switch")
    (contax	"cn"	nil colab-contax	nil "Contax Subsystem Switch")
    (colab	"colab"	nil colab-colab		colab-else "Colab System Switch")
   )
  "List of systems (command prompt newtab function alternate-function helptext)")

(defmacro col-cmd	(x) `(car ,x))
(defmacro col-prompt	(x) `(second ,x))
(defmacro col-newtab	(x) `(third ,x))
(defmacro col-function	(x) `(fourth ,x))
(defmacro col-else-fct	(x) `(fifth ,x))
(defmacro col-help	(x) `(sixth ,x))

#| **************************************************************************
   global command tables
   *********************************************************************** |#

(defvar *col-default-tab* *col-list-of-systems*
  "Default command level is toplevel")

(defvar *col-list-of-colab* ; List of commands, which colab can interpret
  '((listing "" nil colab-listing	nil "Lists all known databases")
    (consult "" nil colab-consult	nil "Consults a database to all known subsystems")
    (destroy "" nil colab-destroy	nil "Destroy the databases of all known subsystems")
    (replace "" nil colab-replace	nil "Replace a database to all known subsystems")
    (back    "" nil colab-back		nil "Goes back to the last invoked subsystems")
    (exec    "" nil colab-exec		nil "Executes a batch file")
    (cmds    "" nil colab-cmds		nil "Print all available commands")
    (tables  "" nil colab-tables	nil "Print the contents of all known command tables")
    (tx      "" nil colab-call-tx	nil "Call subsystem taxon with rest of arguments")
    (cn      "" nil colab-call-cn	nil "Call subsystem contax with rest of arguments")
    (fw      "" nil colab-call-fw	nil "Call subsystem forward with rest of arguments")
    (rf      "" nil colab-call-rf	nil "Call subsystem rfi with rest of arguments")
    (col     "" nil colab-call-col	nil "Call system colab with rest of arguments")
    (?       "" nil colab-questionmark	nil "Short help function")
    (help    "" nil colab-cmd-help	nil "Long help function")
    (cd      "" nil colab-cd		nil "Change directory")
    (pwd     "" nil colab-pwd		nil "Print the current directory")
    (reload  "" nil colab-reload	nil "Reload a specified subsystem")
    (lisp    "" nil colab-lisp		nil "Eval expression")
    (version "" nil colab-version	nil "Print Colab Version")
    )
)

(defvar *col-list-of-relfun*	nil "List of relfun commands")
(defvar *col-list-of-forward*	nil "List of forward commands")
(defvar *col-list-of-taxon*	nil "List of taxon commands")
(defvar *col-list-of-contax*	nil "List of forward commands")

(defvar *col-current-list*	nil "Current command list")
(defvar *col-current-system*	:colab "Current active subsystem")

#| **************************************************************************
   global initialisation
   *********************************************************************** |#

#+Genera (defun col-set-syntax nil ; Genera does not allow #'<fct> in
				   ; set-macro-character !!!!!!
  "redefine commonlisp reader to recognize '_', '`', '@' and '|'"
  (set-macro-character #\_ 'col-underscore-reader nil *col-readtable*)
  (set-macro-character #\` 'col-backquote-reader  nil *col-readtable*)
  (set-macro-character #\@ 'col-ecal-reader       nil *col-readtable*)
  (set-macro-character #\| 'col-bar-reader        nil *col-readtable*))

#-Genera(defun col-set-syntax nil
  "redefine commonlisp reader to recognize '_', '`', '@' and '|'"
  (set-macro-character #\_ #'col-underscore-reader nil *col-readtable*)
  (set-macro-character #\` #'col-backquote-reader  nil *col-readtable*)
  (set-macro-character #\@ #'col-ecal-reader       nil *col-readtable*)
  (set-macro-character #\| #'col-bar-reader        nil *col-readtable*))

(defun col-underscore-reader (stream char)
  (declare (ignore char))
  (let ((x (read stream t nil t)))
    (if (numberp x)
        (list 'vari (intern (princ-to-string x)))
        (list 'vari x))))

(defun col-backquote-reader (stream char)
  (declare (ignore char))
  (list 'inst (read stream t nil t)))

(defun col-ecal-reader (stream char)
  (declare (ignore char))
  (list 'ecal (read stream t nil t)))

(defun col-cut-reader (stream char)
  (declare (ignore char))
  (list 'cut (read stream t nil t)))

(defun col-bar-reader (stream char)
  (declare (ignore stream char))
  '\|)

(defun col-reset-syntax nil
  (setq *readtable* (copy-readtable nil)))

(defun col-set-batch-mode ()
  (setq *col-input-mode* 'batch))

(defun col-set-interactive-mode ()
    (setq *col-input-mode* 'interactive))

(defun col-interactive-mode-p ()
    (eq *col-input-mode* 'interactive))

(defun col-batch-mode-p ()
  (eq *col-input-mode* 'batch))

(defun colab-init (&aux (cmd-entry (assoc 'colab *col-list-of-systems*)))
  "Initializes the system"
  (setq *col-prompt* 
	(cons (col-prompt cmd-entry) nil))
  (setq *colab-alternate* 'colab-else)
  (col-set-interactive-mode)
  (if (not *col-readtable*)
      (progn (setq *col-readtable* (copy-readtable))
	     (col-set-syntax))))

(colab-init)

(defun colab-tables (userline)
  (declare (ignore userline))
  (print "*col-default-tab*")	(print *col-default-tab*)
  (print "*col-list-of-colab*")	(print *col-list-of-colab*)
  (print "*col-list-of-forward*") (print *col-list-of-forward*)
  (print "*col-list-of-taxon*")	(print *col-list-of-taxon*)
  (print "*col-list-of-contax*") (print *col-list-of-contax*)
  (print "*col-list-of-relfun*") (print *col-list-of-relfun*)
  (print "*col-current-list*")	(print *col-current-list*)
  (print "*colab-alternate*")	(print *colab-alternate*)
)

#| **************************************************************************
   main functions
   *********************************************************************** |#

(defun col-reset ()
  "After an error running batchfiles should be closed."
  (cond ((streamp *col-script-input*) 
	 (close *col-script-input*)
	 (col-set-interactive-mode)))
  (if (streamp *rfi-script-output*)
      (close *rfi-script-output*))
  (setq *rfi-script-output*   nil)
  (setq *col-script-input*    nil)
)

(defun colab (&optional newtable
	      &aux (cmd-entry (car (member (car *col-prompt*)
					   *col-list-of-systems*
					   :key #'cadr))))
  "If an optional parameter is supplied, it is a recursive call of
   colab, with a new command table."
  (cond (newtable (setq *col-current-list* newtable) (throw :colab newtable))
        (t (setq *colab-alternate* 
		 (if (and cmd-entry (col-else-fct cmd-entry))
		     (col-else-fct cmd-entry)
		     'colab-else))
	   (col-reset)
	   (do ((table *col-current-list*
		       (catch :colab (colab-top *col-current-list*))))
	       ((equal table 'lisp) t)
	       #|(print "thrown")
	       (print table)|#)))
)

(defun col-new-prompt (prompt
		       &aux (memb? (member prompt *col-prompt* :test 'equal)))
  "Add a new prompt to the system"
  (cond (memb? (setq *col-prompt* memb?))
	(t (push prompt *col-prompt*))
  )
)

(defun col-do-subsystem (cmd-entry userline
			 &aux (prompt (col-prompt cmd-entry))
			      (else-fct *colab-alternate*))
  "Call an new subsystem"
  (if (string-not-equal prompt "")
      (col-new-prompt prompt))
  (if (col-else-fct cmd-entry)
      (setq *colab-alternate* (col-else-fct cmd-entry)))
  (princ (col-help cmd-entry))(terpri)
  (funcall (col-function cmd-entry) userline)
  (setq *colab-alternate* else-fct)
  (if (string-not-equal prompt "")
      (pop *col-prompt*))
  nil
)

(defun col-search-cmd (usercmd usr-table
		       &aux (cmd-entry (assoc usercmd *col-default-tab*)))
  "Search for a command in the following way:
   1. try the global default table wich contains commands the switch to
      the other (sub-)systems,
   2. if no comand was found, try the subshell command table for
      subsystem specific commands.
   3. if no command was found, try colab standard commands like
      'listing', 'consult' ...
  "
  (if (null cmd-entry) (setq cmd-entry (assoc usercmd usr-table)))
  (if (null cmd-entry) (setq cmd-entry (assoc usercmd *col-list-of-colab*)))
  cmd-entry
)

(defun colab-top-execute (userline usr-table
			  &aux (cmd-entry (col-search-cmd (car userline)
							  usr-table)))
  "Executes a command, assuming a set of command tables, which will
   be searched by col-search-cmd
   If no command was found, execute the function, whose name is
   stored in *colab-alternate*
   Any throw to :error will be catched.
   A throw to :error should return as the result nil.
   A throw to :newcmd will return as the result a new entered command.
   All alternate commands should return nil
   if no new entered command is specified.
  "
  (cond ((null cmd-entry)
	 (catch :newcmd
             (catch :error
		(catch nil (funcall *colab-alternate* userline))))
)
	((col-newtab cmd-entry) ; new command table supplied?
	 (push (col-prompt cmd-entry) *col-prompt*)
	 (setq *col-default-tab* (col-newtab cmd-entry))
	 nil)
	(t (catch :newcmd
		   (catch :error (col-do-subsystem cmd-entry (rest userline)))
		   ))
  )
)

(defun colab-top (usr-table)
  "Executes the top level command loop for one subshell."
  (do* ((userline (col-read)))
       ((and (equal (car userline) 'lisp) (null (cdr userline))) 'lisp)
       (setq userline (colab-top-execute userline usr-table))
       (if (null userline) (setq userline (col-read)))
  )
)

#| **************************************************************************
   gateway functions to subsystems
   *********************************************************************** |#

(defun col-check-subsystem (subsystem)
  "Check if subsystem is loaded."
  (if (member subsystem *features*)
      t
      nil)
)

(defun colab-rfi (userline)
  (declare (ignore userline))
  (setq *col-current-system* :rfi)
  (rfi-cmd-inter)
  (colab *col-list-of-relfun*))

(defun colab-rfe (userline)
  (declare (ignore userline))
  (setq *col-current-system* :rfi)
  (if (emulator-available-p)
      (progn (rfi-cmd-emul) (colab *col-list-of-relfun*))
      (print "emulator not available"))
)

(defun colab-relfun (userline)
  (declare (ignore userline))
  (setq *col-current-system* :relfun)
  (relfun))

;(defun col-load-subsystem (subsystem filename)
(defun col-load-subsystem (subsystem load-fn)
  "Loads specified subsystem if not loaded and if exists
   Returns T is succsesfull, otherwise nil"
  (unless (col-check-subsystem subsystem)
	  (princ "Try to load; please wait a moment.")(not (terpri))
	  (funcall load-fn))
  (setq *col-current-system* subsystem)
  )

; Functions that load the Subsystems forward, taxon and contax
; and defines their compile-lists

(defun colab-forward (userline)
  (declare (ignore userline))
  (setq our-fs:*compile-in-use* t)
  (setq our-fs::compile-list nil)
  ;(when (col-load-subsystem :forward *colab-forward-startup*)
  (when (col-load-subsystem :forward 'load-forward)
	(if our-fs::compile-list (setq our-fs::compile-forward-list our-fs::compile-list)
	    nil)
  	(setq our-fs::compile-list nil)
  	(setq our-fs:*compile-in-use* nil)
        (colab *col-list-of-forward*)))

(defun colab-taxon (userline)
  (declare (ignore userline))
  (setq our-fs:*compile-in-use* t)
  (setq our-fs::compile-list nil)
  ;(when (col-load-subsystem :taxon *colab-taxon-startup*)
  (when (col-load-subsystem :taxon 'load-taxon)
	(if our-fs::compile-list (setq our-fs::compile-taxon-list our-fs::compile-list)
	    nil)
  	(setq our-fs::compile-list nil)
  	(setq our-fs:*compile-in-use* nil)
	(colab *col-list-of-taxon*)))

(defun colab-contax (userline)
  (declare (ignore userline))
  (setq our-fs:*compile-in-use* t)
  (setq our-fs::compile-list nil)
  ;(when (col-load-subsystem :contax *colab-contax-startup*)
  (when (col-load-subsystem :contax 'load-contax)
	(if our-fs::compile-list (setq our-fs::compile-contax-list our-fs::compile-list)
	    nil)
  	(setq our-fs::compile-list nil)
  	(setq our-fs:*compile-in-use* nil)
	(colab *col-list-of-contax*)))

(defun colab-colab (userline)
  (declare (ignore userline))
  (colab *col-list-of-colab*))

#| **************************************************************************
   local colab commands
   *********************************************************************** |#

(defun col-map-element (table element error-message
			&aux (cmd-entry (assoc (car element) table)))
  "Call the function associated with the tag of element in the table table"
  "If no function could be found, print an error message"
  (cond ((null cmd-entry) (print error-message)(print element))
	(t (funcall (col-function cmd-entry) element)))
)

#| colab - listing command *********************************************** |#

(defun colab-listing (userline)
  (colab-list-taxon userline)
  (colab-list-contax userline)
  (colab-list-relfun userline)
  (colab-list-forward userline)
)

#| colab - replace database command ************************************** |#

(defun col-extension (file extension
                      &aux (filename (cond ((stringp file) file)
                                           ((symbolp file)
                                            (string-downcase (string file)))
                                           (t ""))))
  "Compute the extension of a given type."
  (let ((slash (position #+Mac #\: #-Mac #\/ filename :from-end t))
        (point (position #\. filename :from-end t)))
    (cond ((string/= filename "")
           (cond ((or (and slash point (< slash point))
                      (and (null slash) point))
                  filename)
                 ((or (and slash point (> slash point))
                      (null point))
                  (concatenate 'string filename extension))))
          (T (col-error "(col-extension): "
                       (format nil "~A" file)
                       " filename must be of type string or symbol!")))))

(defun colab-replace (userline
                      &aux (filename
                            (col-make-pathname (col-extension (car userline) ".rf"))))
  "Replace database."
  (if (probe-file filename)
      (colab-replace-1 filename)
      (col-error "(col-replace): " filename " file doesn't exist!"))
)

(defun colab-replace-1 (filename)
  (colab-destroy nil)
  (col-cmd-consult-1 filename)
)

#| colab - destroy database command ************************************** |#

(defun colab-destroy-colab (userline)
  (declare (ignore userline))
  nil)

(defun colab-destroy (userline)
  (colab-destroy-relfun userline)
  (colab-destroy-forward userline)
  (colab-destroy-taxon userline)
  (colab-destroy-contax userline)
  (colab-destroy-colab userline)
)

#| colab - consult database command ************************************** |#

(defun colab-consult (userline
		      &aux (filename
			    (col-make-pathname (col-extension (car userline) ".rf"))))
  "Consult a databse with specified name."
  (if (probe-file filename)
      (col-cmd-consult-1 filename)
      (col-error "(col-consult): " filename " file doesn't exist!")))

(defun colab-consult-attrterm (element)
  "List hybrid database elements."
  (colab-consult-taxon element)
  (colab-consult-forward element)
)

(defun colab-consult-defun (elem)
  "There is a defun token in a database;
   consider it as a definition of a lisp-function."
  (eval elem))

(defvar *col-consult-fct*
  '((hn nil nil colab-consult-relfun nil "assert element to relfun database")
    (ft nil nil colab-consult-relfun nil "assert element to relfun database")
    (ad nil nil colab-consult-relfun nil "assert element to relfun database")
    (indi nil nil colab-consult-taxon nil "assert element to taxon database")
    (asse nil nil colab-consult-taxon nil "assert element to taxon database")
    (doma nil nil colab-consult-taxon nil "assert element to taxon database")
    (conc nil nil colab-consult-taxon nil "assert element to taxon database")
    (pred nil nil colab-consult-taxon nil "assert element to taxon database")
    (attr nil nil colab-consult-taxon nil "assert element to taxon database")
    (role nil nil colab-consult-taxon nil "assert element to taxon database")
    (prim nil nil colab-consult-taxon nil "assert element to taxon database")
    (cpred nil nil colab-consult-taxon nil "assert element to taxon database")
    (apred nil nil colab-consult-taxon nil "assert element to taxon database")
    (ofam nil nil colab-consult-taxon nil "assert element to taxon database")
    (cfam nil nil colab-consult-taxon nil "assert element to taxon database")
    (sfam nil nil colab-consult-taxon nil "assert element to taxon database")
    (rolefact nil nil colab-consult-attrterm nil "list taxon/forward database element")
    (attrfact nil nil colab-consult-attrterm nil "list taxon/forward database element")
    (attrterm nil nil colab-consult-attrterm nil "list taxon/forward database element")
    (hierarchy nil nil colab-consult-taxon nil "list taxon/forward database element")
    (rl nil nil colab-consult-forward nil "assert element to forward database")
    (up nil nil colab-consult-forward nil "assert element to forward database")
    (fact nil nil colab-consult-forward nil "list forward database element")
    (lispeval nil nil colab-consult-contax nil "assert contax database element")
    (tt nil nil colab-consult-contax nil "assert contax database element")
    (dd nil nil colab-consult-contax nil "assert contax database element")
    (pc nil nil colab-consult-contax nil "assert contax database element")
    (lc nil nil colab-consult-contax nil "assert contax database element")
    (rc nil nil colab-consult-contax nil "assert contax database element")
    (cc nil nil colab-consult-contax nil "assert contax database element")
    (dc nil nil colab-consult-contax nil "assert contax database element")
    (cv nil nil colab-consult-contax nil "assert contax database element")
    (ci nil nil colab-consult-contax nil "assert contax database element")
    (defun nil nil colab-consult-defun nil "Evaluate a definition in a Database")
   )
  "List of Functions, which will insert elements in local databases")

(defun col-cmd-consult-1 (file &aux tmpdb)
  (setq tmpdb (with-open-file (ifile file :direction :input)
  (let ((*readtable* *col-readtable*))
       (do ((clause nil)
	    (tmpdb nil (cons clause tmpdb)))
	   ((null (setq clause (read ifile nil nil nil)))
	    (reverse tmpdb))))))
  (mapcar #'(lambda (element)
		    (col-map-element *col-consult-fct* element
				     "unknown type for consult")) tmpdb)
  tmpdb
)

#| colab - back command ************************************************** |#

(defun colab-back (userline
		   &aux (cmd-entry (member (cadr *col-prompt*)
					   *col-list-of-systems*
					   :key #'cadr)))
  "Go one subsystem level back."
  (declare (ignore userline))
  (cond ((null cmd-entry)
	 (princ "No way to go back!")
;	 (print (cadr *col-prompt*))
;	 (print cmd-entry)
        )
	; *col-current-system* set to :colab, if toplevel is used.
	(t (if (equal (cadr *col-prompt*) "colab")
	       (setq *col-current-system* :colab))
	   (col-do-subsystem (col-cmd cmd-entry) nil))) 
)

#| colab - exec command ************************************************** |#

(defun colab-exec (userline)
   "Execute a batchfile."
   (col-cmd-execute userline)
)

#| colab - list cmds command ********************************************* |#

(defun col-print-cmd (cmd-entry)
  (princ (string-downcase (col-cmd cmd-entry))) (princ " ")
)

(defun colab-cmds (userline)
  "Print available commands."
  (declare (ignore userline))
  (princ "colab-system-commands: ")
  (mapcar #'col-print-cmd *col-default-tab*)
  (terpri)
  (princ "sub-system-commands: ")
  (mapcar #'col-print-cmd *col-current-list*)
  (terpri)
  (princ "colab-support-commands: ")
  (mapcar #'col-print-cmd *col-list-of-colab*)
)

#| colab - execute a command in an other subsystem *********************** |#

(defun col-wrap-alternate (usercmd usr-table userline
                           &aux (cmd-entry (assoc usercmd *col-default-tab*))
				(else-fct *colab-alternate*))
  "Execute a subshell command and return."
  (if (null cmd-entry)
      (error "colab-internal error: usercmd <~A> not found ~%Please contact Klaus" usercmd))
  (if (col-else-fct cmd-entry)
      (setq *colab-alternate* (col-else-fct cmd-entry)))
  (unwind-protect (colab-top-execute userline usr-table)
		  (setq *colab-alternate* else-fct))
)
			   
(defun colab-call-rf (userline)
  "Execute a command in a subsystem."
  (if (col-check-subsystem :relfun)
      (col-wrap-alternate 'relfun *col-list-of-relfun* userline)
      (print "Subsystem :relfun is not available"))
)

(defun colab-call-fw (userline)
  "Execute a command in a subsystem."
  (if (col-check-subsystem :forward)
      (col-wrap-alternate 'forward *col-list-of-forward* userline)
      (print "Subsystem :forward is not available"))
)

(defun colab-call-cn (userline)
  "Execute a command in a subsystem."
  (if (col-check-subsystem :contax)
      (col-wrap-alternate 'contax *col-list-of-contax* userline)
      (print "Subsystem :contax is not available"))
)

(defun colab-call-tx (userline)
  "Execute a command in a subsystem."
  (if (col-check-subsystem :taxon)
      (col-wrap-alternate 'taxon *col-list-of-taxon* userline)
      (print "Subsystem :taxon is not available"))
)

(defun colab-call-col (userline)
  "Execute a command in a subsystem."
  (col-wrap-alternate 'colab *col-list-of-colab* userline)
)

#| colab - provide a small help facility ********************************* |#

(defun col-print-questionmark (cmd-entry)
  (format t "~18A ~A~%" (string-downcase (col-cmd cmd-entry)) (col-help cmd-entry))
)

(defun colab-questionmark (userline)
  "Print short command descriptions."
  (cond ((null userline)
	 (princ "colab-system-commands: ")(terpri)
	 (mapcar #'col-print-questionmark *col-default-tab*)
	 (terpri)
	 (princ "sub-system-commands: ")(terpri)
	 (mapcar #'col-print-questionmark *col-current-list*)
	 (terpri)
	 (princ "colab-support-commands: ")(terpri)
	 (mapcar #'col-print-questionmark *col-list-of-colab*))
	(t (let ((cmd-entry (col-search-cmd (car userline) *col-current-list*)))
		(col-print-questionmark cmd-entry))))
)

#| colab - provide a big help facility *********************************** |#
(defvar *relfun-help-dir* nil "Relfun Help Directory")
(defvar *forward-help-dir* nil "Forward Help Directory")
(defvar *taxon-help-dir* nil "Taxon Help Directory")
(defvar *contax-help-dir* nil "Contax Help Directory")

(defun colab-cmd-help (userline)
  "Help function for more genearal help."
  (if (null userline)
      (col-cmd-help-1 "help.tex")                 ; general help text
      (col-cmd-help-1 (string (car userline)))))  ; specific help text

(defun col-cmd-help-1 (file)
  (if (eq *col-current-system* :colab)
      (col-cmd-help-2 file)
  (let ((filename (our-fs::logdir-file
		   (cond ((or (eq *col-current-system* :relfun)
			      (eq *col-current-system* :rfi))
			  :rf-help)
			 ((eq *col-current-system* :forward)
			  :fw-help)
			 ((eq *col-current-system* :taxon)
			  :tx-help)
			 ((eq *col-current-system* :contax)
			  :cn-help))
		   (ren (string-downcase (col-extension file ".tex"))))))
       (if (probe-file filename)
	   (col-cmd-help-3 filename)
	   (col-cmd-help-2 file)))))

(defun ren (file)
  "remove the characters #\0 and #\? from the filename"
  (remove #\0 (remove #\? file)))

(defun col-cmd-help-2 (file)
  (let ((filename (our-fs::logdir-file :col-help
				       (ren (string-downcase 
					     (col-extension file ".tex"))))))
       (if (probe-file filename)
           (col-cmd-help-3 filename)
           (print "Sorry... no help available!"))))

(defun col-cmd-help-3 (file)
  "For each line in the file print deTeXed lines."
  (with-open-file (ifile file :direction :input)
        (do ((line (read-line ifile nil nil nil)
             (read-line ifile nil nil nil)))
            ((null line))
            (col-cmd-help-4 line)
	    (terpri))))

(defun col-cmd-help-4 (line)
  "deTeX a line."
  (col-remove-char (delete #\} (delete #\$ line)) '(#\{) ))

(defun col-remove-char (line chars)
  "Remove chars from line."
  (cond ((null chars) (col-remove-cmd line))
        (t (let ((pos (position (car chars) line)))
                (cond ((not pos) (col-remove-char line (cdr chars)))
                      (t (col-remove-char (subseq line 0 (1+ pos)) (cdr chars))
                         (col-remove-char (subseq line (1+ pos)) chars))))))
)

(defun col-remove-cmd (line &aux (pos (position #\\ line)))
  (cond ((and pos (> pos 0))
	 (princ (subseq line 0 pos))
	 (col-remove-cmd (subseq line pos)))
	((and pos (or (string= line "\\str{") 
		      (string= line "\\stmb{")
		      (string= line "\\stmt{"))))
	(pos (princ (delete #\\ (subseq line 0 pos)))
	     (col-remove-cmd (subseq line (1+ pos))))
	(t (princ (delete #\{ line))))
  )

#| colab - reload subsystem *********************************************** |#

(defun colab-reload (userline &aux (system (car userline)))
  "This function will reload a specified lisp system."
  (setq *features*
	(remove (cond ((equal system 'forward) :forward)
		      ((equal system 'relfun) :relfun)
		      ((equal system 'taxon) :taxon)
		      ((equal system 'contax) :contax)
		      (t (col-error "colab-reload: invalid system specified")))
		*features*))
  (colab-top-execute userline nil)
)

#| colab - evaluate expression ******************************************** |#

(defun colab-lisp (userline)
  "This function will be called, if lisp with arguments is typed."
  (mapcar #'(lambda (x) (print (eval x))) userline)
)

#| colab - print colab version ******************************************** |#

(defun colab-version (userline)
  "This function prints the version number of colab."
  (declare (ignore userline))
  (princ *col-version*)(terpri)
)

#| colab - pathname abstraction ******************************************* |#

(defun col-pathname-directory (pathname)
  "Returns the pathname-directory of pathname"
  #-:lispm (pathname-directory pathname)
  #+:lispm (scl:send (pathname pathname) :raw-directory)
)

(defun col-directory-append (pathname subdir)
  (if subdir
      #+:Mac (concatenate 'string pathname subdir ":")
      #-:Mac (append pathname (list subdir))
      pathname)
)

(defun col-directory-butlast (pathname)
  #+:Mac (subseq pathname  0 (1+ (cond ((position #\: pathname :from-end t))
				       (t -1))))
  #-:Mac (butlast pathname)
)

(defun col-make-pathname (pathname)
  (merge-pathnames pathname *colab-directory*)
)

(defun col-home-dir ()
  "Compute the home directory, which is the startup directory in Alegro or kcl
   and the user home directory in symbolics."
  ;(col-pathname-directory
   #+:lispm (user-homedir-pathname)
   #-:lispm (truename #-:Mac *default-pathname-defaults*
		      #+:Mac (user-homedir-pathname))
;	      )
)

(setq *colab-directory* (col-home-dir))

(defun colab-pwd (userline)
  (declare (ignore userline))
  (princ "Current Pathname is ")(prin1 *colab-directory*)(terpri))

(defun colab-cd (userline 
		 &aux (file (car userline))
		 (pathname (cond ((null file) (col-home-dir))
				 ((stringp file) file)
				 ((symbolp file)
				  (string-downcase (string file)))
				 (t (col-home-dir)))))
  "Change the default pathname."
  (setq *colab-directory*
	(merge-pathnames 
	 (cond ((pathnamep pathname) pathname)
	       ((not (equal (subseq pathname (1- (length pathname))) "/"))
		(concatenate 'string pathname "/")))
	 *colab-directory*))
  (colab-pwd nil)
)

#| **************************************************************************
   batch facility
   *********************************************************************** |#

(defun col-cmd-execute (userline)
  (let ((infile (col-make-pathname (col-extension (car userline) ".bat"))))
       (cond ((col-batch-mode-p)
	      (col-error "(col-cmd-execute): batch already running!"))
	     ((not (probe-file infile))
	      (col-error "(col-cmd-execute): " infile " batchfile doesn't exist!"))
	     (t
	      (col-cmd-execute-1 infile)))))

(defun col-cmd-execute-1 (infile)
  (princ "batchjob executing...")
  (terpri)
  (setq *col-script-input* (open infile :direction :input))
  (col-set-batch-mode))

(defun col-cmd-endexecute ()
  (close *col-script-input*)
  (setq *col-script-input* nil)
  (col-set-interactive-mode)
  (princ "batchjob done...")
  (terpri))

#| **************************************************************************
   reader facilities
   *********************************************************************** |#

(defun more-p nil
  (do ((response (if (eq *col-current-system* :relfun)
		     (readl)(col-read))
		 (if (eq *col-current-system* :relfun)
		     (readl) (col-read))))
      ((or (eq (car response) 'm)
	   (eq (car response) 'more)
	   (if (eq *col-current-system* :relfun)
	       (or (eq (car response) 'ori)
		   (eq (car response) 'lisp)
		   (not (rfi-command-p response)))
	       t)
	   )
       (if (or (eq (car response) 'm)
	       (eq (car response) 'more))
	   nil
	   (if (eq *col-current-system* :relfun)
	       response
	       (throw :newcmd response))))
      (if (eq *col-current-system* :relfun) (rfi-command response))
      )
)

(defun col-read ()
  "Read terminal input as strings and convert it to objects, which colab likes."
  (do ((usercmd "" (col-reader))) 
      ((not (equal usercmd ""))
       (col-transform-string-to-lisp usercmd))))

(defun col-transform-string-to-lisp (str)
  "Convert from string to lisp using col-readtable."
  (let ((*readtable* *col-readtable*))
       (read-from-string (concatenate 'string "(" str ")"))))

(defun col-reader ()
  "Reads from terminal and returns a string."
    (do* ((line (col-readline t) (col-readline nil))
	  (cmd  line             (concatenate 'string cmd line)))
	 ((col-complete-cmd-p cmd) cmd)))

(defun col-readline (firsttime)
  (col-remove-remarks (col-readline-1 firsttime)))

(defun col-script-mode-p ()
    (not (null *rfi-script-output*)))

(defun col-prompt-print (file)
  (maplist #'(lambda (x) (princ (if (equal (car x) "fw")
                                    (if *fw-interpreter-mode* "fwi" "fwe") 
                                    (car x))
                                file)
		     (if (null (cdr x)) (princ "> ")(princ ",")))
	   (reverse *col-prompt*)))



(defun col-readline-1 (firsttime)
  (cond ; normal input
	((and (not (col-batch-mode-p)) (not (col-script-mode-p)))
	 (col-read-from-terminal firsttime))
        ; protocoll to script-file
        ((and (not (col-batch-mode-p)) (col-script-mode-p))
	 (let ((inputline (col-read-from-terminal firsttime)))
	      (cond ((and firsttime (equal inputline "endscript"))
		     (rfi-cmd-endscript)
		     "")
		    (t (if firsttime
			   (progn (fresh-line *rfi-script-output*)
				  (col-prompt-print *rfi-script-output*)))
		       (princ inputline *rfi-script-output*)
		       (terpri *rfi-script-output*)
		       inputline))))
        ; get input form batch-file
        ((and (col-batch-mode-p) (not (col-script-mode-p)))
	 (let ((inputline (col-read-from-batch-file)))
	      (cond ((and firsttime (equal inputline "end-of-batch-file"))
		     (col-cmd-endexecute)
		     "")
		    (t (if firsttime
			   (progn (fresh-line *col-standard-output*)
				  (col-prompt-print *col-standard-output*)))
		       (princ inputline *col-standard-output*)
		       (terpri *col-standard-output*)
		       inputline))))
        ; get input from batch-file and protocoll to script-file
        ;
        ((and (col-batch-mode-p) (col-script-mode-p))
         (let ((inputline (read-from-batch-file)))
	      (cond (firsttime
		     (cond ((equal inputline "end-of-batch-file")
			    (col-cmd-endexecute)
			    "")
			   ((equal inputline "endscript")
			    (fresh-line *col-standard-output*)
			    (col-prompt-print *col-standard-output*)
			    (princ inputline *col-standard-output*)
			    (terpri *col-standard-output*)
			    (rfi-cmd-endscript)
			    "")
			   (t (fresh-line *col-standard-output*)
			      (fresh-line *rfi-script-output*)
			      (col-prompt-print *col-standard-output*)
			      (col-prompt-print *rfi-script-output*)
			      (princ inputline *col-standard-output*)
			      (princ inputline *rfi-script-output*)
			      (terpri *col-standard-output*)
			      (terpri *rfi-script-output*)
			      inputline)))
		    (t (princ inputline *col-standard-output*)
		       (princ inputline *rfi-script-output*)
		       (terpri *col-standard-output*)
		       (terpri *rfi-script-output*)
		       inputline)))))
)

(defun col-read-from-batch-file ()
  (read-line *col-script-input* nil "end-of-batch-file" nil))

(defun col-read-from-terminal (firsttime)
  (if firsttime (progn (fresh-line *col-standard-output*)
		       (col-prompt-print *col-standard-output*)))
  (read-line *col-standard-input*))

(defun col-complete-cmd-p (x)
  (= (count #\( x) (count #\) x)))

(defun col-remove-remarks (x)           ; Remark starts with a ";"
  (let ((p (position #\; x)))
       (if p (subseq x 0 p) x))
)

(defun col-error (&rest liste)
  (apply #'format
	 (append (list t (apply #'concatenate
				(append (list 'string "ERROR ")
					(mapcar #'(lambda (x) " ~A") liste))))
		 liste))
  (throw :error nil))

(pushnew :colab *features*)

#| **************************************************************************
   relfun specific primitives 
   *********************************************************************** |#

(defvar *hn-rulebase*		nil "hn-Version of bidirctional rules")
(defvar *rule-database*		nil "List of bidirctional rules")
(defvar *factbase*		nil "List of hybrid ground facts")

(if (null *col-list-of-relfun*)
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
    )
))

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

(defun colab-relfun-horizon (userline)
  (declare (ignore userline))
  (setq *rfi-database* (horizon-database *rfi-database*))
  (setq *hn-rulebase* (horizon-database *hn-rulebase*)))

(defun colab-relfun-verti (userline)
  (colab-rfi-cmd-compile (cons 'verti userline)))

(defun colab-relfun-compile (userline)
  (setq *rfi-database* (horizon-database *rfi-database*))
  (setq *hn-rulebase* (horizon-database *hn-rulebase*))
  (setq *factbase* (horizon-database *factbase*))
  (colab-rfi-cmd-compile (cons 'compile userline)))

(defun colab-rfi-cmd-compile (userline)
  (if (emulator-available-p)
      (loco (colab-select-clauses-of (cadr userline)))))


(defun colab-select-clauses-of (procedurename)
  (let ((database (append *rfi-database* *hn-rulebase* *factbase*)))
  (cond ((null procedurename) database)
        (t (colab-select-clauses-of-1 procedurename database)))))

(defun colab-select-clauses-of-1 (procedurename db)
  (cond ((null db) nil)
        ((eq procedurename (car (s-conclusion (car db))))
         (cons (car db) (colab-select-clauses-of-1 procedurename (cdr db))))
        (t (colab-select-clauses-of-1 procedurename (cdr db)))))



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

#| **************************************************************************
   forward specific primitives 
   *********************************************************************** |#

(defvar *up-rulebase* nil "Forward-Database")

(if (null *col-list-of-forward*)
(setq *col-list-of-forward* ; List of commands, which forward can interpret
  '((listing	"" nil colab-listing	nil "Lists all known databases")
    (fwtest	"" nil forward-test	nil "Replace")
    (fc-init	"" nil fc-init		nil "Initialize forward")
    )
))

(defun colab-consult-forward (elem)
  (declare (ignore elem))
  (princ "forward not loaded")(terpri))

(defun colab-destroy-forward (userline)
  (declare (ignore userline))
  (princ "forward not loaded")(terpri))

(defun colab-list-forward (elem)
  (declare (ignore elem))
  (princ "forward not loaded")(terpri))

#| **************************************************************************
   taxon specific primitives 
   *********************************************************************** |#

(if (null *col-list-of-taxon*)
(setq *col-list-of-taxon* ; List of commands, which taxon can interpret
  '(
    )
))

(defun colab-consult-taxon (elem)
  (declare (ignore elem))
  (princ "taxon not loaded")(terpri))

(defun colab-destroy-taxon (userline)
  (declare (ignore userline))
  (princ "taxon not loaded")(terpri))

(defun colab-list-taxon (elem)
  (declare (ignore elem))
  (princ "taxon not loaded")(terpri))

#| **************************************************************************
   contax specific primitives 
   *********************************************************************** |#

(defvar *cn-database* nil "Contax-Database")

(if (null *col-list-of-contax*)
(setq *col-list-of-contax* ; List of commands, which contax can interpret
  '((destroy "" nil colab-destroy-contax nil "Destroy the contax database *contax-database*")
    (list "" nil colab-list-contax nil "List the contax database *contax-database*")
    (cn "" nil cn nil "propagate the database *contax-database*")
    )
))

(defun colab-consult-contax (elem)
  (declare (ignore elem))
  (princ "contax not loaded")(terpri))

(defun colab-destroy-contax (userline)
  (declare (ignore userline))
  (princ "contax not loaded")(terpri))

(defun colab-list-contax (elem)
  (declare (ignore elem))
  (princ "contax not loaded")(terpri))

