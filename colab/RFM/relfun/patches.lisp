;;; Interface of comment.lsp.


;;; Part 1: Modified functions:



;;; rfi.lsp:


(defvar *comment-style* nil
  "Controls reading and printing.
   If nil then comments are ignored.
   If not nil then it is a list describing how to print comments:
   *comment-style* =  (<comment-column>
                       <style-of-type-1-comments>
                       <style-of-type-2-comments> )
   <comment-column> is in the range 0..*rfi-print-width*.
   <style-of-type-1-comments> is a symbol describing how to format
                              end-of-line-comments:
     RAW                        no special formatting
     COMMENT-COLUMN             align to <comment-column>.
     COMMENT-COLUMN-OR-MARGIN   align to <comment-column>. If not
                                possible align to right margin
     MARGIN                     align to right margin
   <style-of-type-2-comments> is a symbol describing how to format
                              begin-of-line-comments:
     BREAK                      break lines if longer then *rfi-print-width*
     NO-BREAK                   never break lines even if a line
                                exceeds the right margin.
   See also function translate-file.
")


(defvar *default-comment-style*
    '(40 comment-column-or-margin break))


(defun pp-init (&optional (start-column 0))
  (setq pp-stack* nil)
  (setq pp-istack* (list start-column))
  (setq pp-currentpos* start-column))


(defun pp (*expr &optional (start-column 0))
  (if (eq *style* 'prolog)
      (progn
	(pro-print *expr)
	(rf-terpri)
	t)
    (progn
      (pp-init start-column)
      (pp-expr *expr)
      (if *comment-style*
	  ;; converter is active ==> return current column:
	  (1+ pp-currentpos*)
	  (progn (pp-newline) t)))))


(defun pp-expr (*expr)
  (when (eq 'comment-was-here (first pp-stack*)) ; comment printed ?
	(pp-pop pp-stack*) )
  (cond ((consp *expr)
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
		(pp-list *expr '|(| '|)|))))
	((com-cell-p *expr)
	 (lsyn-comment-printer *expr
			       pp-currentpos*
			       (1+ (first pp-istack*)) ; indent column
			       ))
	(t (pp-prin *expr))))


(defun pp-clause (clause &optional (start-column 0))
  (if (eq *style* 'prolog)
      (if *comment-style*
	  ;; converter is active ==> return current col:
	  (pro-print clause)
	  (progn
	    (pro-print clause)
	    (rf-terpri)
	    t ))
      (progn
	(pp-init start-column)
	(if (com-cell-p clause)
	    (lsyn-comment-printer clause
				  pp-currentpos*
				  (1+ (first pp-istack*)) ; indent column
				  t	; clause-p
				  )
	    (if (and (< (length clause) 3) (pp-fits clause))
		(pp-small-clause clause)
		(pp-big-clause clause)))
	(if *comment-style*
	    ;; converter is active ==> return current col.,
	    (1+ pp-currentpos*)
	    (progn (pp-newline) t) ))))


(defun pp-finish (post)
  (cond ((eq 'comment-was-here (pp-top pp-stack*))
	 ;; print on fresh line because last item was a comment:
	 (setq pp-currentpos* (pp-top pp-istack*))
	 (pro-newline-and-indent (1+ pp-currentpos*))
	 (pp-pop pp-stack*)
	 (unless (equal ")" (pp-top pp-stack*))
	   (pp-pop pp-stack*) )
	 (pp-pop pp-stack*) )
	((equal ")" (pp-top pp-stack*))
	 ;; pre was on same line ==> don't print a blank:
	 (pp-pop pp-stack*)
	 )
	((equal " " (pp-top pp-stack*))
	 ;; pre was not on same line ==> print a blank
	 (pp-pop pp-stack*)
	 (pp-pop pp-stack*)
	 (pp-prin '| |)
	 ))
  (pp-prin post) )


;; handle-rfi-cmd: improved handling of head-patterns:

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
		;; examples for listing:
		;;   listing                  % all clauses
		;;   listing numbered         % functor of head is NUMBERED
		;;   listing partition[Cr](\|Y) % heads matching the pattern
		(cond ((string= "" rest-of-line-as-string)
		       '(listing) )
		      ((position "(" rest-of-line-as-string :test #'string=)
		       (list 'listing
			     (pro-parse-head rest-of-line-as-string) ))
		      (t
		       (cons 'listing
			     (read-goal) ))))
	       (rx
		(list 'rx (read-clause)) )
	       ;; commands not available in P-syntax:
	       ((a0hn a0ft azft azhn)
		(rf-princ-like-lisp "Command not available in P-syntax.")
		(setq error-p t) )
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


;;; -------------------------------------------------------------------------
;;;
;;;                 rfi-commands
;;;
;;; -------------------------------------------------------------------------


;; echoing of comments in batch-mode improved:

(defun complete-cmd-p (x)
  (if (eq *style* 'prolog)
      (and (pro-complete-cmd-p x)
	   (= (count #\( x) (count #\) x))
	   (= (count #\[ x) (count #\] x)) )
    (= (count #\( x) (count #\) x)) ))


(defun pro-complete-cmd-p (cmd-as-str)
  ; Commas and some other characters indicate an uncompleted input.
  (if (= 0 (length cmd-as-str))
      t
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



;; continuation indicator depends on length of prompt:

(defun read-from-terminal (firsttime)
  (if firsttime
      (progn (fresh-line *rfi-standard-output*) 
	     (princ *rfi-prompt* *rfi-standard-output*))
      (spaces (length (princ-to-string *rfi-prompt*))) )
  (read-line *rfi-standard-input*) )


(defun rfi-cmd-translate-file (full-userline-as-list)
  ;; Called for the following rfi-cmds:
  ;;   rf->rf, rf->rfp, rfp->rf, rfp->rfp
  (let ((direction (first full-userline-as-list))
	(source (second full-userline-as-list))
	(destination (third full-userline-as-list))
	)
    (translate-file source destination :direction direction) ))



;;; Patches for rfi-commands:


(setq *rfi-commands* (append '(rf->rf rf->rfp rfp->rf rfp->rfp
			       bal->bap bap->bal)
			     *rfi-commands* ))


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
	  ;; horizontal database transformation. (TK 07.10.1990)
	  ;; the function NORMALIZE-DATABASE has to be imported from the file NORMALIZER.LISP
	  ((eq com 'normalize)
	   (setq *rfi-database* (normalize-database *rfi-database*)))
	  ((eq com 'horizon)
	   (setq *rfi-database* (horizon-database *rfi-database*)))
	  ((eq com 'verti)
	   (rfi-cmd-compile userline) (gwam.assem))
	  ((eq com 'compile)
	   (setq *rfi-database* (horizon-database *rfi-database*))
	   (rfi-cmd-compile userline) (gwam.assem)) 
	  ;;userline still ignored in assembling!!!!!
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
	  ((member com 
		   '(rf->rf rf->rfp rfp->rf rfp->rfp bal->bap bap->bal))
	   (rfi-cmd-translate-file userline) )
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
    
    ;; get new input from user afer a interpreter-command
    (cond ((and (eq com 'bye) (rfi-cmd-bye))
	   t)
	  (t nil))))
		      





