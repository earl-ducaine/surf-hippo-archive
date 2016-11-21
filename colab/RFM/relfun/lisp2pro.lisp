;;; Copyright Notice
;;;
;;; This software is distributed for non-profit and research purposes only.
;;; Non-profit redistribution of the current version or parts of the
;;; current version is permitted if this copyright notice is included unchanged.
;;; I give no warranty of any kind for this prototype. It will be further
;;; improved as time permits.  
;;;
;;; Michael Herfert, 1992
 
 
; -----------------------------------------------------------------------------
; Module Lisp2Pro.Lsp:   Transforms an expression given in the Lisp-like
;                        syntax into the Prolog-like syntax.
;
;
; Michael Herfert.
; 2/92: First Version
; 7/92: Fixed problem with "|"
;
;
;
; Exported Item:
;
;  pro-print  expr-or-clause-in-lisp-syntax                      [Function]
;
;      Prints the argument in a pretty-print format in Prolog-like syntax.
;



(defun inst-tup-t (x)
  (and (inst-t x)
       (tup-t (second x)) ))


(defun inst-vari-t (x)
  (and (inst-t x)
       (vari-t (second x)) ))


(defun variable-binding-t (x)
  (and (consp x)
       (= 3 (length x))
       ;(vari-t (first x))
       (eq '= (second x)) ))


(defconstant the-non-printing-char (code-char 0))


;; The following constants are parameters of the pretty printer.
;; They can be changed here without side effects:

;; Small number ==> use many lines when printing: 
(defconstant max-len-of-a-simple-arg 7)


;; Small number ==> use many lines when printing:
(defconstant max-args-per-line 3)


(defun internal-error (&rest msgs)
  (mapcar #'rf-print msgs)
  (error "Internal-Error.") )


;; The type pro-expr describes the Prolog-like representation of a RelFun-
;; expression.
;; The field len describes the space to print the whole expression
;; including commas, spaces and parentheses resp. brackets.
;; The contents of value depends on the value of type:
;; type = atomic  (that means constant or variable)
;;   value: the string-form of the atom.
;; type = argument-list
;;   value: a list of triples:  (<pre-sep> <argument> <post-sep>)
;;          <pre-sep> (seldom used) is e.g. #\|
;;          <argument> is a pro-expr describing the argument
;;          <post-sep> (often used) is e.g. #\,
;; type = round-application (e.g. f(x, y))
;;   value: a list.  First element:   the functor,
;;                   Second element:  the argument-list.
;; type = sqr-application (e.g. g[a,b])
;;   value: a list.  First element:   the functor or nil if it is a tupel.
;;                   Second element:  the argument-list.
;; type = is (e.g. X is 5)
;;   value: a list.  First element:   left side of is
;;                   Second element:  right side of is
;; type = variable-binding (e.g. (X = 10))
;;   value: a list.  First element:   the variable
;;                   Second element:  the binding-value
;; type in {hn-fact, hn-rule, ft-fact, ft-rule}:
;;   value: a list:  1st element:  head
;;                   2nd           connector         (e.g. " :- ")
;;                   3rd           body              (body is of type arg-list)

(defstruct pro-expr 
  type  ; dom(type) =  {round-application, sqr-application,
        ;               argument-list, arg-list-tail, is,
        ;               variable-binding, atomic}
  len					; dom(len) = integer
  value					; dom(value) = STRING u pro-expr.
  internal-p				; <==> comment inside value
  (pre-txt "")				; dom = STRING
  (post-txt "")				; dom = STRING
  ;; If comment begins with a #\newline then it's a begin-of-line-comment,
  ;; otherwise the first substring (upto the first #\newline) is a
  ;; end-of-line-comment.
  ;; If comment is not the empty string it is terminated by #\newline.
  ;; comment is printed after value.
  )


(defun newline-indent (indent)
  (rf-terpri)
  (dotimes (i indent)
           (rf-princ-like-lisp " ") ))


(defun pps (s &optional (indent 0) &key rem)
  ;; pretty-print-structure useful for debugging.
  (if rem (progn (rf-terpri) (rf-princ-like-lisp rem) (rf-terpri)))
  (cond ((pro-expr-p s)
         (rf-princ-like-lisp "  <*pro-expr*>")
         (newline-indent indent)
         (rf-princ-like-lisp "Type: ")
         (rf-princ-like-lisp (pro-expr-type s))
         (newline-indent indent)
         (rf-princ-like-lisp "Len:  ")
         (rf-princ-like-lisp (pro-expr-len s))
         (newline-indent indent)
         (rf-princ-like-lisp "Val:  ")
         (pps (pro-expr-value s) (+ indent 6)) )
        ((consp s)
         (rf-princ-like-lisp "[")
         (mapcar #'(lambda (x)
                     (newline-indent (1+ indent))
                     (pps x (1+ indent)) 
                     )
                 s)
         (newline-indent indent)
         (rf-princ-like-lisp "]"))
        (t
         (rf-princ-like-lisp s) ))
  s) 


(defun construct-measure-tree (expr must-be-sqr-p &aux pair)
  ;; Returns a value of type pro-expr.
  ;; The parameter must-be-sqr-p signals that all round applications
  ;; have to be transformed to sqr-applications, because the call came
  ;; from inside a sqr argument list.
  (cond ((com-cell-p expr)
	 (construct-measure-tree-of-com-cell expr must-be-sqr-p) )
	((stringp expr)
         (make-pro-expr :type 'atomic
                        :len  (flatsize expr)
                        :value (prin1-to-string expr) ))
        ((eq 'id expr)
         (make-pro-expr :type 'atomic
                        :len  1
                        :value "_" ))
        ((atom expr)
         (make-pro-expr :type 'atomic
                        :len  (flatc expr)
                        :value (string-downcase (princ-to-string expr)) ))
        ((vari-t expr)
	 ;; expr = (VARI symbol)
         (let ((name-conc-level (pro-get-variable expr)))
           (make-pro-expr :type 'atomic
                          :len (length name-conc-level)
                          :value name-conc-level )))
        ((variable-binding-t expr)
	 ;; expr = (variable = binding-value)
	 ;; If the interpreter is active, then variable is a symbol with 
	 ;; leading "_"  (e.g. _xyz).
	 ;; If the emulator is active then, variable is a taged list
	 ;; (e.g. (vari x)) 
         (let ((var-as-str 
                (if (atom (first expr))
                    (lisp-var-sym->pro-var-sym (first expr))
                    (pro-get-variable (car expr)) )))
           (setq pair (list (make-pro-expr
                             :type 'atomic
                             :len (length var-as-str)
                             :value var-as-str )
                            (construct-measure-tree (third expr)
                                                    must-be-sqr-p ))))
         (make-pro-expr
          :type 'variable-binding
          :len  (+ 3			; 3 = length(" = ")
                   (pro-expr-len (first pair))
                   (pro-expr-len (second pair)) )
          :value pair ))
	((pro-clause-t expr)
	 (construct-measure-tree-of-clause expr must-be-sqr-p) )
	((pro-is-t expr)
	 ;; expr = (IS left right)
	 ;; impl. INST on the left side
         (setq pair (list (construct-measure-tree (second expr)
                                                  t ) ;left       
                          (construct-measure-tree (third expr)
                                                  nil ) )) ;right
         (make-pro-expr
          :type 'is
          :len  (+ 4			; 4 = length(" is ")
                   (pro-expr-len (first pair))
                   (pro-expr-len (second pair)) )
          :value pair ))
	((and must-be-sqr-p (pro-tup-t expr))
	 ;; expr = (tup arg-1 .. arg-n)  
	 ;; inst outside --> suppress the tup-constructor
	 (setq pair (list               ; constructor & argument-list
		     nil
		     (construct-measure-tree-of-arg-list (cdr expr)
							 t ) ))
	 (make-pro-expr
          :type 'sqr-application
          :len (pro-expr-len (second pair))
          :value pair )) 
	(must-be-sqr-p
	 ;; expr = (constructor arg-1 .. arg-n)
	 ;; inst outside --> handle inst like any other constructor
	 (setq pair (list		; constructor & argument-list
		     (construct-measure-tree (first expr) t)  
		     (construct-measure-tree-of-arg-list (cdr expr) t) ))
         (make-pro-expr
          :type 'sqr-application
          :len  (+ (pro-expr-len (first pair))
                   (pro-expr-len (second pair)) )
          :value pair ))
	;; there was no inst outside:
	;; By the following case (inst (vari x)) is printed as X if there
	;; was no inst outside.
	;; Remove this sexpression and (inst (vari x)) is printed as inst(X).
	((inst-vari-t expr)
	 (construct-measure-tree (second expr) nil) )
	((and (inst-t expr) (not (vari-t (second expr))))
	 ;; expr = (INST (constructor arg-1 .. arg-n))
	 (construct-measure-tree (second expr) t) )
	(t 
	 ;; expr = (functor arg-1 .. arg-n)
         (setq pair (list 
		     (construct-measure-tree (first expr) nil)  
		     (construct-measure-tree-of-arg-list (cdr expr) nil)))
	 (make-pro-expr 
	  :type 'round-application
	  :len (+ (pro-expr-len (first pair))
		  (pro-expr-len (second pair)) )
	  :value pair ))))
	 

(defun construct-measure-tree-of-arg-list (arg-list must-be-sqr-p &key foot-p)
  ;; Returns a value of type pro-expr.
  ;; The len-field contains the whole length included parentheses
  ;; (or brackets), commas and spaces.
  ;; The type-field is of type ARGUMENT-LIST.
  ;; The value-field is a list of elements.
  ;; Each element is a triple:
  ;;   (<pre-sep> <element-value> <post-sep>)
  (do* ((rest-args arg-list (rest rest-args))
	(arg (first rest-args) (first rest-args))
	(next-arg (second rest-args) (second rest-args))
	(ignore-next-arg-p nil)
	(total-length 2)		; len("()")
	(triple nil (list pre-sep tree post-sep))
	(pro-arg-list nil
		      (if tree (cons triple pro-arg-list) pro-arg-list))
	(internal-comment-p nil
			    (or
			     internal-comment-p
			     (and tree (pro-expr-internal-p tree))
			     (and tree (string/= "" (pro-expr-pre-txt tree)))
			     (and tree (string/= "" (pro-expr-post-txt tree)))
			     ))
	(pre-sep)
	(tree)
	(post-sep)
	)
       ((null rest-args)
	;; end of do-loop:
	(make-pro-expr
	 :type 'argument-list
	 :value (reverse pro-arg-list)
	 :internal-p internal-comment-p
	 :len total-length ))
    (if ignore-next-arg-p
	(setq ignore-next-arg-p nil
	      tree nil )
	(cond ((eq '! arg)
	       ;; cut is the first el. in arg-list:
	       (cond ((null (cdr rest-args))
		       ;; arg-list = (!)
		       (setq tree (construct-measure-tree '! must-be-sqr-p)
			     post-sep the-non-printing-char ))
		      ((and foot-p
			    (null (cddr rest-args)) )
		       ;; arg-list = (! <foot>)
		       (setq tree (construct-measure-tree next-arg
							  must-be-sqr-p )
			     pre-sep "!& "
			     post-sep the-non-printing-char
			     ignore-next-arg-p t ))
		      ((null (cddr rest-args))
		       ;; arg-list = (! <arg>)
		       (setq tree (construct-measure-tree next-arg
							  must-be-sqr-p)
			     pre-sep "! "
			     post-sep the-non-printing-char
			     ignore-next-arg-p t ))
		      (t
		       ;; arg-list = (! <arg-1> .. <arg-n>)
		       (setq tree (construct-measure-tree next-arg
							  must-be-sqr-p)
			     pre-sep "! "
			     post-sep #\,
			     ignore-next-arg-p t )))
	       (when (char/= the-non-printing-char post-sep)
		 (incf total-length 2) ) ; 2=len(", ")  (automatic space)
	       (when pre-sep
		 (incf total-length (length pre-sep)) )
	       (incf total-length  (pro-expr-len tree)) )
	      ((eq '|\|| arg)
	       ;; arg-list = (\| <arg>)
	       (setq tree (construct-measure-tree next-arg must-be-sqr-p)
		     pre-sep #\|
		     post-sep the-non-printing-char 
		     ignore-next-arg-p t)
	       (incf total-length (+ 2 (pro-expr-len tree))) ; 2=len("| ")
	       )
	      (t
	       (setq pre-sep nil)
	       (setq tree (construct-measure-tree arg must-be-sqr-p) )
	       (incf total-length (pro-expr-len tree))
	       (cond ((eq '! next-arg)
		      (if (null (cddr rest-args))
			  ;; cut at the end:
			  (setq post-sep " !" ; 2=len(" !")
				    total-length (+ total-length 2) )
			  ;; cut not at the end:
			  ;; rest-args = (<arg> ! <arg-2> ..)
			  (if (and foot-p (null (cdddr rest-args)))
			      ;; rest-args = (<arg> ! <foot>)
			      (setq post-sep " !&" ; 4=len(" !& ")
				    total-length (+ total-length 4) )
			      ;; no foot after cut:
			      (setq post-sep " !" ; =len(" ! ")
				    total-length (+ total-length 3) )))
		      (setq ignore-next-arg-p t) )
		     ((eq '|\|| next-arg)
                      ;; arg-list = (<arg-1> .. \| <arg-n>)
		      (incf total-length 3) ; 3 = len(" | ")
		      (setq post-sep " |")
		      (setq ignore-next-arg-p t) )
		     ((null (rest rest-args))
		      ;; last element:
		      (setq post-sep the-non-printing-char) )
		     ((or (and foot-p
			       (null (cddr rest-args)))
			  (and foot-p
			       (null (cdddr rest-args))
			       (eq '\! (caddr rest-args)) ))
		      ;; two versions of footed rule:
		      ;;     rest-args = (<arg> <foot>)
		      ;; or  rest-args = (<arg> <foot> !)
		      (incf total-length 3)	; 3=len(" & ")
		      (setq post-sep " &") )
		     (t
		      (incf total-length 2) ; 2 = len(", ")
		      (setq post-sep #\,) )) )))))


(defun construct-measure-tree-of-head (head)
  (cond ((com-cell-p head)
	 ;; the head has a comment:
	 (let ((pro-head
		(construct-measure-tree-of-head (com-cell-lisp head))))
	   (setf (pro-expr-pre-txt pro-head) (com-cell-pre-txt head)
		 (pro-expr-post-txt pro-head) (com-cell-post-txt head) )
	   pro-head ))
	((consp head)
	 ;; head = (functor arg-1 arg-2 .. )
	 (let ((pair (list
		      ;; functor & argument-list
		      (construct-measure-tree (first head) t)  
		      (construct-measure-tree-of-arg-list (cdr head) t) )))
	   (make-pro-expr
	    :type 'round-application
	    :len  (+ (pro-expr-len (first pair))
		     (pro-expr-len (second pair)) )
	    :value pair )))
	(t
	 ;; head = ATOM
	 (construct-measure-tree head nil) )))
  

(defun construct-measure-tree-of-clause (expr must-be-sqr-p)
  (declare (ignore must-be-sqr-p))
  (let* ((type-of-clause
	  (if (type-tag-eq expr 'hn)
	      ;; hornish-clause:
	      (if (= 2 (length expr))
		  'hn-fact		; (HN <head>)
		  'hn-rule )		; (HN <head> <b1> .. <bn>)
	      ;; footed-clause:
	      (if (or (= 3 (length expr))
		      (and (= 4 (length expr))
			   (eq '! (fourth expr)) ))
		  'ft-fact		; (FT <head> <b> [!])
		  'ft-rule )))		; (FT <head> <b1> .. <bn>)
	 (head (construct-measure-tree-of-head (second expr)))
	 (connector (case type-of-clause
		      (hn-fact "")
		      (hn-rule " :- ")
		      (ft-fact " :-& ")
		      (ft-rule " :- " )
		      (t (error
			  "Internal error: <construct-measure-tree-of-clause>"
			  ))))
	 (body
	  (construct-measure-tree-of-arg-list (cddr expr)
					      nil ; must-be-sqr
					      :foot-p (eq 'ft-rule
							  type-of-clause) )) 
	 )
    (decf (pro-expr-len body) 1)	; -1 = len(".") - len("()") 
    (make-pro-expr
     :type type-of-clause
     :value (list head connector body)
     :internal-p (or (pro-expr-internal-p head)
		     (pro-expr-internal-p body) )
     :len (+ (pro-expr-len head)
	     (pro-expr-len body)
	     (length connector) ))))


(defun construct-measure-tree-of-com-cell (expr must-be-sqr-p)
  ;; expr is a com-cell.
  (let ((tree (construct-measure-tree (com-cell-lisp expr)
				      must-be-sqr-p )))
    (setf (pro-expr-pre-txt tree) (com-cell-pre-txt expr)
	  (pro-expr-post-txt tree) (com-cell-post-txt expr)
	  (pro-expr-internal-p tree) (com-cell-internal-p expr) )
    tree
    ))


(defun pro-print (expr &optional (x-cursor 1))
  ; Prints expr in the Prolog-style syntax.
  ; Starts at x-cursor.
  ; Returns no value
  (pro-print-tree (construct-measure-tree expr nil) x-cursor))


;; All functions named pro-print-... return the position of the
;; cursor after printing the expression.


(defun pro-print-clause (expr x-cursor)
  ;; Printing of a clause:
  ;; 1.:  <head> <connector> <b1> .. <bn>.
  ;; 2.:  <head> <connector>
  ;;      _________<b1> .. <bn>.        (_ means space)
  ;; 3.:  <head>
  ;;      ____<connector> <b1> .. <bn>.
  (let* ((value (pro-expr-value expr))
	 (head (first value))
	 (connector (second value))
	 (body (third value))
	 )
    (pro-print-char
     #\.
     (if (and (space-enough-p (pro-expr-len expr) x-cursor)
	      (not (pro-expr-internal-p expr)) )
	 ;; fits in one line:
	 (pro-print-arg-list
	  body
	  (pro-print-string
	   connector
	   (pro-print-tree head x-cursor :try-single-line-p t))
	  0
	  the-non-printing-char
	  the-non-printing-char
	  :try-single-line-p t )
	 ;; not on a single line:
	 (let (new-cursor)
	   (cond ((string= "" connector)
		  ;; hn-fact ==> no connector
		  (setq new-cursor
			(pro-print-tree head x-cursor :try-single-line-p t) ))
		 ((space-enough-p head x-cursor (length connector))
		  ;; print as: <head> <connector>
		  (pro-print-tree head
				  x-cursor
				  :try-single-line-p t
				  :post-script connector )
		  (setq new-cursor
			(pro-newline-and-indent (+ 9 x-cursor))))
		 (t
		  ;; print as: <head>
		  ;;           ____ <connector> <b1> .. <bn>
		  (pro-print-tree head x-cursor :try-single-line-p t)
		  (setq new-cursor
			(pro-print-string
			 connector
			 (pro-newline-and-indent (+ x-cursor 4)) ))))
	   (pro-print-arg-list body
			       new-cursor
			       0	; don't go left
			       the-non-printing-char
			       the-non-printing-char
			       :try-single-line-p t ))))))


(defun pro-print-head (head &optional (x-cursor 1))
  (pro-print-tree (construct-measure-tree-of-head head) x-cursor) )
			    

(defun pro-print-tree (tree x-cursor &key post-script try-single-line-p)
  ;; post-script (e.g. #\,) is printed after tree but before
  ;; a possibly existing comment.
  (unless (string= "" (pro-expr-pre-txt tree))
    ;; print a leading comment (always type 2):
    (print-comment-type-2
     (pro-expr-pre-txt tree)
     (if (char= #\newline (char (pro-expr-pre-txt tree) 0)) 1 0)
     psyn-comment-lead-in
     x-cursor )
    (pro-newline-and-indent x-cursor) )
  (let ((new-x-cursor
	 (case (pro-expr-type tree)
	   (atomic
	    (pro-print-atom tree x-cursor) )
	   ((round-application sqr-application)
	    (pro-print-application tree
				   x-cursor
				   :try-single-line-p try-single-line-p ))
	   (is
	    (pro-print-is tree x-cursor) )
	   ((hn-fact hn-rule ft-fact ft-rule)
	    (pro-print-clause tree x-cursor) )
	   (variable-binding
	    (pro-print-variable-binding tree x-cursor) )
	   (t
	    (internal-error "Unknown tree-type in <pro-print-tree>"
			    (pro-expr-type tree) )))))
    (when post-script
      (setq new-x-cursor (pro-print-txt post-script new-x-cursor)) )
    (if (string= (pro-expr-post-txt tree) "") 
	;; no comment:
	new-x-cursor
	;; print the comment:
	(print-comment (pro-expr-post-txt tree)
		       psyn-comment-lead-in
		       new-x-cursor
		       x-cursor ))))


(defun pro-print-atom (tree x-cursor)
  (if (space-enough-p tree x-cursor)
      (progn (rf-princ-like-lisp (pro-expr-value tree))
             (+ x-cursor
                (pro-expr-len tree) ))
      (progn (rf-terpri)
             (rf-princ-like-lisp (pro-expr-value tree))
             (pro-expr-len tree)  )))


(defun pro-print-application (tree x-cursor &key try-single-line-p)
  ; type  = round-application or sqr-application
  ; value = (functor arg-list)
  (let ((functor  (first (pro-expr-value tree)))
        (arg-list (second (pro-expr-value tree))) 
        (x        x-cursor)
        (xx nil)
        (open-char nil)
        (close-char nil) )
    (case (pro-expr-type tree)
          (round-application (setq open-char round-left)
                             (setq close-char round-right) )
          (sqr-application (setq open-char sqr-left)
                           (setq close-char sqr-right) )
          (t (error "in pro-print-application.")) )
    (if functor
        ; it's not a list:
        (progn (if (space-enough-p functor x)
                   ; never break a functor
                   (setq x x-cursor)
                   (setq x (pro-newline-and-indent 1)) )
               (setq xx (pro-print-tree functor
					x
					:try-single-line-p try-single-line-p ))
	       (when (string/= "" (pro-expr-post-txt functor))
		 ;; functor has a comment ==> args require a fresh line:
		 (setq xx (pro-newline-and-indent (+ x 2)))) )
        ; it's a list:
        (setq xx x) )
    (pro-print-arg-list arg-list
                        xx
                        (+ x 2)
                        open-char
                        close-char
			:try-single-line-p try-single-line-p )))


(defun pro-print-arg-list
    (arg-list x-cursor x-cursor-left open-char close-char &key
	      try-single-line-p)
  (let* ((elements (pro-expr-value arg-list))
         (arg-list-info (get-arg-list-info elements))
         (nr-of-el (first arg-list-info))
         (max-len  (second arg-list-info))
         (simple-args-p (third arg-list-info))
	 (internal-comment-p (pro-expr-internal-p arg-list))
         )
    (cond ((and (or (<= nr-of-el max-args-per-line)
                    simple-args-p
		    try-single-line-p )
                (space-enough-p (pro-expr-len arg-list) x-cursor)
		(not internal-comment-p) )
           ;; functor(arg-1, arg2)
           (pro-print-char close-char
                           (pro-print-arg-list-elements
                            elements 
                            (pro-print-char open-char x-cursor)
                            nil  ; no extra-lines
			    :try-single-line-p try-single-line-p
			    )))
          ((or (space-enough-p max-len x-cursor 2)   ; 2 = length(sqr-left",")
               (<= x-cursor x-cursor-left)
	       try-single-line-p )
           ;; functor(  arg-1,
           ;;           arg-2 )
           (pro-print-char close-char
                           (pro-print-arg-list-elements
                            elements 
                            (pro-print-char open-char x-cursor)
                            t		; print on separated lines
                            )))
          (t
           ;; functor(
           ;;   arg-1,
           ;;   arg-2)
           (pro-print-char open-char x-cursor)
           (pro-print-char close-char
                           (pro-print-arg-list-elements
                            elements 
                            (pro-newline-and-indent x-cursor-left)
                            t  ; print on separated lines
                            ))))))


(defun pro-print-arg-list-elements
    (elements x-cursor extra-lines-p &key try-single-line-p)
  ;; each element is a triple:  (<pre-sep> <element-value> <post-sep>)
  ;;   <pre-sep> is usualy not used (that means NIL)
  ;;   <post-sep> is usualy #\,
  ;;            it is the-non-printing-char for the last element.
  (let ((new-x-cursor x-cursor)
	last-element-has-comment-p
	)
    (mapl #'(lambda (rest-elements &aux (triple (car rest-elements)))
	      (unless (null (first triple))
		(setq new-x-cursor
		      (pro-print-txt (first triple) new-x-cursor) ))
	      (setq new-x-cursor
		    (pro-print-tree (second triple)
				    new-x-cursor
				    :post-script (third triple)
				    :try-single-line-p try-single-line-p ))
	      (cond ((null (rest rest-elements))
		     ;; all elements printed ==> no space behind post-sep:
		     (setq last-element-has-comment-p
			   (string/= "" (pro-expr-post-txt (second triple)))))
		    (t
		     (setq new-x-cursor
			   (if extra-lines-p
			       (pro-newline-and-indent x-cursor)
			       (pro-print-char #\space new-x-cursor)
			       )))))
	  elements )
    (if last-element-has-comment-p
	;; last item was a comment ==> closing-char requires a fresh line:
	(pro-newline-and-indent x-cursor)
	new-x-cursor )))
	 

(defun pro-print-is (tree x-cursor)
  ;; type = is
  ;; value = (left-side right-side)
  (let ((x x-cursor)
        (left-side (first (pro-expr-value tree)))
        (right-side (second (pro-expr-value tree))) )
    (setq x (pro-print-tree left-side x))
    (if (comment-p left-side)
	;; last item was a comment, print as:
	;;   <left-side>
	;;   is <right-side>
	(pro-print-tree right-side
			(pro-print-string "is "
					  x 
					  (pro-newline-and-indent x-cursor) ))
	(pro-print-tree right-side
			(pro-print-string " is "
					  x 
					  x-cursor )))))


(defun pro-print-variable-binding (tree x-cursor)
  ; type = variable-binding
  ; value = (variable  binding-value)
  ; Assumes that x-cursor = 1.
  (let ((variable (first (pro-expr-value tree)))
        (binding-value (second (pro-expr-value tree))) )
    (pro-print-tree
     binding-value
     (pro-print-string " = "
                       (pro-print-tree variable x-cursor)
                       x-cursor ))) )


(defun pro-print-txt (char-or-string x-cursor)
  (if (characterp char-or-string)
      (pro-print-char char-or-string x-cursor)
      (pro-print-string char-or-string x-cursor) ))


(defun pro-print-char (char x-cursor)
  (if (char= char the-non-printing-char)
      x-cursor
      (case char
            ((#\. #\, #\space)
             (rf-princ-like-lisp char)
             (1+ x-cursor) )
            (t
             (if (space-enough-p 1 x-cursor)
                 (progn (rf-princ-like-lisp char)
                        (1+ x-cursor) )
                 (progn (pro-newline-and-indent 1)
                        (rf-princ-like-lisp char)
                        2 ))))))        ; 2 = Cursor column


(defun pro-print-string (str x-cursor &optional (x-cursor-left 1))
  ; Tries to print str on the current line.
  ; If not possible print on the next line at column x-cursor-left.
  (let ((str-wo-blank ""))
    (cond ((space-enough-p (length str) x-cursor)
           ; fits on line
           (rf-princ-like-lisp str)
           (+ x-cursor
              (length str) ))
          ((space-enough-p (length str) x-cursor-left)
           ; next line with indentation
           (pro-newline-and-indent x-cursor-left)
           (if (char= #\Space
                      (char str 0))
               (setq str-wo-blank (subseq str 1))
               (setq str-wo-blank str) )
           (rf-princ-like-lisp str-wo-blank) 
           (+ x-cursor-left
              (length str-wo-blank) ))
          (t
           ; next line without indentation
           (pro-newline-and-indent 1) 
           (if (char= #\Space
                      (char str 0))
               (setq str-wo-blank (subseq str 1))
               (setq str-wo-blank str) )
           (rf-princ-like-lisp str-wo-blank) 
           (+ x-cursor-left
              (length str-wo-blank) )))))


(defun pro-newline-and-indent (x)
  (rf-terpri)
  (dotimes (i (1- x) x) (rf-princ-like-lisp " ")) )


(defun space-enough-p (tree-or-number x-cursor &optional (extra-space 0))
  ; Note:  the last column is reserved for special characters, which
  ;        never should be printed at the begin of a line (e.g comma,
  ;        point).
  (>= (- *rf-print-width* x-cursor extra-space) 
      (if (pro-expr-p tree-or-number)
          (pro-expr-len tree-or-number)
          tree-or-number )))


(defun pro-get-variable (var)
  ; var = (VARI name level)
  ; where the level-field is optional.
  ; Example: var = (VARI time 127) ==> "Time:127"
  (let ((level (level-of var))
        (name (string-upcase
               (string-downcase (princ-to-string
                                 (second var)) )
               :start 0 
               :end 1 )))
    (if (not (null level))
        (setq name (strcat name ":" (princ-to-string level))) )
    (if (digit-char-p (character (subseq name 0 1)))
        (strcat "_" name)
        name )))


(defun lisp-var-sym->pro-var-sym (lisp-var-sym)
  ; Example:  input:   _xyz
  ;           output:  Xyz
  (let ((pro-name
         (nstring-upcase (nstring-downcase 
                          (subseq (princ-to-string lisp-var-sym) 1))
                         :start 0
                         :end 1 )))
    (if (digit-char-p (character (subseq pro-name 0 1)))
        (strcat "_" pro-name)
        pro-name )))


(defun get-arg-list-info (list-of-triples)
  ;; Returns a list.  1.: No. of elements
  ;;                  2.: Length of biggest element.
  ;;                  3.: simple-arg-p
  (do* ((n 0 (1+ n))
        (maxi 0 (max maxi 
                     (pro-expr-len element) ))
        (simple-arg-p t (and simple-arg-p
                             (or (eq 'atomic
                                     (pro-expr-type element) )
                                 (<= (pro-expr-len element) 
                                     max-len-of-a-simple-arg))))
        (l list-of-triples (cdr l))
	(triple (car l) (car l))
        (element (second triple) (second triple)  )
        )
       ((null l)
        (list n maxi simple-arg-p) )))
      

(defun type-tag-eq (expr tag-of-car)
  ;; <==> (car expr) is tag-of-car
  ;; Works with com-cells too.
  (cond ((com-cell-p expr)
	 (type-tag-eq (com-cell-lisp expr) tag-of-car) )
	((consp expr)
	 (eq (car expr) tag-of-car) )
	(t
	 nil )))


(defun pro-is-t (expr)
  ;; <==> IS is the car of expr.
  ;; Works with com-cells too.
  (type-tag-eq expr 'is) )


(defun pro-tup-t (expr)
  ;; <==> TUP is the car of expr.
  ;; Works with com-cells too.
  (type-tag-eq expr 'tup) )


(defun pro-clause-t (expr)
  ;; <==> HN or FT is the car of expr.
  ;; Works with com-cells too.
  (or (type-tag-eq expr 'hn)
      (type-tag-eq expr 'ft) ))


;; ----------------------------------------------------------------------------
;; End of Module Lisp2Pro.Lsp
;; ----------------------------------------------------------------------------

