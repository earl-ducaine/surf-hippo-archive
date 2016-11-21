;;; Copyright Notice
;;;
;;; This software is distributed for non-profit and research purposes only.
;;; Non-profit redistribution of the current version or parts of the
;;; current version is permitted if this copyright notice is included unchanged.
;;; I give no warranty of any kind for this prototype. It will be further
;;; improved as time permits.  
;;;
;;; Michael Herfert,  1992
 
 
; ----------------------------------------------------------------------------
;
; Module Pro2Lisp.Lsp:  Contains functions to read goals und clauses
;                       given in the Prolog-like syntax of Relfun.
;                       The input can be read from a string or from a 
;                       file of chararacters.
;                       The output contains the equivalent form in
;                       the Lisp-like syntax as a symbolic expression.
;
;                       If an error is detected a message is displayed
;                       an the value NIL is returned.
;
;
; Michael Herfert, 2/92
;
;
; Exported items:
;
;
;   pro-read-data-base  filename                                 [Function]
;
;       Assumes that the file contains clauses.
;       Returns a list of the clauses in Lisp-like syntax.
;
;
;   pro-read-goal  string                                        [Function]
;
;       Reads in a single goal from a string.
;       Note that there is no point at the end of a goal.
;
;
;   pro-read-clause  string                                      [Function]
;
;       Reads in a single clause from the string.
;
; 
;   pro-split-input  string                                      [Function]
;
;       Splits the given string in the first symbol and the rest
;       of it. Useful for recognizing system-commands.
;       Returns a pair.
;       Examples:  
;                (pro-split-input "consult my-base.rfp")
;          ==>   (consult . "my-base.rfp")
;
;                (pro-split-input "[a,b,c]")
;          ==>   (nil . "")


;; #. causes read-time-evaluation, important for the compiler.
;; (Steele p. 531/534)
#.
(set-dispatch-macro-character		; Steele, p. 546ff
 ;; #% sends a message to the scanner to look for a comment.
 #\#
 #\%
 '(lambda (stream subchar integer-arg)
   (declare (ignore subchar integer-arg)) ; subchar is #\%
   (list 'look-for-comment) ))


(defun set-syntax-pro (a-readtable)
  ; reading lists:
  ; (#'... not allowed on boards!!!)
  (set-macro-character #\| 'single-char-reader  nil a-readtable)
  (set-macro-character #\, 'single-char-reader  nil a-readtable)
  (set-macro-character #\' 'single-char-reader  nil a-readtable)
  (set-macro-character #\` 'single-char-reader  nil a-readtable)
  (set-syntax-from-char #\_ #\A a-readtable nil)
  a-readtable )


(defun single-char-reader (stream char)
  (declare (ignore stream))
  char )


(defvar *rfi-readtable-pro* 
  ; this is a read-only variable
  (set-syntax-pro (copy-readtable nil)) )  ; nil --> copy of std. readtable


(defvar *extra-parsing-fcts*
  nil
  ;; used in next release 
  ;;(list (cons 'inst 'parse-inst))
  "Assoc-list of symbols requiring special parsing.
   Special parsing implies these symbols can not be used for meta-calls.
   The functions have always this arg-list:  (scanner &optional auto-inst-p)
  "
  )


;; Easy life for the editor:
(defconstant sqr-left #\[)
(defconstant sqr-right #\])
(defconstant round-left #\()
(defconstant round-right #\))
(defconstant comma #\,)
(defconstant bar #\|)
(defconstant eof-char (code-char 4))


;; --------------------------  M a c r o s  -----------------------------------


(defmacro accept-token-type (tok-type)
  ;; Signals an error, if the wrong token is found,
  ;; otherwise it reads in the next token.
  `(if (eq (funcall scanner 'last-token-type)
	,tok-type)
    (funcall scanner) 
    (signal-error scanner 
     (format nil "Expected: ~A~%Found:    "
      (tok-type->string ,tok-type) )
     (funcall scanner 'last-token) )))


(defmacro strcat (&rest strings)
  `(concatenate 'string
                ,@strings ))


(defmacro append* (var-sym a-list)
  ;; easy to type
  `(setq ,var-sym (append ,var-sym ,a-list)) )


(defmacro let-assoc ((fct-id item assoc-list) then-form else-form)
  ;; assoc-list = ( (key-1 . fct-1) (key-2 . fct-2) .. )
  ;; If item is in assoc-list then the corresponding function is called
  ;; with then-args as arguments.
  ;; Else then-args are not evaluated and else-form is executed.
  ;; Example:
  ;;             (let-assoc (g symbol *extra-parsing-fcts*)
  ;;                (g scanner nil)
  ;;                (parse-round-list scanner nil) )
  `(let ((pair (assoc ,item ,assoc-list)))
    (if (null pair)
	,else-form
	(flet ((,fct-id (&rest args)
		 (apply (symbol-function (cdr pair)) args)))
	  ,then-form ))))


;; ----------------------------------------------------------------------------


(defun signal-error (scanner &rest msg-s)
  ; Prints all arguments and the error-position via the scanner.
  (pro-print-error scanner msg-s)
  (throw :pro-read-error-tag nil) )   ; nil signals an error


(defun pro-print-error (scanner msg-s)
  ; msg-s = (msg-1 msg-2 ..)
  (rf-terpri)
  (let ((x (token-x-pos (funcall scanner 'last-token)))
        (y (token-y-pos (funcall scanner 'last-token)))
        (last-line (funcall scanner 'last-line))
        (act-line (funcall scanner 'act-line))
        )
    (rf-princ-like-lisp (format nil 
                                "Error near line ~A, column ~A.~%"
                                y
                                x ))
    (mapcar #'(lambda (msg)
                (if (token-p msg)
                    (if (token-value msg)
                        (rf-princ-like-lisp (token-value msg))
                        (rf-princ-like-lisp (tok-type->string
                                             (token-type msg) )))
                    (rf-princ-like-lisp msg) ))
            msg-s )
    (rf-terpri)
    (if (> y 1)
        (rf-princ-like-lisp last-line) )
    (rf-terpri)
    (rf-princ-like-lisp act-line)
    (rf-terpri) 
    (rf-terpri) ))


(defun tok-type->string (tok-type)
  (case tok-type
        (round-left (string round-left))
        (round-right (string round-right))
        (sqr-left (string sqr-left))
        (sqr-right (string sqr-right))
        (comma (string comma))
        (bar (string bar))
        (ampersand "&")
        (implies ":-")
        (point ".")
        (is "is")
        (empty "End of input")
        (t (princ-to-string tok-type)) ))
                   


;; The scanner returns a value of this type:
(defstruct token
  type    ; dom = {constant, variable, 
          ;        round-left, round-right, sqr-left, square-right
          ;        point, comma, ampersand, implies,
          ;        is, empty}
  value   ; used with number, constant, variable.
  x-pos   ; to report an error-position
  y-pos
)


(defun not-equal (x y) (not (eq x y)) )


(defun gen-scanner (the-input-stream)
  ;; Returns a scanner-function.
  ;; The scanner could be seen as an object in the view of OOP.
  ;; A very big function, but there are many local functions defined by LABELS.
  (let ((x-pos 0)
        (y-pos 1)
        (char-pos -1)
        (last-ch #\Space) 
        (last-token (make-token :x-pos 1 :y-pos 1 :type 'empty))
        (last-line "")
	(comment "")
        (act-line "1:  ")
	(str-of-newlines "")
	(stacked-tokens nil)	; used e.g. for expanding "!-" to ":-" and "!"
	(special-tokens
	 ;; special-tokens = (triple-1 .. triple-n)
	 ;; triple = (token-as-string token-type stacked-tokens)
	 '((":-" implies nil)
	   (":-&" implies (ampersand))
	   (":-!&" implies (cut ampersand))
	   (":-!" implies (cut))
	   ("!&" cut (ampersand))
	   ("!-" implies (cut))
	   ("!-&" implies (cut ampersand))
	   ("!" cut nil)
	   ("&" ampersand nil)
	   ))
        ;; terminating characters:
        ;; (cons <char> <type>)
        (terminating-chars (list (cons sqr-left 'sqr-left)
                                 (cons sqr-right 'sqr-right)
                                 (cons round-left 'round-left)
                                 (cons round-right 'round-right)
                                 (cons #\. 'point)
                                 (cons #\, 'comma) 
                                 (cons #\| 'bar)
				 ;(cons #\! 'cut)
                                 ;(cons #\& 'ampersand)
				 )))
    (labels
	((get-ch () 
	   (if (char/= last-ch eof-char)
	       (progn
		 (setq last-ch (read-char the-input-stream 
					  nil 
					  eof-char ))
		 (setq char-pos (1+ char-pos))
		 (cond ((char= last-ch #\Newline)
			(setq x-pos 1)
			(setq y-pos (1+ y-pos)) 
			(setq last-line act-line)
			(setq act-line (strcat (princ-to-string y-pos)
					       ":  " )))
		       (t
			(setq act-line (strcat act-line (string last-ch)))
			(setq x-pos (1+ x-pos))) )))
	   last-ch )
	 (scan-item (continue-p)
	   (let ((r ""))
	     (loop (if (and (char/= eof-char last-ch)
			    (funcall continue-p last-ch) )
		       (setq r (strcat r (string last-ch)))
		       (return r) )
		   (get-ch) )))
	 (psyn-comment-type-1-reader ()
	   ;; reads in the comment and all comments immedeatly following.
	   (let ((triple (read-comments psyn-comment-lead-in
					1 ; type of first comment
					the-input-stream)))
	     (setq comment (strcat comment (first triple))
		   y-pos (+ y-pos (second triple))
		   x-pos (third triple) )))
	 (psyn-comment-type-2-reader ()
	   ;; called at the end of a line to check if the next line
	   ;; begins with a comment.
	   ;; On entry (read-char the-input-stream) returns the first
	   ;; char after #\newline.
	   (unread-char #\newline the-input-stream)
	   (let* ((tuple (read-until-black the-input-stream))
		  (non-white-char (first tuple))
		  (triple (if (char= psyn-comment-lead-in non-white-char)
			      (read-comments psyn-comment-lead-in
					     2
					     the-input-stream)
			      nil ))
		  )
	     (setq str-of-newlines (second tuple))
	     (if triple
		 ;; comment detected:
		 (progn 
			;(princ "non-w:") (princ non-white-char) (terpri)
			(setq comment
			      (strcat str-of-newlines
				      (subseq (first triple) 1) ))
			;(princ "comment=") (princ comment) (princ"<") (terpri)
			)
		 ;; no comment at begin of line:
		 (setq x-pos 0) )
	     ))
	 (id-char-p (ch)
	   (cond ((both-case-p ch) t)
		 ((digit-char-p ch) t)
		 (t (member ch '(#\+ #\- #\* #\/ #\_ #\< #\>
				 #\= 
				 )
			    :test #'char=)) ))
	 (special-char-p (ch)
	   (cond ((assoc ch terminating-chars :test #'char=)
		  nil )
		 ((member ch '(#\; #\' #\`) :test #'char=)
		  nil )
		 ((char<= ch #\Space) nil)
		 ((char> ch (int-char 127)) nil)
		 ((digit-char-p ch) nil)
		 ((both-case-p ch) nil)
		 (t t) ))
	 (string-constituent-p (ch)
	   (and (not (char= #\" ch))
		(not (char= #\Newline ch)) ))
	 (scan-whitespace ()
	   (do ((abort nil))
	       (abort)
	     (case last-ch
	       (#\Space (get-ch)) 
	       (#\Newline (if (null *comment-style*)
			      nil
			      (psyn-comment-type-2-reader) )
			  (get-ch)	; last-ch:= non-comment-char
			  )
	       (#\Tab (get-ch)) 
	       (#\%
		(if (null *comment-style*)
		    ;; ignore until end of line:
		    (do ((abort nil))	; comment
			(abort)
		      (setq abort (char=
				   last-ch
				   #\Newline))
		      (get-ch) )
		    ;; read in the type-1-comment:
		    (progn (psyn-comment-type-1-reader)
			   (get-ch)	; last-ch := non-comment-char
			   )))
	       (t (setq abort t)))))
	 (scan-digits ()
	   ;; digits := digit {digit}
	   ;; returns a string of digits
	   (if (digit-char-p last-ch)
	       (scan-item #'digit-char-p)
	       (signal-error #'scanner
			     (format nil 
				     "Expected:  digit~%Found:     ~C"
				     last-ch ))))
	 (scan-integer ()
	   ;; integer := ["+"l"-"] digits
	   ;; returns a string
	   (let ((sign (case last-ch
			 (#\+ (get-ch)
			      "+")
			 (#\- (get-ch)
			      "-")
			 (t "" ))))
	     (strcat sign (scan-digits)) ))
	 (scan-real ()
	   ;; real :=  integer ["." digits]
	   ;;          ["E" integer]
	   ;; returns a number
	   (let* ((left-side (scan-integer))
		  (right-side (if (and (char= #\. last-ch)
				       (digit-char-p (peek-char
						      nil
						      the-input-stream
						      nil
						      eof-char)))
				  (progn (get-ch)
					 (strcat "." (scan-integer)) )
				  "" ))
		  (exponent (case last-ch 
			      ((#\E)
			       (get-ch)
			       (strcat "E" (scan-integer)) )
			      (t "") )) )
	     (read-from-string (strcat left-side
				       right-side
				       exponent ))))
	 (next-token ()
	   (let ((r nil)
		 (*readtable* *rfi-readtable-pro*) ) ; dynamic variable
	     (if stacked-tokens
		 (setq r (make-token :type (car stacked-tokens)
				     :x-pos x-pos
				     :y-pos y-pos )
		       stacked-tokens (cdr stacked-tokens) )
		 (progn
		   (scan-whitespace)
		   (setq r (make-token :x-pos x-pos :y-pos y-pos))
		   (cond  ((digit-char-p last-ch)
			   (setf (token-type r) 'constant)
			   (if (char= #\1 last-ch)
			       ;; check for functors 1- and 1+
			       (case (peek-char nil the-input-stream
						nil eof-char)
				 (#\+ (setf (token-value r) '1+)
				      (get-ch) (get-ch) )
				 (#\- (setf (token-value r) '1-)
				      (get-ch) (get-ch) )
				 (t   (setf (token-value r) 
					    (scan-real) )))
			       (setf (token-value r) (scan-real))))
			  ((lower-case-p last-ch)
			   (setf (token-value r)
				 (read-from-string (scan-item
						    #'id-char-p)) )
			   (if (eq 'is (token-value r))
			       (setf (token-type r) 'is
				     (token-value r) nil ) 
			       (setf (token-type r) 'constant) ))
			  ((upper-case-p last-ch)
			   (setf (token-type r) 'variable)
			   (setf (token-value r)
				 (read-from-string (scan-item
						    #'id-char-p)) ))
			  ((char= #\_ last-ch)
			   (get-ch)	; read the underscore
			   (cond ((digit-char-p last-ch)
				  (setf (token-type r)
					'variable
					(token-value r)
					(intern (scan-digits))))  
				 ((lower-case-p last-ch)
				  (setf (token-type r)
					'variable
					(token-value r)
					(read-from-string (scan-item
							   #'id-char-p ))))
				 ((upper-case-p last-ch)
				  (signal-error 
				   #'scanner 
				   "Uppercase-letter not allowed after _"))
				 (t	; anonymous variable
				  (setf (token-type r)
					'variable
					(token-value r)
					'_ ))))
			  ((char= #\" last-ch)
			   ;;(setf (token-type r) 'string)
			   (setf (token-type r) 'constant)
			   (get-ch)	; read opening quote
			   (setf (token-value r)
				 (scan-item #'string-constituent-p) )
			   (if (char= #\" last-ch)
			       (get-ch)	; read closing quote
			       (signal-error #'scanner
					     (format nil 
						     "Expected: \"~%Found: ~C"
						     last-ch ))))
			  ((or (char= #\+ last-ch)
			       (char= #\- last-ch) )
			   (cond ((digit-char-p (peek-char 
						 nil the-input-stream
						 nil eof-char))
				  ;;(setf (token-type r) 'number) 
				  (setf (token-type r) 'constant
					(token-value r) (scan-real) ))
				 (t 
				  (setf (token-type r) 'constant
					(token-value r) (read-from-string
							 (scan-item
							  #'id-char-p))))))
			  ((special-char-p last-ch)
			   ;; read in a max. sequence of special-chars
			   (let* ((str (scan-item #'special-char-p))
				  (triple (car (member str
						       special-tokens
						       :test #'string=
						       :key #'car ))))
			     (if triple
				 (setf (token-type r) (second triple)
				       stacked-tokens (third triple) )
				 (setf (token-type r) 'constant
				       (token-value r) (read-from-string str) )
				 )))
			  ((char= eof-char last-ch)
			   (setf (token-type r) 'empty) )
			  (t (let ((pair-of-char-and-type
				    (assoc last-ch terminating-chars) ))
			       (if pair-of-char-and-type
				   (progn
				     (setf (token-type r) 
					   (cdr pair-of-char-and-type))
				     (get-ch) )
				   (signal-error #'scanner 
						 (format
						  nil
						  "Illegal character: ~C"
						  last-ch ))))))))
	     (setq last-token r)
	     r ))
	 (continue-after-error ()
	   ;; searches for the next token after a point.
	   (setq stacked-tokens nil)
	   (when *comment-style*
	     (setq last-ch (peek-char nil the-input-stream nil eof-char)) )
	   (loop
	    (case (token-type last-token)
	      (point (return (next-token)))
	      (empty (return last-token))
	      (t (next-token)) ))
	   )
	 (scanner (&optional (message 'next-token))
	   (case message
	     (next-token (next-token))
	     (last-token last-token)
	     (last-token-type (token-type last-token))
	     (last-token-value (token-value last-token))
	     (comment-p (string/= "" comment))
	     (comment (prog1 comment (setq comment "")))
	     (str-of-newlines (prog1 str-of-newlines
				(setq str-of-newlines "") ))
	     (x-pos x-pos)
	     (y-pos y-pos)
	     (last-line last-line)
	     (act-line  act-line)
	     (last-char last-ch)
	     (continue-after-error (continue-after-error))
	     (pos-of-first-non-white (scan-whitespace)
				     char-pos )
	     (t  (internal-error "unknown message in scanner"
				 message)) )))
					; init variables:
      (get-ch)
      (next-token)
      #'scanner				; value of gen-scanner-from-fct
      )))


;; Following two meta-rules to obtain a clear structure:

(defun parse-general-loop ( scanner syntax-rule
                           first-of-syntax-rule 
                           &optional auto-inst-p )
  ; Parses a construct of the form:
  ;   syntax-rule {syntax-rule}
  ; where  first(syntax-rule) = first-of-syntax-rule.
  ; syntax-rule is a parser-function, first-of-syntax-rule is a token-type.
  ; Returns a list of the results
  (do ((l (list (funcall syntax-rule scanner auto-inst-p))
          (append l (list (funcall syntax-rule scanner auto-inst-p))) ))
      ((not-equal first-of-syntax-rule
		  (funcall scanner 'last-token-type) )
       l) ))


(defun parse-general-enumeration (scanner syntax-rule &optional auto-inst-p)
  ;; Parses a construct of the form:
  ;;   syntax-rule {"," syntax-rule}
  ;; where syntax-rule is a parser-function.
  ;; Returns a list of the results.
  ;; Comments: on entry: yes;  on exit: no;
  (do* ((list-of-items (clist #%) (append list-of-items (clist item #%)))
	(item (funcall syntax-rule scanner auto-inst-p)
	      (funcall syntax-rule scanner auto-inst-p) ))
       ((not-equal (token-type (funcall scanner 'last-token))
		   'comma)
	(append list-of-items (clist item #%)) )
    (accept-token-type 'comma) ))


(defun parse-clause (scanner)
  ;; clause ::= head  (   "."
  ;;                   \|  ":-" [body] "."
  ;;                  )
  ;; Comments: on entry: no;  on exit: yes.
  (let* ((head      (parse-head scanner))
         (tok-type  (funcall scanner 'last-token-type)) )
    (funcall scanner)  ; accept the symbol between head and body
    (case tok-type
          (point     (clist 'hn head )) ; horn-fact
          (implies   (if (eq 'point (funcall scanner 'last-token-type)) ;rule
                         ;; empty body:
                         (prog1 (clist 'hn head #%) ; comment after head
			   (accept-token-type 'point) )
                         (prog1 (parse-the-body scanner head)
			   (accept-token-type 'point) )))
          (t      (signal-error scanner
                                "Unknown symbol between head and body: "
                                tok-type )))))


(defun parse-head (scanner)
  ;; head ::= term round-list-with-terms
  ;; Comments: on entry: no;  on exit: yes.
  (cons (parse-term scanner t)   ; t means auto-inst-p
        (parse-round-list-with-terms scanner) ))


(defun parse-the-body (scanner head)
  ;; On entry: body is non-empty.
  ;; body ::=     [expr+] (   "!" [expr+] ["&" expr]
  ;;                      \|  "&" expr ["!"]
  ;;                      )
  ;; Returns a horn- or a footed-rule.
  ;; Note: PARSE-BODY is a reserved symbol in LUCID-Lisp.
  (let ((type-of-clause 'hn)
	(comment-after-head #%)
	(body nil) )
    (case (funcall scanner 'last-token-type)
      ((cut ampersand))
      (t (setq body (parse-expr+ scanner))) ) ; parse-expr+ accepts comments
    (case (funcall scanner 'last-token-type)
      (cut (accept-token-type 'cut)
	   (setq body (append body (clist '\! #%)))
	   (case (funcall scanner 'last-token-type)
	     ((ampersand point))
	     (t (setq body (append body (parse-expr+ scanner)))) )
	   (when (eq 'ampersand (funcall scanner 'last-token-type))
	     (accept-token-type 'ampersand)
	     (setq body (append body (clist #% (parse-expr scanner) #%))
		   type-of-clause 'ft )))
      (ampersand (accept-token-type 'ampersand)
		 (setq body (append body (clist #% (parse-expr scanner) #%))
		       type-of-clause 'ft )
		 (when (eq 'cut (funcall scanner 'last-token-type))
		   (accept-token-type 'cut)
		   (setq body (append body (clist #% '\! #%))) ))
      (t) )
    (cons type-of-clause
	  (append (clist head comment-after-head) body) )))    


(defun parse-expr (scanner &optional auto-inst-p)
  ;; expr ::=  (  term [   {round-list}
  ;;                    |  IS expr
  ;;                   ]
  ;;            | builtin
  ;;           )
  ;; Comments: on entry: no;  on exit: yes.
  ;;(declare (ignore auto-inst-p))
  (let ((r (parse-term scanner)))
    (case (funcall scanner 'last-token-type)
      (is (accept-token-type 'is)
	  (clist 'is (remove-non-var-inst r) #% (parse-expr scanner) #%))
      (round-left (let-assoc (parser-fct r *extra-parsing-fcts*)
			     (parser-fct scanner auto-inst-p)
			     (construct-application
			      r
			      (parse-general-loop scanner 
						  #'parse-round-list
						  'round-left ))))
      (t    r) )))


(defun parse-term (scanner &optional auto-inst-p)
  ;; term ::=  (  CONSTANT 
  ;;            | VARIABLE
  ;;            | sqr-list
  ;;           )
  ;;           {sqr-list}
  ;; If auto-inst-p is t then no explicit inst-tag is generated.
  ;; Needed for terms in the head of a rule and for nested lists.
  ;; Comments: on entry: no;  on exit: yes.
  (let ((tok (funcall scanner 'last-token))
        (l nil)
        (sqr-left-p nil)
        )
    (case (token-type tok)
      (constant (setq l (token-value tok))
		(accept-token-type 'constant))
      (variable (setq l (parse-variable scanner)))
      (sqr-left (setq l (cons 'tup (parse-sqr-list scanner t))
		      sqr-left-p t ))
      (t        (signal-error scanner
			      (format nil 
				      (strcat
				       "Constant, variable or [ expected."
				       "~%Found: " ))
			      tok )))
    (if (eq 'sqr-left (funcall scanner 'last-token-type))
        (setq l (construct-application
                 l			; comment after sqr-left:
                 (parse-general-loop scanner 
				     #'parse-sqr-list
				     'sqr-left
				     t ) )
              sqr-left-p t ))
    (if (and (not auto-inst-p)
             sqr-left-p )
        (setq l (list 'inst l)) )
    l ))


(defun parse-round-list (scanner &optional auto-inst-p)
  ;; round-list ::= "(" [expr+] ["|" expr] ")"
  ;; Comments: on entry: yes;  on exit: yes;
  (accept-token-type 'round-left)
  (let ((l (clist #%)))			; no comment ==> l is empty
    (case (funcall scanner 'last-token-type)
      (bar )
      (round-right )
      (t (setq l (append l (parse-expr+ scanner)))) )
    (when (eq 'bar (funcall scanner 'last-token-type))
      (setq l
	    (append l
		    (clist #%
			   '\|
			   (progn (accept-token-type 'bar) #%)
			   (parse-expr scanner auto-inst-p)) )))
    (accept-token-type 'round-right)
    l ))


(defun parse-sqr-list (scanner &optional auto-inst-p)
  ;; sqr-list ::= "[" [term+] ["|" term] "]"
  ;; Comments: on entry: yes;  on exit: yes;
  (accept-token-type 'sqr-left)
  (let ((l (clist #%)))			; no comment ==> l is empty
    (case (funcall scanner 'last-token-type)
      (bar )
      (sqr-right )
      (t (setq l (append l (parse-term+ scanner auto-inst-p)))) )
    (when (eq 'bar (funcall scanner 'last-token-type))
      (setq l
	    (append l
		    (clist #%
			   '\|
			   (progn (accept-token-type 'bar) #%)
			   (parse-term scanner auto-inst-p) ))))
    (accept-token-type 'sqr-right)
    l ))


(defun parse-round-list-with-terms (scanner)
  ;; round-list-with-terms ::= "(" [term+] ["|" VARIABLE] ")"
  ;; Comments: on entry: yes;  on exit: yes;
  (accept-token-type 'round-left)
  (let ((l (clist #%)))			; no comment ==> l is empty
    (case (funcall scanner 'last-token-type)
      (bar )
      (round-right )
      (t (setq l (append l (parse-term+ scanner t)))) )
    (when (eq 'bar (funcall scanner 'last-token-type))
      (setq l (append l
		      (clist #%
			     '\|
			     (progn (accept-token-type 'bar) #%)
			     (parse-variable scanner)) )))
    (accept-token-type 'round-right)
    l ))


(defun parse-expr+ (scanner &optional auto-inst-p)
  ;; expr+ ::= expr {"," expr}
  ;; Returns a list of results.
  ;; auto-inst-p is not used in this function.
  ;; Comments: on entry: yes;  on exit: no;
  (declare (ignore auto-inst-p))
  (parse-general-enumeration scanner #'parse-expr) )


(defun parse-term+ (scanner &optional auto-inst-p)
  ;; term+ ::= term {"," term}
  ; Returns a list of results.
  ;; Comments: on entry: yes;  on exit: no;
  (parse-general-enumeration scanner #'parse-term auto-inst-p) )


(defun parse-variable (scanner)
  ;; Comments: on entry: yes;  on exit: no;
  (let ((var-name (funcall scanner 'last-token-value)))
    (accept-token-type 'variable)
    (if (eq '_ var-name)
        'id				; anonymous variable
	(list 'vari var-name) )))
         
    
(defun construct-application (functor list-of-arg-lists)
  ; Example:  functor = f
  ;           list-of-args-lists = ((a b) (x y))
  ;           Result: ((f a b) x y)
  (if (null list-of-arg-lists)
      functor
      (construct-application (cons functor
                                   (car list-of-arg-lists) )
                             (cdr list-of-arg-lists) )))


(defun remove-non-var-inst (term)
  ;; removes a possibly existing INST-tag if is not followed by a variable.
  (if (and (consp term)
	   (inst-t term)
	   (not (vari-t (second term))) )
      (second term) ; remove inst
      term ))


(defun clist (&rest args)
  ;; Like list but takes care of comments:
  ;; If one of the args is a com-cell with empty string-fields
  ;; then it will be not included in the list.
  (let (the-list)
    (dolist (element args (reverse the-list))
      (unless (and (com-cell-p element)
		   (or (null *comment-style*)
		       (and (string= "" (com-cell-pre-txt element))
			    (string= "" (com-cell-post-txt element))
			    )))
	;; don't ignore the element:
	(setq the-list (cons element the-list)) ))))


(defun pro-parse-head (head-as-string)
  (with-input-from-string (the-input-stream head-as-string)
    (catch :pro-read-error-tag
      (parse-head (gen-scanner the-input-stream)) )))



;; --------------------  Exported functions:  ---------------------------------



(defun pro-read-data-base (filename)
  (with-open-file (the-input-stream filename :direction :input)
    (pro-read-data-base-from-stream the-input-stream) ))
    

(defun pro-read-data-base-from-stream (the-input-stream)
  (catch :pro-read-error-tag
    (let ((scanner (gen-scanner the-input-stream))
	  (clause nil)
	  (data-base nil)
	  (error-p nil) )
      (loop
       (if (eq 'empty (funcall scanner 'last-token-type))
	   (if error-p
	       (return nil)
	       (return (reverse data-base)) ))
       (setq clause
	     (catch :pro-read-error-tag
	       (parse-clause scanner) ))
       (cond ((null clause)  
	      ;; Error 
	      (setq error-p t)
	      (rf-terpri)
	      (rf-princ-like-lisp "Continue reading to find more")
	      (rf-princ-like-lisp " errors in line ")
	      (funcall scanner 'continue-after-error)
	      (rf-princ-like-lisp (token-y-pos (funcall scanner
							'last-token )))
	      (rf-princ-like-lisp ".")
	      (rf-terpri) )
	     (error-p
	      ;; Error in previous clause --> don't construct database
	      )
	     (t
	      ;; No error
	      (setq data-base (cons clause data-base)) ))))))

  
(defun pro-read-clause (str)
  (catch :pro-read-error-tag
    (with-input-from-string 
     (the-input-stream str)
     (let* ((scanner (gen-scanner the-input-stream))
            (clause (parse-clause scanner)) )
       (cond ((null clause)
              ;; Error
              nil )
             ((eq 'empty (funcall scanner 'last-token-type))
              ;; all of the input is o.k.
              (if *comment-style*
		  (hide-comments clause)
		  clause ))
             (t
              (pro-print-error
               scanner
               (list "Only the first part of the input is correct."))
              nil ))))))


(defun pro-read-goal (str)
  ; goal ::= expr {"," expr}
  (catch :pro-read-error-tag
    (with-input-from-string 
     (the-input-stream str)
     (let* ((scanner (gen-scanner the-input-stream))
            (goal (parse-general-enumeration scanner #'parse-expr)) )
       (cond ((null goal)
              ;; Error
              nil )
             ((eq 'empty (funcall scanner 'last-token-type))
              ;; all of the input is o.k.
	      (if *comment-style*
		  (hide-comments goal)
		  goal ))
             ((eq 'point (funcall scanner 'last-token-type))
              (pro-print-error
               scanner
               (list "Illegal point at the end of a goal.") )
              nil )
             (t
              (pro-print-error
               scanner
               (list "Only the first part of the input is correct.") )
              nil ))))))


(defun pro-split-input (input-line)
  ; Examples:  
  ;   
  ;        (pro-split-input "consult my-base.rfp")
  ;  ==>   (consult . "my-base.rfp")
  ;
  ;        (pro-split-input "[a,b,c]")
  ;  ==>   (nil . "")
  ;
  (catch :pro-read-error-tag
    (with-input-from-string 
     (the-input-stream input-line)
     (let* ((scanner (gen-scanner the-input-stream))
            (tok     (funcall scanner 'last-token)) )
       (if (and (eq 'constant 
                    (token-type tok) )
                (symbolp (token-value tok)) )
           (cons (token-value tok)
                 (subseq input-line 
                         (funcall scanner 
                                  'pos-of-first-non-white) ))
         (cons nil "") )))))

           
 
;; ----------------------------------------------------------------------------
;; End of module Pro2Lisp.Lsp
;; ----------------------------------------------------------------------------
