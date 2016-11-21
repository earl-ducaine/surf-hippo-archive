
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
;       of it. Useful for recognizing rfi-commands.
;       Returns a pair.
;       Examples:  
;                (pro-split-input "consult my-base.rfp")
;          ==>   (consult . "my-base.rfp")
;
;                (pro-split-input "[a,b,c]")
;          ==>   (nil . "")





(defun set-syntax-pro (a-readtable)
  ; reading lists:

;;; #'<fct_name> had to be changed to '<fct_name>
;;; because of an internal symbolics error 
;;;   --> see rfi.lsp:rfi-set-syntax

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


; Easy life for the editor:
(defconstant sqr-left #\[)
(defconstant sqr-right #\])
(defconstant round-left #\()
(defconstant round-right #\))
(defconstant comma #\,)
(defconstant bar #\|)
(defconstant eof-char (code-char 4))


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


(defun not-eq (x y)
  (not (eq x y)) )


(defmacro accept-token-type (tok-type)
  ;Signals an error, if the wrong token is found,
  ;otherwise it reads in the next token.
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


(defun gen-scanner (the-input-stream)
  ; Returns a scanner-function.
  ; The scanner could be seen as an object in the view of OOP.
  ; A very big function, but there are many local functions defined by LABELS.
  (let ((x-pos 0)
        (y-pos 1)
        (char-pos -1)
        (last-ch #\Space) 
        (last-token (make-token :x-pos 1 :y-pos 1 :type 'empty))
        (last-line "")
        (act-line "1:  ") 
        ;; terminating characters:
        ;; (cons <char> <type>)
        (terminating-chars (list (cons sqr-left 'sqr-left)
                                 (cons sqr-right 'sqr-right)
                                 (cons round-left 'round-left)
                                 (cons round-right 'round-right)
                                 (cons #\. 'point)
                                 (cons #\, 'comma) 
                                 (cons #\| 'bar)
                                 (cons #\& 'ampersand) )))
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
              (format t "~%get-ch: <~C>~%" last-ch)
	      last-ch
	      )
      (scan-item (continue-p)
                 (let ((r ""))
                   (loop (if (and (char/= eof-char last-ch)
                                  (funcall continue-p last-ch) )
                             (setq r (strcat r (string last-ch)))
                             (return r) )
                         (get-ch) )))
      (id-char-p (ch)
                 (cond ((both-case-p ch) t)
                       ((digit-char-p ch) t)
                       (t (member ch '(#\+ #\- #\* #\/ #\_ #\! #\< #\>
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
                    ;; integer := ["+" "-"] digits
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
                            (get-ch)  ; read the underscore
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
                                  (t ; anonymous variable
                                   (setf (token-type r)
                                         'variable
                                         (token-value r)
                                         '_ ))))
                           ((char= #\" last-ch)
                            ;;(setf (token-type r) 'string)
                            (setf (token-type r) 'constant)
                            (get-ch)   ; read opening quote
                            (setf (token-value r)
                                  (scan-item #'string-constituent-p) )
                            (if (char= #\" last-ch)
                                (get-ch)    ; read closing quote
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
                            (let ((str (scan-item #'special-char-p)))
                              (if (string= ":-" str)
                                  (setf (token-type r) 'implies)
                                (setf (token-type r) 'constant
                                      (token-value r) (read-from-string str)
                                      ))))
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
                                                (format nil
                                                        "Illegal character: ~C"
                                                        last-ch ))))))
                    (setq last-token r)
                    r ))
      (continue-after-error ()
                            ;; searches for the next token after a point.
                            (loop
                             (case (token-type last-token)
                                   (point (return (next-token)))
                                   (empty (return last-token))
                                   (t (next-token)) )))
      (scanner (&optional (message 'next-token))
	       (format t "~%Scanner: message=~A, last-ch=<~C>" message last-ch)
               (case message
                     (next-token (next-token))
                     (last-token last-token)
                     (last-token-type (token-type last-token))
                     (last-token-value (token-value last-token))
                     (x-pos x-pos)
                     (y-pos y-pos)
                     (last-line last-line)
                     (act-line  act-line)
                     (continue-after-error (continue-after-error))
                     ((pos-of-first-non-white) 
		      (progn (print "lbl: pos-of-first..")
			     (scan-whitespace)
			     (print "lbl: pos-of-first: Ende.")
			     char-pos ))
                     (t  (print "Internal Error.")) ))
      (scan-whitespace ()
		       (print "Hi.")
		       (format t "~A <~C>~%" "Hier ist scan-whitespace." last-ch)
                       (do ((abort1 nil))
                           (abort1)
                           (case last-ch
                                 (#\Space (get-ch)) 
                                 (#\Newline (get-ch))
                                 (#\Tab (get-ch)) 
                                 (#\%  (do ((abort2 nil)) ; comment
                                           (abort2)
                                           (setq abort2 (char=
                                                        last-ch
                                                        #\Newline))
                                           (get-ch) ))
                                 (t (setq abort1 t)) ))
		       (print "Ende von scan-whitespace.")
		       )
      )
     ;; init variables:
     (get-ch)
     (next-token)
     #'(lambda (msg)
	 (scanner msg) )
;     (function scanner)  ; value of gen-scanner-from-fct
     )))


(defun builtin-sym-p (tok)
  ;; for future extensions
  (declare (ignore tok))
  nil
  )


; Following two meta-rules to obtain a clear structure:

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
      ((not-eq first-of-syntax-rule
            (funcall scanner 'last-token-type) )
       l) ))


(defun parse-general-enumeration (scanner syntax-rule &optional auto-inst-p)
  ; Parses a construct of the form:
  ;   syntax-rule {"," syntax-rule}
  ; where syntax-rule is a parser-function.
  ; Returns a list of the results.
  (do ((l (list (funcall syntax-rule scanner auto-inst-p))
          (append l (list (funcall syntax-rule scanner auto-inst-p))) ))
      ((not-eq (token-type (funcall scanner 'last-token))
            'comma)
       l )
      (accept-token-type 'comma) ))


(defun parse-clause (scanner)
  ; clause ::= head  (   "."
  ;                   |  ":-" [body] "."
  ;                  )
  (let* ((head      (parse-head scanner))
         (tok-type  (funcall scanner 'last-token-type)) )
    (funcall scanner)  ; accept the symbol between head and body
    (case tok-type
          (point     (list 'hn head))                            ; horn-fact
          (implies   (if (eq 'point (funcall scanner 'last-token-type)) ;rule
                         ; empty body:
                         (progn (accept-token-type 'point)
                                (list 'hd head) )
                         (prog1 (parse-the-body scanner head)
                                (accept-token-type 'point) )))
          (t      (signal-error scanner
                                "Unknown symbol between head and body: "
                                tok-type )))))


(defun parse-head (scanner)
  ; head ::= term round-list-with-terms
  (cons (parse-term scanner t)   ; t means auto-inst-p
        (parse-round-list-with-terms scanner) ))


(defun parse-the-body (scanner head &aux body)
  ; body ::=   ( expr+ ["&" expr] )
  ;          | ( "&" expr )
  ; Returns a horn- or a footed-rule.
  (if (not-eq 'ampersand (funcall scanner 'last-token-type))
      (setq body (parse-expr+ scanner)) )
  (if (eq 'ampersand (funcall scanner 'last-token-type))
      (progn (accept-token-type 'ampersand)
             `(ft ,head ,@body ,(parse-expr scanner)) )
      `(hn ,head ,@body) ))


(defun parse-expr (scanner &optional auto-inst-p)
  ; expr ::=  (  term [   {round-list}
  ;                    |  IS expr
  ;                   ]
  ;            | builtin
  ;           )
  ; auto-inst-p is not used in this function.
  (declare (ignore auto-inst-p))
  (if (builtin-sym-p (funcall scanner 'last-token))
      (error "Noch zu ergaenzen")
      (let ((r (parse-term scanner)))
        (case (funcall scanner 'last-token-type)
              (is (accept-token-type 'is)
                  (if (and (consp r)
                           (eq 'inst (car r)) )
                      ;; is does not need an expl. inst-tag
                      (list 'is (second r) (parse-expr scanner))
                    (list 'is r (parse-expr scanner)) ))
              (round-left (construct-application
                           r
                           (parse-general-loop scanner 
                                               #'parse-round-list
                                               'round-left )))
              (t    r) ))))


(defun parse-term (scanner &optional auto-inst-p)
  ; term ::=  (  CONSTANT 
  ;            | VARIABLE
  ;            | sqr-list
  ;           )
  ;           {sqr-list}
  ; If auto-inst-p is t then no explicit inst-tag is generated.
  ; Needed for terms in the head of a rule and for nested lists.
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
                 l
                 (parse-general-loop scanner 
                                     #'parse-sqr-list
                                     'sqr-left
                                     t ) )
              sqr-left-p t ))
    (if (and (not auto-inst-p)
             sqr-left-p )
        (setq l (list 'inst l)) )
    l ))


#|
(defun parse-round-list (scanner &optional auto-inst-p &aux l)
  ; round-list ::= "(" [expr+] ["|" VARIABLE] ")"
  ; auto-inst-p is not used in this function.
  (accept-token-type 'round-left)
  (case (funcall scanner 'last-token-type)
        (bar )
        (round-right )
        (t (setq l (parse-expr+ scanner))) )
  (if (eq 'bar (funcall scanner 'last-token-type))
      (progn (accept-token-type 'bar)
             (setq l (append l
                             (list '\| (parse-variable scanner)) ))))
  (accept-token-type 'round-right)
  l )
|#


(defun parse-round-list (scanner &optional auto-inst-p &aux l)
  ; round-list ::= "(" [expr+] ["|" expr] ")"
  (accept-token-type 'round-left)
  (case (funcall scanner 'last-token-type)
        (bar )
        (round-right )
        (t (setq l (parse-expr+ scanner))) )
  (if (eq 'bar (funcall scanner 'last-token-type))
      (progn (accept-token-type 'bar)
             (setq l (append l
                             (list '\| (parse-expr scanner auto-inst-p)) ))))
  (accept-token-type 'round-right)
  l )


(defun parse-sqr-list (scanner &optional auto-inst-p &aux l)
  ; sqr-list ::= "[" [term+] ["|" term] "]"
  (accept-token-type 'sqr-left)
  (case (funcall scanner 'last-token-type)
        (bar )
        (sqr-right )
        (t (setq l (parse-term+ scanner auto-inst-p))) )
  (if (eq 'bar (funcall scanner 'last-token-type))
      (progn (accept-token-type 'bar)
             (setq l (append l
                             (list '\| (parse-term scanner auto-inst-p)) ))))
  (accept-token-type 'sqr-right)
  l )


(defun parse-round-list-with-terms (scanner &aux l)
  ; round-list-with-terms ::= "(" [term+] ["|" VARIABLE] ")"
  (accept-token-type 'round-left)
  (case (funcall scanner 'last-token-type)
        (bar )
        (round-right )
        (t (setq l (parse-term+ scanner t))) )
  (if (eq 'bar (funcall scanner 'last-token-type))
      (progn (accept-token-type 'bar)
             (setq l (append l
                             (list '\| (parse-variable scanner)) ))))
  (accept-token-type 'round-right)
  l )


(defun parse-expr+ (scanner &optional auto-inst-p)
  ; expr+ ::= expr {"," expr}
  ; Returns a list of results.
  ; auto-inst-p is not used in this function.
  (declare (ignore auto-inst-p))
  (parse-general-enumeration scanner #'parse-expr) )


(defun parse-term+ (scanner &optional auto-inst-p)
  ;; term+ ::= term {"," term}
  ; Returns a list of results.
  (parse-general-enumeration scanner #'parse-term auto-inst-p) )


(defun parse-variable (scanner)
  (let ((var-name (funcall scanner 'last-token-value)))
    (accept-token-type 'variable)
    (if (eq '_ var-name)
        'id                      ; anonymous variable
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



; --------------------  Exported functions:  ---------------------------------


(defun pro-read-data-base (filename)
  (catch :pro-read-error-tag
    (with-open-file
     (the-input-stream filename :direction :input)
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
               ; No error
               (setq data-base (cons clause data-base)) )))))))

  
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
              clause )
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
              goal )
             ((eq 'point (funcall scanner 'last-token-type))
              (pro-print-error
               scanner
               (list "Point not allowed at the end of a goal.") )
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
     (let* ((scannerr (gen-scanner the-input-stream))
            (tok     (funcall scannerr 'last-token)) )
       (if (and (eq 'constant 
                    (token-type tok) )
                (symbolp (token-value tok)) )
           (cons (token-value tok)
                 (subseq input-line 
                         (apply scannerr 
                                  'pos-of-first-non-white) ))
         (cons nil "") )))))

           
 
(defun pro-parse-head (head-as-string)
  (with-input-from-string (the-input-stream head-as-string)
                          (catch :pro-read-error-tag
                            (parse-head (gen-scanner the-input-stream)) )))



; ----------------------------------------------------------------------------
; End of module Pro2Lisp.Lsp
; ----------------------------------------------------------------------------






