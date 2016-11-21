;;; Copyright Notice
;;;
;;; This software is distributed for non-profit and research purposes only.
;;; Non-profit redistribution of the current version or parts of the
;;; current version is permitted if this copyright notice is included unchanged.
;;; I give no warranty of any kind for this prototype. It will be further
;;; improved as time permits.  
;;;
;;; Michael Herfert, 1992
 
 
;;; -------------------------------------------------------------------------- 
;;;
;;; Module comment.lsp.
;;;
;;;   Contains functions to read and print comments
;;;   of Relfun-programs.
;;;   If L-syntax is active then comments are recognized by two
;;;   read-macros.
;;;   If P-syntax is active then comments are recognized by the scanner.



(defconstant lsyn-comment-lead-in #\;)
(defconstant psyn-comment-lead-in #\%)
  

(defstruct com-cell
  lisp					; expr to print before the comment
  (pre-txt "")				; comments before lisp (seldom used)
  (post-txt "")				; comments after lisp (the usual case)
  ;; If post-txt begins with a #\newline then it's a begin-of-line-comment,
  ;; otherwise the first substring (up to the first #\newline) is a
  ;; end-of-line-comment.
  ;; If post-txt is not the empty string it is terminated by #\newline.
  ;; post-txt is printed after  lisp.
  internal-p				; lisp contains a coment
  )


(defmacro look-for-comment ()
  ;; Checks if the scanner holds a comment.
  `(make-com-cell
    :post-txt  (if (funcall scanner 'comment-p)
		   (funcall scanner 'comment)
		   ""))
  )


(defun comment-p (sexpr)
  ;; <==> sexpr is a com-cell containing a non-empty comment.
  (or (and (com-cell-p sexpr)
	   (or (string/= "" (com-cell-pre-txt sexpr))
	       (string/= "" (com-cell-post-txt sexpr)) ))
      (and (pro-expr-p sexpr)
	   (or (string/= "" (pro-expr-pre-txt sexpr))
	       (string/= "" (pro-expr-post-txt sexpr)) ))))
   


;;; -------------------------------------------------------------------------
;;;
;;;                   Reading Commments
;;;
;;; -------------------------------------------------------------------------


(defun eat-comment-lead-ins (the-input-stream com-lead-in)
  ;; reads all comment-lead-in characters from the input stream.
  ;; On exit (read-char the-input-stream) returns the first char
  ;; after the last com-lead-in.
  ;; No value.
  (do ((ch (peek-char nil the-input-stream nil eof-char)
	   (peek-char nil the-input-stream nil eof-char) ))
      ((char/= com-lead-in ch))
    (read-char the-input-stream)	; consume the com-lead-in
    ))


(defun read-until-newline (the-input-stream)
  ;; Reads the rest of the line including the newline-char.
  ;; On exit (read-char the-input-stream) returns the first char after
  ;; #\newline.
  (do* ((ch (read-char the-input-stream nil #\newline)
	    (read-char the-input-stream nil #\newline) )
	(txt (string ch)
	     (strcat txt (string ch)) )
	)
       ((char= ch #\newline)
	txt )
       )
  )


(defun read-until-non-white (the-input-stream)
  ;; Read until the first non-white-space char or eof-char in the current line.
  ;; #\newline is not a whitespace.
  ;; On entry the (imag.) cursor is in the first column.
  ;; Returns a list:
  ;;   (<non-white-char> <column-of-non-white-char>)
  ;; On exit the next call of (read-char the-input-stream) returns the
  ;; non-white char.
  (do ((ch (peek-char nil the-input-stream nil eof-char)
	   (peek-char nil the-input-stream nil eof-char)
	   ;; peek-char <peek-type> <stream> <eof-error-p> <eof-value> <rek-p>
	   )
       (column 1 (1+ column))
       )
       ((case ch
	      ((#\space #\tab)
	       nil )			; continue reading
	      (t t))			; non-white found
	(list ch column) 
	)
       (read-char the-input-stream)	; consume the white-space
       ))


(defun read-until-black (the-input-stream)
  ;; Reads until a non-white-space. #\newline is a whitespace.
  ;; On exit (read-char the-input-stream) returns the non-white char.
  ;; Value is a list:     (<non-white-char> <str>)
  ;;           str is a string containing a #\newline for every read #\newline.
  (do ((non-white-char (first (read-until-non-white the-input-stream))
		       (first (read-until-non-white the-input-stream)) )
       (str "" (strcat str (string #\newline)))
       )
      ((char/= #\newline non-white-char)
       (list non-white-char str) )
    (read-char the-input-stream)	; consume the #\newline
    ))

	
(defun read-comments (com-lead-in	
		      type-of-first-com ; dom = {1, 2}
		      the-input-stream )
  ;; Read in comments until a non-comment.
  ;; Returns a list:
  ;;   (<comments> <nr-of-newlines> <column-in-act-line>)
  ;; If type-of-first-com = 2 then <comments> has a leading #\newline.
  ;; On entry (read-char the-input-stream) returns the first char after
  ;; the (first) comment lead in.
  ;; On exit the next call of (read-char the-input-stream) returns
  ;; the first char that is not part of a comment resp. eof.
  (eat-comment-lead-ins the-input-stream com-lead-in)
  (do* ((txt (read-until-newline the-input-stream)
	     (strcat txt (read-until-newline the-input-stream)) )
        (list-2 (read-until-non-white the-input-stream)
		(read-until-non-white the-input-stream) )
	(non-white-char (first list-2) (first list-2))
	(column (second list-2) (second list-2))
	(nr-of-newlines 1 (1+ nr-of-newlines))
	)
       ((and (char/= non-white-char com-lead-in)
	     (char/= non-white-char #\newline) )
	(list (if (= 2 type-of-first-com)
		  (strcat (string #\newline) txt)
		  txt)
	      nr-of-newlines
	      column )
	)
    (when (char= non-white-char com-lead-in)
      ;; consume the com-lead-in:
      (eat-comment-lead-ins the-input-stream com-lead-in))
    ))
    
  
(defun lsyn-comment-type-1-reader (stream comment-lead-in)
  ;; Reader-macro. 
  ;; Called for lines of the format:
  ;;         <lisp-code> ; <comment>
  ;; Reads the comment and all comments immedeatly following this line.
  (declare (ignore comment-lead-in))
  (make-com-cell
   :post-txt (first (read-comments lsyn-comment-lead-in 1 stream)) ))
  

(defun lsyn-comment-type-2-reader (stream the-newline-char)
  ;; Reader-macro.
  ;; Called at the end of the line to check if the next line
  ;; begins with a comment.
  ;; If so it reads the comments and all comments immedeatly following
  ;; this line.
  (declare (ignore the-newline-char))
  (if (char= lsyn-comment-lead-in
	     (first (read-until-non-white stream)) )
      ;; comment detected:
      (progn (read-char stream)		; consume the comment-lead-in
	     (make-com-cell
	      :post-txt (first (read-comments lsyn-comment-lead-in 2 stream))))
    ;; no comment:
    (values)				; return no values
    ))


;; -------------------------------------------------------------------------


(defun hide-comment-at-first (the-list)
  ;; the-list begins with a comment.
  ;; Returns a list.
  (let* ((el-1 (first the-list))
	 (el-2 (second the-list))
	 ;(el-3 (third the-list))
	 (result-of-2 (hide-comments el-2))
	 )
    (if (com-cell-p result-of-2)
	;; el-2 has an internal comment:
	(setf (com-cell-lisp el-1) (com-cell-lisp result-of-2)
	      (com-cell-internal-p el-1) t )
	;; el-2 has no internal comment:
	(setf (com-cell-lisp el-1) result-of-2) )
    (setf (com-cell-pre-txt el-1) (com-cell-post-txt el-1)
	  (com-cell-post-txt el-1) "" )
    (cons el-1 (hide-comments-in-list (cddr the-list))) ))
  

(defun hide-comments-in-list (the-list &aux (the-car (car the-list)))
  ;; the-list can begin with a comment but not with a tag.
  ;; Returns a list.
  ;; Example: (<sexpr-1> <comment-1> <sexpr-2>)
  ;;   ==>    (<comment-1> <sexpr-2>)
  ;; <sexpr-1> is now in the lisp-field of <comment-1>.
  ;(terpri) (princ "hide-comments-in-list: the-list = ") (princ the-list)
  (cond ((null the-list)
	 nil )
	((com-cell-p the-car)
	 ;; the-list begins with a comment:
	 (hide-comment-at-first the-list) )
	((or (eq 'hn the-car)
	     (eq 'ft the-car)
	     (eq '\| the-car) )
	 ;; never comment a tag or a bar:
	 (cons the-car (hide-comments-in-list (rest the-list))) )
	(t
	 ;; no comment, no tag:
	 (let ((el-2 (second the-list)))
	   (if (com-cell-p el-2)
	       ;; comment on second position:
	       (let ((res-of-first (hide-comments (first the-list))))
		 (if (com-cell-p res-of-first)
		     ;; the car of the-list has an internal comment:
		     (setf (com-cell-lisp el-2)
			   (com-cell-lisp res-of-first)
			   (com-cell-internal-p el-2)
			   t )
		     ;; the car of the list has no internal comment:
		     (setf (com-cell-lisp el-2)
			   res-of-first ))
		 (cons (second the-list)
		       (hide-comments-in-list (cddr the-list)) ))
	       ;; no comment on second position:
	       (cons (hide-comments (first the-list))
		     (hide-comments-in-list (cdr the-list)) )
	       )))))
  

(defun hide-comments (expr)
  ;; Merges sexpr-s and comments.
  ;; If expr is a list and one of its element is a comment,
  ;; the whole expr will be packed in a com-cell.
  (if (atom expr)
      expr
      ;; expr is a list:
      (let ((result (hide-comments-in-list expr)))
	(if (any-true-p result #'com-cell-p)
	    ;; at least one element has a comment:
	    (make-com-cell :internal-p t :lisp result)
	    ;; no element contains a comment:
	    result ))))


;;; -------------------------------------------------------------------------
;;;
;;;                   Printing Commments
;;;
;;; -------------------------------------------------------------------------



(defun gen-n-spaces (n)
  (do ((i 0 (1+ i))
       (spaces "" (strcat spaces " "))
       )
      ((= i n)
       spaces
       )))


(defun find-newline (str start-pos)
  ;; Returns the position of the first #\newline.
  ;; Starts search at start-pos.
  ;; There must be a newline.
  (do ((i start-pos (1+ i))
       )
      ((char= #\newline (char str i))
       i
       )))

      
(defun get-next-word (str start-pos)
  ;; Returns a list of two integers:
  ;;   (<start-of-word> <end-of-word>)
  ;; If there is no word after start-pos or a #\newline is detected
  ;; then nil is returned.
  ;; str must contain at least one #\newline.
  ;; Words are delimited by blanks.
  ;; start-pos is numbered from zero.
  (if (>= start-pos (length str))
      nil
    (let* ((start-of-word (do ((i start-pos (1+ i))) ; find non-white-char
			      ((case (char str i)
				     ((#\space #\tab)
				      nil ) ; continue loop
				     (#\newline
				      (return -1) ) ; abort loop, no word
				     (t
				      (return i) )))
			      )) ; abort loop, word found
	   )
      (if (= start-of-word -1)
	  nil				; no word
	(do ((end-of-word start-of-word (1+ end-of-word)))
	    ((case (char str (1+ end-of-word))
		   ((#\space #\tab #\newline)
		    ;; abort loop:
		    t )
		   (t
		    ;; continue loop:
		    nil ))
	     (list start-of-word end-of-word)
	     ))))))


(defun print-one-line-of-comment (str
				  leading-spaces
				  beg-end-of-first-word
				  cursor-col
				  indent-col
				  com-lead-in )
  ;; Prints str (str must be terminated by #\newline), possibly on multiple
  ;; lines.
  ;; Every new line begins with a number of leading spaces, determined
  ;; by leading-spaces.
  ;; Prints at least the first word of str.
  ;; There must be at least one word in str.
  ;; On exit the cursor is after the last printed character.
  ;; No value.
  (do* ((beg-end-of-word beg-end-of-first-word)
	(beg-of-word (first beg-end-of-first-word)
		     (first beg-end-of-word))
	(spaces-before-word leading-spaces
			    (subseq str (1+ end-of-word) beg-of-word))
	(end-of-word (second beg-end-of-first-word)
		     (second beg-end-of-word))
	(the-word (strcat spaces-before-word
			  (subseq str beg-of-word (1+ end-of-word)) )
		  (strcat spaces-before-word
			  (subseq str beg-of-word (1+ end-of-word)) ))
	(new-cursor-col (+ cursor-col (length the-word))
			(+ new-cursor-col (length the-word)) )
	)
       ((and (> new-cursor-col *rf-print-width*) ; last column reserved
	     ; first word extra:
	     (not (eq beg-end-of-word beg-end-of-first-word)) ) 
	;; no space on this line but words to print left:
	(pro-newline-and-indent indent-col)
	(rf-princ-like-lisp com-lead-in)
	;; print the rest words:
	(print-one-line-of-comment str
				   leading-spaces
				   beg-end-of-word
				   (+ 1 indent-col) ; 2 = len(";")
				   indent-col
				   com-lead-in )
	)
       (rf-princ-like-lisp the-word)
       (setq beg-end-of-word (get-next-word str (1+ end-of-word)))
       (when (null beg-end-of-word)
	     ;; no more words:
	     (return)
	     )))


(defun print-one-line-of-comment-wo-break (str str-start-pos)
  ;; Prints str from str-start-pos up to the first #\newline (must exist)
  ;; even if it exceeds *rfi-print-width*
  (rf-princ-like-lisp (subseq str
			      str-start-pos
			      (find-newline str str-start-pos))))


(defun print-comment-type-1 (str-wo-newline lead-in-char cursor-col indent-col)
  ;; Try to print the comment at the current line.
  ;; If this is not possible print as type 2.
  ;; On exit the cursor is on the right of the last printed character.
  ;; No value.
  (let* ((len-of-com (length str-wo-newline))
	 (space-on-line (- *rf-print-width* cursor-col))
	 (real-space-after-indent-col (- *rf-print-width*
					 (max (first *comment-style*)
					      cursor-col )))
	 )
    (if (<= space-on-line (+ 2 len-of-com)) ; 2 = len("; ")
	;; does not fit on line:
	(progn
	  (pro-newline-and-indent indent-col)
	  (print-comment-type-2 (strcat str-wo-newline (string #\newline))
				0	; str-start-pos
				lead-in-char
				indent-col) )
	;; comment fits on line:
	(case (second *comment-style*)
	  (raw
	   ;; no alignment at all.
	   (rf-princ-like-lisp " ")
	   (rf-princ-like-lisp lead-in-char)
	   (rf-princ-like-lisp str-wo-newline) )
	  (comment-column
	   ;; indent to comment-column, if possible.
	   (if (> real-space-after-indent-col len-of-com) 
	       ;; alignment possible ==> go to indent-column
	       (spaces (- (first *comment-style*) cursor-col))
	       ;; no alignment poss.:
	       (rf-princ-like-lisp " ") )
	   (rf-princ-like-lisp lead-in-char)
	   (rf-princ-like-lisp str-wo-newline) )
	  (comment-column-or-margin
	   ;; indent to indent-column, if possible, else align to right.
	   (if (> real-space-after-indent-col len-of-com)
	       ;; align to indent-column:
	       (spaces (- (first *comment-style*) cursor-col))
	       ;; align to right margin:
	       (spaces (- *rf-print-width* cursor-col len-of-com 1)) )
	   (rf-princ-like-lisp lead-in-char)
	   (rf-princ-like-lisp str-wo-newline) )
	  (margin
	   ;; always align to right:
	   (spaces (- *rf-print-width* cursor-col len-of-com 1)) 
	   (rf-princ-like-lisp lead-in-char)
	   (rf-princ-like-lisp str-wo-newline) )
	  (t
	   (error "Internal error in <print-comment-type-1>") )))))
	

(defun print-comment-type-2 (str str-start-pos com-lead-in indent-col)
  ;; Prints str (from str-start-pos) up to the first #\newline (must exist).
  ;; On entry cursor is in indent-col.
  ;; On exit cursor is on the right of the last printed character.
  (let* ((beg-end-of-first-word (get-next-word str str-start-pos))
	 (beg-of-first-word (first beg-end-of-first-word))
	 ;;(end-of-first-word (second beg-end-of-first-word))
	 )
    (unless (null beg-end-of-first-word)
	    ;; comment not empty:
      (rf-princ-like-lisp com-lead-in)
      (if (eq 'break (third *comment-style*))
	  ;; break comment-lines if longer then *rfi-print-width*:
	  (print-one-line-of-comment str
				     (gen-n-spaces (- beg-of-first-word
						      str-start-pos ))
				     beg-end-of-first-word
				     (+ 1 indent-col) ; 1 = len(";")
				     indent-col
				     com-lead-in )
	  (print-one-line-of-comment-wo-break str str-start-pos) ))))


(defun print-comment (str lead-in-char cursor-col indent-col)
  ;; str contains comments each terminated by a #\newline.
  ;; Prints all comments.
  ;; On exit cursor is on the right of the last printed character.
  (let ((start-of-type-2 1)		; pos. 0: #\newline 
	(len-of-str (length str))
	)
    (unless (char= #\newline (char str 0))
      ;; starts with a end-of-line-comment:
      (print-comment-type-1 (subseq str 0 (find-newline str 0))
			    lead-in-char
			    cursor-col
			    indent-col )
      (setq start-of-type-2 (1+ (find-newline str 0))) )
    ;; all other comments are of type 2:
    (do ((i start-of-type-2 (1+ (find-newline str i)))
	 )
	((= i len-of-str)
	 ;; all comments printed:
	 indent-col
	 )
	;; print one line of comment:
	(pro-newline-and-indent indent-col) 
	(print-comment-type-2 str i lead-in-char indent-col) )))
	      

(defun lsyn-comment-printer (com cursor-col indent-col &optional clause-p)
  ;; type(com) = com-cell
  ;; clause-p <==> called from pp-clause
  ;; On exit cursor is on the right of the last printed character.
  (unless (string= "" (com-cell-pre-txt com))
    ;; print a leading comment (always type 2):
    (print-comment-type-2
     (com-cell-pre-txt com)
     (if (char= #\newline (char (com-cell-pre-txt com) 0)) 1 0)
     lsyn-comment-lead-in
     cursor-col )
    (pro-newline-and-indent indent-col) )
  (if (com-cell-internal-p com)
      ;; Comment inside lisp-part:
      (if clause-p
	  (pp-big-clause (com-cell-lisp com))
	  (pp-broken-list (com-cell-lisp com)
			  round-left
			  round-right ))
      ;; no internal comment:
      (if clause-p
	  (pp-clause (com-cell-lisp com))
	  (pp-expr (com-cell-lisp com)) ))
  (unless (string= "" (com-cell-post-txt com))
    ;; print the comment:
    ;;(princ pp-currentpos*)
    (print-comment (com-cell-post-txt com)
		   lsyn-comment-lead-in
		   (1+ pp-currentpos*)
		   indent-col )
    ;; After a comment a closing parenthese requires a new line:
    (pp-push 'comment-was-here pp-stack*)
    ))



;;; -------------------------------------------------------------------------
;;;
;;;                   Auxilliary functions
;;;
;;; -------------------------------------------------------------------------



(defun any-true-p (the-list &optional (pred #'(lambda (x) x)))
  ;; Returns nil, if all elements evaluate to nil under pred.
  ;; Else it returns the first element not evaluating to nil.
  (do* ((rest-of-list the-list (rest rest-of-list))
	(element (first rest-of-list) (first rest-of-list))
	)
       ((null rest-of-list)
	;; all nil
	nil
	)
       (when (funcall pred element)
	     (return element) )))
       

(defun setup-comment-readtable (a-readtable)
  ;; Sets read-macros for #\; and #\newline.
  ;; a-readtable is not destructively modified.
  ;; Value: The new readtable.
  (let ((new-readtable (copy-readtable a-readtable)))
    (set-macro-character lsyn-comment-lead-in
			 #'lsyn-comment-type-1-reader
			 nil		; non-terminating-p
			 new-readtable )
    (set-macro-character #\newline
			 #'lsyn-comment-type-2-reader
			 nil		; non-terminating-p
			 new-readtable )
    new-readtable
    ))


;;; -------------------------------------------------------------------------
;;;
;;;                      Main functions
;;;
;;; -------------------------------------------------------------------------


(defun echo-empty-lines (i-stream comment-lead-in)
  ;; Returns t iff at least one empty line read.
  (do ((non-white-char (first (read-until-non-white i-stream))
		       (first (read-until-non-white i-stream)) )
       (empty-lines-p nil t)
       )
      ((or (char= eof-char non-white-char)
	   (char/= #\newline non-white-char) )
       (when (and empty-lines-p (char/= comment-lead-in non-white-char))
	 (pro-newline-and-indent 1) )
       empty-lines-p )
    (read-char i-stream)
    (when empty-lines-p (pro-newline-and-indent 1)) ))


(defun rf->rf (lsyn-in lsyn-out comment-style)
  ;; Pretty prints a file written in L-syntax including the comments.
  (let* ((*comment-style* comment-style)
	 (*style* 'lisp)
	 (*readtable* (setup-comment-readtable *rfi-readtable*))
	 (*rfi-standard-output* lsyn-out)
	 )
    (do* ((empty-lines-p (echo-empty-lines lsyn-in lsyn-comment-lead-in)
			 (echo-empty-lines lsyn-in lsyn-comment-lead-in))
	  (cursor-col 1 (if empty-lines-p 1 cursor-col))
	  (non-white-char (peek-char nil lsyn-in nil eof-char)
			  (peek-char nil lsyn-in nil eof-char))
	  (type-of-comment 2 (if empty-lines-p 2 1))
	  )
	 ((char= eof-char non-white-char) t)
      (if (char= lsyn-comment-lead-in non-white-char)
	  ;; comment detected:
	  (progn
	    (read-char lsyn-in)	; consume the com-lead-in
	    (print-comment
	     (first (read-comments lsyn-comment-lead-in
				   type-of-comment
				   lsyn-in ))
	     lsyn-comment-lead-in
	     cursor-col
	     1 )			; 1 = indent-col.
	    (pro-newline-and-indent 1)
	    )
	  ;; clause detected:
	  (progn
	    (setq cursor-col (pp-clause (hide-comments (read lsyn-in))))
	    )))))


(defun rf->rfp (lsyn-stream psyn-stream comment-style)
  ;; Transforms a file written in L-syntax to P-Syntax including the
  ;; comments.
  (let* ((*comment-style* comment-style)
	 (*readtable* (setup-comment-readtable *rfi-readtable*))
	 (*rfi-standard-output* psyn-stream)
	 )
    (do* ((empty-lines-p (echo-empty-lines lsyn-stream lsyn-comment-lead-in)
			 (echo-empty-lines lsyn-stream lsyn-comment-lead-in))
	  (cursor-col 1 (if empty-lines-p 1 cursor-col))
	  (non-white-char (peek-char nil lsyn-stream nil eof-char)
			  (peek-char nil lsyn-stream nil eof-char))
	  (type-of-comment 2 (if empty-lines-p 2 1))
	  )
	 ((char= eof-char non-white-char) t)
      (if (char= lsyn-comment-lead-in non-white-char)
	  ;; comment detected:
	  (progn
	    (read-char lsyn-stream)	; consume the com-lead-in
	    ;; print-comment starts with a #\newline for type-2-comments
	    (print-comment
	     (first (read-comments lsyn-comment-lead-in
				   type-of-comment
				   lsyn-stream ))
	     psyn-comment-lead-in
	     cursor-col
	     1 )			; 1 = indent-col.
	    (pro-newline-and-indent 1)
	    )
	  ;; clause detected:
	  (progn
	    (setq cursor-col (pro-print (hide-comments (read lsyn-stream))))
	    )))))


(defun rfp->rfp (psyn-in psyn-out comment-style)
  ;; Pretty print a file written in P-syntax including the comments.
  (let* ((*comment-style* comment-style)
	 (*rfi-standard-output* psyn-out)
	 scanner 
	 )
    (catch :pro-read-error-tag
      (do* ((empty-lines-p (echo-empty-lines psyn-in psyn-comment-lead-in)
			   (echo-empty-lines psyn-in psyn-comment-lead-in))
	    (cursor-col 1 (if empty-lines-p 1 cursor-col))
	    (non-white-char (peek-char nil psyn-in nil eof-char)
			    (peek-char nil psyn-in nil eof-char))
	    (type-of-comment 2 (if empty-lines-p 2 1))
	    )
	   ((char= eof-char non-white-char) t)
	(if (char= psyn-comment-lead-in non-white-char)
	    ;; comment detected:
	    (progn
	      (read-char psyn-in)	; consume the com-lead-in
	      ;; print-comment starts with a #\newline for type-2-comments
	      (print-comment
	       (first (read-comments psyn-comment-lead-in
				     type-of-comment
				     psyn-in ))
	       psyn-comment-lead-in
	       cursor-col
	       1 )			; 1 = indent-col.
	      (pro-newline-and-indent 1)
	      )
	    ;; clause detected:
	    (progn
	      (if scanner
		  nil
		  (setq scanner (gen-scanner psyn-in)) )
	      (setq cursor-col
		    (pro-print (hide-comments (parse-clause scanner))))
					;(princ cursor-col)
	      (if (funcall scanner 'comment-p)
		  (progn
		    (print-comment (funcall scanner 'comment)
				   psyn-comment-lead-in
				   cursor-col
				   1 )
		    (pro-newline-and-indent 1))
		  (progn 
		    (dotimes (i (length (funcall scanner 'str-of-newlines)))
		      (pro-newline-and-indent 1) )
		    ))))))))


(defun rfp->rf (psyn-stream lsyn-stream comment-style)
  ;; Transforms a file written in P-syntax to L-Syntax including the
  ;; comments.
  (let* ((*comment-style* comment-style)
	 (*style* 'lisp)
	 (*rfi-standard-output* lsyn-stream)
	 scanner 
	 )
    (catch :pro-read-error-tag
      (do* ((empty-lines-p (echo-empty-lines psyn-stream psyn-comment-lead-in)
			   (echo-empty-lines psyn-stream psyn-comment-lead-in))
	    (cursor-col 1 (if empty-lines-p 1 cursor-col))
	    (non-white-char (peek-char nil psyn-stream nil eof-char)
			    (peek-char nil psyn-stream nil eof-char))
	    (type-of-comment 2 (if empty-lines-p 2 1))
	    )
	   ((char= eof-char non-white-char) t)
	(if (char= psyn-comment-lead-in non-white-char)
	    ;; comment detected:
	    (progn
	      (read-char psyn-stream)	; consume the com-lead-in
	      (print-comment
	       (first (read-comments psyn-comment-lead-in
				     type-of-comment
				     psyn-stream ))
	       lsyn-comment-lead-in
	       cursor-col
	       1 )			; 1 = indent-col.
	      (pro-newline-and-indent 1)
	      )
	    ;; clause detected:
	    (progn
	      (if scanner
		  nil
		  (setq scanner (gen-scanner psyn-stream)) )
	      (setq cursor-col
		    (pp-clause (hide-comments (parse-clause scanner))))
					;(princ cursor-col)
	      (if (funcall scanner 'comment-p)
		  (progn
		    (print-comment (funcall scanner 'comment)
				   lsyn-comment-lead-in
				   cursor-col
				   1 )
		    (pro-newline-and-indent 1))
		  (progn 
		    (dotimes (i (length (funcall scanner 'str-of-newlines)))
		      (pro-newline-and-indent 1) )
		    ))))))))
		 

(defun bal->bap (lsyn-stream psyn-stream comment-style)
  ;; Transforms a batch-file written in L-syntax to P-syntax.
  (let* ((*comment-style* comment-style)
	 (*readtable* (setup-comment-readtable *rfi-readtable*))
	 (*rfi-standard-output* psyn-stream)
	 goal-or-cmd
	 first-symbol
	 )
    (do* ((empty-lines-p (echo-empty-lines lsyn-stream lsyn-comment-lead-in)
			 (echo-empty-lines lsyn-stream lsyn-comment-lead-in))
	  (cursor-col 1 (if empty-lines-p 1 cursor-col))
	  (non-white-char (peek-char nil lsyn-stream nil eof-char)
			  (peek-char nil lsyn-stream nil eof-char))
	  (type-of-comment 2 (if empty-lines-p 2 1))
	  )
	 ((char= eof-char non-white-char) t)
      (if (char= lsyn-comment-lead-in non-white-char)
	  ;; comment detected:
	  (progn
	    (read-char lsyn-stream)	; consume the com-lead-in
	    ;; print-comment starts with a #\newline for type-2-comments
	    (print-comment
	     (first (read-comments lsyn-comment-lead-in
				   type-of-comment
				   lsyn-stream ))
	     psyn-comment-lead-in
	     cursor-col
	     1 )			; 1 = indent-col.
	    (pro-newline-and-indent 1)
	    )
	  ;; command or goal detected:
	  (progn
	    (setq goal-or-cmd (read-goal-or-cmd-from-lsyn-batch lsyn-stream)
		  first-symbol (first goal-or-cmd)
		  cursor-col
		  (if (member first-symbol *rfi-commands*)
		      (let ((first-symbol-as-string
			     (strcat (princ-to-string first-symbol)
				     " " )))
			(case first-symbol
			  ((l listing)
			   (cond ((null (cdr goal-or-cmd))
				  ;; listing without argument:
				  (pro-print-string first-symbol-as-string 1) )
				 ((null (cddr goal-or-cmd))
				  ;; listing with symbol as argument:
				  (pro-print
				   (second goal-or-cmd)
				   (pro-print-string first-symbol-as-string 1)
				   ))
				 (t
				  ;; listing with head-pattern:
				  (pro-print-head
				   (second goal-or-cmd)
				   (pro-print-string first-symbol-as-string 1)
				   ))))
			  ((a0 az rx)
			   (pro-print
			    (second goal-or-cmd)
			    (pro-print-string first-symbol-as-string 1)) )
			  (azhn 
			   (pro-print
			    (cons 'hn (cdr goal-or-cmd))
			    (pro-print-string "az " 1)) )
			  (a0hn 
			   (pro-print
			    (cons 'hn (cdr goal-or-cmd))
			    (pro-print-string "a0 " 1)) )
			  (azft 
			   (pro-print
			    (cons 'ft (cdr goal-or-cmd))
			    (pro-print-string "az " 1)) )
			  (a0ft 
			   (pro-print
			    (cons 'ft (cdr goal-or-cmd))
			    (pro-print-string "a0 " 1)) )
			  (t
			   ;; no special handling of the rfi-cmd:
			   (dolist (element goal-or-cmd)
			     (rf-princ-like-lisp element)
			     (rf-princ-like-lisp " ") )
			   50 )))
		      ;; it's a goal:
		      (prog1
			  (pro-print-arg-list
			   (construct-measure-tree-of-arg-list goal-or-cmd
							       nil)
			   1		; cursor col.
			   0
			   the-non-printing-char
			   the-non-printing-char )))))))))


(defun bap->bal (psyn-stream lsyn-stream comment-style)
  ;; Transforms a batch-file written in P-syntax to L-syntax.
  (let* ((*comment-style* comment-style)
	 (*rfi-standard-output* lsyn-stream)
	 (*style* 'lisp)		; style when printing
	 )
    (do* ((empty-lines-p (echo-empty-lines psyn-stream psyn-comment-lead-in)
			 (echo-empty-lines psyn-stream psyn-comment-lead-in))
	  (cursor-col 1 (if empty-lines-p 1 cursor-col))
	  (non-white-char (peek-char nil psyn-stream nil eof-char)
			  (peek-char nil psyn-stream nil eof-char))
	  (type-of-comment 2 (if empty-lines-p 2 1))
	  )
	 ((char= eof-char non-white-char) t)
      (if (char= psyn-comment-lead-in non-white-char)
	  ;; comment detected:
	  (progn
	    (print-comment
	     (first (read-comments psyn-comment-lead-in
				   type-of-comment
				   psyn-stream ))
	     lsyn-comment-lead-in
	     cursor-col
	     1 )			; 1 = indent-col.
	    (pro-newline-and-indent 1)
	    )
	  ;; command or goal detected:
	  (setq cursor-col
		(let* ((goal-or-cmd
			(read-goal-or-cmd-from-psyn-batch psyn-stream))
		       (pair (pro-split-input goal-or-cmd))
		       (first-symbol (car pair))
		       (rest-of-line-as-string (cdr pair))
		       )
		  (if (member first-symbol *rfi-commands*)
		      ;; it's a rfi-command:
		      (let* ((first-symbol-as-string
			      (strcat (princ-to-string first-symbol) " " ))
			     (start-column (length first-symbol-as-string))
			     (user-line-as-list
			      (handle-rfi-cmd first-symbol
					      rest-of-line-as-string
					      goal-or-cmd )))
			(rf-princ-like-lisp first-symbol-as-string)
			(case first-symbol
			  ((a0 az rx)
			   ;; print as clause:
			   (pp-clause (second user-line-as-list)
				      start-column ) )
			  ;; l and listing require no special printing:
			  (t
			   ;; rfi-cmd requires no special printing:
			   (print-lsyn-enumeration (rest user-line-as-list)
						   start-column ))))
		      ;; it's a goal (at least 1 element):
		      (print-lsyn-enumeration (pro-read-goal goal-or-cmd) 0)
		      )))))))


(defun read-goal-or-cmd-from-lsyn-batch (the-input-stream)
  (do* ((goal-or-cmd (list (hide-comments (read the-input-stream)))
		     (cons (hide-comments (read the-input-stream))
			   goal-or-cmd ))
	(non-white-char (car (read-until-non-white the-input-stream))
			(car (read-until-non-white the-input-stream)) )
	)
       ((case non-white-char
	  ((#\newline #\;) t)           ; abort loop
	  (t nil) )			; continue
	(reverse goal-or-cmd) )
    ))


(defun read-goal-or-cmd-from-psyn-batch (the-input-stream)
  ;; Returns the complete input (may be multiple lines) as a string.
  (do* ((goal-or-cmd "" (strcat goal-or-cmd (string ch)))
	(ch (peek-char nil the-input-stream) (peek-char nil the-input-stream))
	(*style* 'prolog)
	)
       (nil)
    (case ch
      (#\newline
       ;; end of input line but may be continued
       (when (complete-cmd-p goal-or-cmd)
	 (return goal-or-cmd) ))
      (#\%
       ;; comment at end of line:
       (if (complete-cmd-p goal-or-cmd)
	   (return goal-or-cmd)
	   (let* ((str-stream (make-string-output-stream))
		  (echo-stream (make-echo-stream the-input-stream str-stream))
		  )
	     ;; value does not care,
	     ;; but input chars are accumulated in str-stream:
	     (read-comments psyn-comment-lead-in
			    1		; type of comment
			    echo-stream )
	     (setq goal-or-cmd (strcat
				goal-or-cmd
				(get-output-stream-string str-stream) ))
	     (close str-stream) )))
      (t))
    (setq ch (read-char the-input-stream)) ))


(defun print-lsyn-enumeration (enum start-column)
  ;; Example: enum = (a b c)   => a b c
  ;; Returns the cursor-col.
  ;; left-margin: start-column = 0.
  (pp-init start-column)
  (if (com-cell-p enum)
      ;; internal comment (never end-of-line comment):
      (pp-broken-list (com-cell-lisp enum) '|| '||)
      (pp-list enum '|| '||) )
  (1+ pp-currentpos*)
  )


(defun translate-file (in-filename
		       out-filename
		       &key direction
		       (comment-style *default-comment-style*))
  ;; Translates a file from one syntax to the other including the comments.
  ;; Filenames can be symbols or strings, if not supplied standard extensions
  ;; are added.
  ;; Empty or NULL out-filename means send to *standard-output*.
  ;; Possible values for direction (always required) are:
  ;;             rf->rf, rf->rfp, rfp->rf, rfp->rfp
  ;; Example:  (translate-file "facfix.rf" facfix :direction 'rf->rfp)
  ;;           This is equivalent to:
  ;;           (translate-file "facfix.rf" "facfix.rfp" :direction 'rf->rfp)
  (let (error-p translator
		in-extension out-extension
		full-in-filename
		(full-out-filename (if (and (stringp out-filename)
					    (string= "" out-filename) )
				       nil
				       out-filename ))
		(comment-lead-in (if (eq 'prolog *style*)
				     psyn-comment-lead-in
				     lsyn-comment-lead-in ))
		)
    (case direction
      (rf->rfp (setq in-extension ".rf"
		     out-extension ".rfp"
		     translator #'rf->rfp ) )
      (rfp->rf (setq in-extension ".rfp"
		     out-extension ".rf"
		     translator #'rfp->rf ))
      (rf->rf (setq in-extension ".rf"
		    out-extension ".rf"
		    translator #'rf->rf ))
      (rfp->rfp (setq in-extension ".rfp"
		      out-extension ".rfp"
		      translator #'rfp->rfp ))
      (bal->bap (setq in-extension ".bat"
			out-extension ".bat"
			translator #'bal->bap ))
      (bap->bal (setq in-extension ".bat"
			out-extension ".bat"
			translator #'bap->bal ))
      (t (setq error-p t
	       in-extension ""
	       out-extension "") ))
    (setq full-in-filename (rfi-extension in-filename in-extension))
    (when full-out-filename
      (setq full-out-filename (rfi-extension out-filename out-extension)) )
    (cond (error-p
	   (rf-princ-like-lisp "Error: Illegal direction.") )
	  ((equal full-in-filename full-out-filename)
	   (rf-princ-like-lisp "Error: Source = Destination") )
	  ((not (probe-file full-in-filename))
	   ;; the in-file must exist:
	   (rf-princ-like-lisp "Error: Source does not exist.")
	   (setq error-p t) )
	  ((and full-out-filename (probe-file full-out-filename))
	   ;; outfile exists:
	   (if (rfi-yes-or-no-p "Destination already exists - overwrite? ")
	       (delete-file full-out-filename)
	       (setq error-p t) ))
	  (t) )
    (unless (and (numberp (first comment-style))
		 (member (second comment-style)
			 '(raw comment-column comment-column-or-margin margin))
		 (member (third comment-style)
			 '(break no-break) ))
      (setq error-p t)
      (rf-princ-like-lisp "Error: Illegal comment-style.")
      (rf-terpri)
      (rf-princ-like-lisp "       Domain of the list comment-style is ")
      (rf-terpri)
      (rf-princ-like-lisp
       "       ( <integer>,")
      (rf-terpri)
      (rf-princ-like-lisp
       "         {COMMENT-COLUMN, COMMENT-COLUMN-OR-MARGIN, MARGIN},")
      (rf-terpri)
      (rf-princ-like-lisp
       "         {BREAK, NO-BREAK} )")

      )
    (unless error-p
      (with-open-file (in-stream full-in-filename :direction :input)
	(rf-princ-like-lisp
	 (format nil "~c Converting ~S " comment-lead-in full-in-filename) )
	(if full-out-filename
	    ;; send result to file:
	    (with-open-file (out-stream full-out-filename :direction :output)
	      (rf-princ-like-lisp
	       (format nil "to ~S .." full-out-filename) )
	      (funcall translator in-stream out-stream comment-style) )
	    ;; send result to terminal:
	    (progn
	      (rf-princ-like-lisp "..")
	      (funcall
	       translator in-stream *rfi-standard-output* comment-style ))
	    )))))


;;; -------------------------------------------------------------------------
;;;
;;;                  End of module comment.lsp
;;;
;;; -------------------------------------------------------------------------

  

  
  
  

  



