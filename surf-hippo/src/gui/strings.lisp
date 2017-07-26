;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GUI Source file: strings_and_lists.lisp


(IN-PACKAGE "WINDOWS-HACK")

;; Various useful functions for handling strings, many of which probably can be
;; done better if I read CLTL more thoroughly. String related functions may also be found in
;; sequences.lisp. 

(defun number-of-newlines (string)
  (loop for char across string when (eq #\newline char) sum 1))

(defun string-has-special (string)
  (loop for char across string
	when (member char '(#\" #\# #\' #\;
			    #\\ ; this doesn't do anything
			    #\` #\|)) do (return t))
  )


;; *********************************
;; Printing
;; *********************************

(defun print-spaces (stream indent)
  "Print INDENT spaces to STREAM. If INDENT is not a number, but T, then print 1 space."
  (when indent (loop for x from 1 to (if (numberp indent) indent 1) do (format stream " "))))
    

;; *********************************
;; Strings
;; *********************************


;; This is useful if the string may include "|", which precludes checking if read-from-string
;; returns a number.
(defun string-has-non-number (string)
  (loop for char across string
	unless (find char "1234567890")
	do (return t)))



(defun string-member (string list)
  (member string list :test 'string=))

(defun string-member (string list)
  (member string list :test #'equal))


#|
(defun concatenate-string-list (list-of-strings &key (string-spacer "") string-count-to-add-linefeed)
  (let ((result ""))
    (loop for string in (copy-list (flatten-list list-of-strings))
	  for count from 1
	  do
	  (setq result
		(concatenate 'string result
			     (if string-count-to-add-linefeed
				 (if
				  (= (mod count string-count-to-add-linefeed) 0)
				  (format nil "~a~%" string-spacer)
				  (if (= 0 (length result)) "" string-spacer))
				 (if (= 0 (length result)) "" string-spacer))
			     string)))
    result))
|#


(defun all-caps-p (string)
  (string= (string-upcase string) string))

(defun add-linefeeds-to-string-list (list-of-strings)
  (concatenate-string-list list-of-strings :lf-count 1))

(defun sequence-to-string-list (sequence)
  (mapcar 'princ-to-string (sequence-to-list sequence)))
  
(defun concatenate-strings (&rest strings)
  (concatenate-string-list strings))

(defun concatenate-string-list (list-of-strings &key (string-spacer "") string-count-to-add-linefeed lf-count)
  (when lf-count (setq string-count-to-add-linefeed lf-count))
  (let ((strings (loop for thing in	; (copy-list (flatten-list list-of-strings))
		       (flatten-list list-of-strings)
		       when (stringp thing) collect thing)))
    (apply 'concatenate
	   (cons 'string 
		 (loop for string in strings
		       for count from 1
		       collect string into result
		       when (< count (length list-of-strings)) collect string-spacer into result
		       when (and string-count-to-add-linefeed
				 (= (mod count string-count-to-add-linefeed) 0)
				 (< count (length strings)))
		       collect (format nil "~%") into result
		       finally (return result))))))

(defun concatenate-atoms-to-string-list (list-of-atoms &key (string-spacer "") string-count-to-add-linefeed
						       lf-count)
  (concatenate-string-list
   (loop for atom in list-of-atoms collect (format nil "~A" atom))
   :string-spacer string-spacer
   :string-count-to-add-linefeed string-count-to-add-linefeed
   :lf-count lf-count))

(defun seq-to-string (seq)
  (concatenate-string-list
   (typecase seq
     (array (loop for i from 0 to (1- (length seq)) collect (format nil "~,2e" (aref seq i))))
     (list (loop for i in seq collect (format nil "~,2e" i))))
   :string-spacer " "))


(defun char-seq-to-string (char-list)
  (let ((out ""))
    (loop for char in char-list
	  do (setq out (concatenate 'string out (string char))))
    out))

#|
(defun convert-string-to-vertical (string)
  (let (out)
    (loop for index from 0 to (1- (length string))
	  do (let ((char (schar string index)))
	       (unless (eq char #\NEWLINE) (setq out (concatenate 'string out (string char) (string #\NEWLINE))))))
    out))
|#


(defun convert-string-to-vertical (string)
  (loop for char in (coerce string 'list)
	unless (eq char #\NEWLINE)
	collect char into out and
	collect #\NEWLINE into out
	finally (return (coerce out 'string))))

#|
(defun all-numbers-in-string (string)
  (loop for index from 0 to (1- (length string))
	unless (let ((char (schar string index)))
		 (loop for testchar in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		       when (eq char testchar) do (return t)))
	do (return nil)
	finally (return (read-from-string string))))
|#

(defun all-numbers-in-string (string)
  (loop for char in (coerce string 'list)
	unless (member char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	do (return nil)
	finally (return (read-from-string string))))



;; Replaces any run of CHAR in STRING by a single instance of CHAR, and returns the reduced string.
(defun reduce-repeats (string char)
  (let (last-char)
    (loop for index from 0 to (1- (length string))
	  unless (and (eq (schar string index) char) (eq (schar string index) last-char))
	  collect (schar string index) into out
	  do (setq last-char (schar string index))
	  finally (return (coerce out 'string)))))

#|

(defun replace-tabs-in-string (string)
  "Replace every tab in STRING with a sequence of 8 spaces."
  (if (not (find #\tab string))
      string
      (let ((out ""))
	(loop for index from 0 to (1- (length string))
	      do
	      (let ((char (schar string index)))
		(setq out (concatenate 'string out (if (eq char #\tab)
						       "        " 
						       (string char))))))
	out)))
|#

(defun replace-tabs-in-string (string)
  (if (not (find #\tab string))
      string
      (char-seq-to-string
       (loop for char in (coerce string 'list)    
	     collect (case char
		       (#\tab "       " )
		       (t (string char)))))))

#|
(defun replace-char-w-space-in-string (string char)
  (let ((char (typecase char
		(string (schar char 0))
		(t char))))
    (if (not (find char string))
	string
	(let ((out ""))
	  (loop for index from 0 to (1- (length string))
		do
		(let ((chartemp (schar string index)))
		  (setq out (concatenate 'string out (if (eq chartemp char)
							 " "
							 (string chartemp))))))
	  out))))
|#
(defun replace-char-w-space-in-string (string char)
  (let ((char (typecase char
		(string (schar char 0))
		(t char))))
    (if (not (find char string))
	string
	(char-seq-to-string
	 (loop for dummy-char in (coerce string 'list)    
	       collect (if (eq char dummy-char) " " dummy-char))))))

(defun remove-spaces (string)
  (replace-char-w-another-in-string string " "))

(defun replace-chars-w-another-in-string (string chars &optional another)
  (loop for char in chars do
	(setq string (replace-char-w-another-in-string string char another))
	finally (return string)))

(defun replace-char-w-another-in-string (string char &optional another)
  (let ((char (typecase char
		(string (schar char 0))
		(t char)))
	(another (typecase another
		   (string (if (> (length another) 0) (schar another 0) ""))
		   (nil "")
		   (t another))))
    (if (not (find char string))
	string
	(let ((out ""))
	  (loop for index from 0 to (1- (length string))
		do
		(let ((chartemp (schar string index)))
		  (setq out (concatenate 'string out (if (eq chartemp char)
							 (if another (string another) "")
							 (string chartemp))))))
	  out))))

(defun replace-chars-w-space-in-string (string chars)
  (loop for char in chars
	do (setq string (replace-char-w-space-in-string string char)))
  string)


(defun replace-space-with-underscore (string)
  (if (not (find  #\space string))
      string
      (let ((out ""))
	(loop for index from 0 to (1- (length string))
	      do
	      (let ((char (schar string index)))
		(setq out (concatenate 'string out (if (eq char #\space)
						       "_" 
						       (string char))))))
	out)))

(defun listify-words-in-string (string &optional (space-char '(#\Newline #\space)))
  (let ((out '())
	(word "")
	(in-white-space nil)
	(1-length (1- (length string)))
	(space-chars (coerce-to-list space-char)))
    (loop for index from 0 to 1-length
	  do
	  (let ((char (schar string index)))
	    (if (member char space-chars)
		(unless in-white-space
		  (push word out)
		  (setq word ""
			in-white-space t))
		(progn (setq word (concatenate 'string word (string char))
			     in-white-space nil)
		       (when (= index 1-length)
			 (push word out))
		       ))))
    (reverse out)))

	
(defun replace-underscore-with-space (string)
  (if (not (find #\_ string))
      string
      (let ((out ""))
	(loop for index from 0 to (1- (length string))
	      do
	      (let ((char (schar string index)))
		(setq out (concatenate 'string out (if (eq char #\_)
						       " " 
						       (string char))))))
	out)))


(defun find-tail-- (string)
    (loop for i downfrom (1- (length string)) to 0 
	  when (eq (schar string i) #\-)
	  do (return i)))

(defun find-first-. (string)
    (loop for i from 0 to (1- (length string))
	  when (eq (schar string i) #\.)
	  do (return i)))

(defun string-remove-head (string how-many)
  (let ((out ""))
    (loop for index from how-many to (1- (length string))
	  do (setq out (concatenate 'string out (string (schar string index)))))
    out))

(defun string-remove-tail (string how-many)
  (let ((out ""))
    (loop for index from 0 to (- (1- (length string)) how-many)
	  do (setq out (concatenate 'string out (string (schar string index)))))
    out))

(defun string-tail (string &optional how-many)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (string string))
  (if (numberp how-many)
      (if (< how-many 1) ""
	  (let ((string-length (the fixnum (length (the simple-base-string string))))
		(out ""))
	    (do ((index (the fixnum (- string-length (the fixnum (min string-length (the fixnum how-many)))))
			(+ (the fixnum index) 1)))
		((= index string-length) out)
	      (setq out (concatenate 'string out (the simple-base-string (string (schar string index))))))))
      string))
    
(defun string-head (string &optional how-many)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (string string))
  (if (numberp how-many)
      (if (< how-many 1) ""
	  (let ((out ""))
	    (do ((index 0 (1+ (the fixnum index))))
		((= index (the fixnum (min (the fixnum how-many)
					   (length (the simple-base-string string))))) out)
	      (setq out (concatenate 'string out (the simple-base-string (string (schar string index))))))))
      string))

(defun strings= (list)
  (let ((test-string (car list)))
    (loop for string in (cdr list)
	  when (not (string= test-string string))
	  do (return nil)
	  finally (return t))))

(defun find-tail-char (string char &optional (end-start 0))
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (string string)
	   (character char)
	   (fixnum end-start))
  (do ((i (- (1- (length string)) end-start) (- (the fixnum i) 1)))
      ((or (= i 0) (eq (schar string i) char)) (and (eq (schar string i) char) i))))

(defun string-in-lists (string lists)
  (loop for list in lists when (member string list :test #'string-equal) do (return t)))

(defun find-string-in-strings (target-string string-list)
  (loop for string in string-list
	when (search target-string string)
	do (return t)))

(defun find-strings-in-string (target-string string-list)
  (loop for string in string-list
	when (search string target-string)
	do (return t)))

(defun trim-long-name (string &optional (threshold 7))
  (let ((first. (find-first-. string)))
    (if (> (length string) threshold)
	(concatenate 'string 
		     (string-head string (if first. first. 6))

		     "."
		     (string-tail string (1- (- (length string) (FIND-TAIL-- string)))))
	string)))
	
(defun replace-repeated-character-w-single (string string-character-to-replace)
  (if (not (find (schar string-character-to-replace 0) string))
      string
      (let* ((new-char)
	     (old-char (schar string 0))
	     (out (string old-char)))
	(loop for index from 1 to (1- (length string))
	      do
	      (setq new-char (schar string index))
	      when
	      (or (not (equal new-char (schar string-character-to-replace 0)))
		  (not (equal new-char old-char)))
	      do
	      (setq out (concatenate 'string out (string new-char)))
	      (setq old-char new-char))
	out)))

;; same as (search "he" "hello")?
(defun substring-found (substring parentstring)
  "Look for substring in parentstring, if found, returns start position of substring in parentstring,
otherwise return NIL."
          (loop for start2 from 0 to (1- (length parentstring))
	  when (> (+ start2 (length substring)) (length parentstring))
	  do (return nil)
		  when (string= substring parentstring :start2 start2 :end2 (+ start2
							    (length substring)))
      do (return start2)))

;;; INCREMENT-VERSION-LETTER Maps "" -> "a", "a" -> "b", ... , "z" -> "a"
(defun increment-version-letter (string)
  (if (= (length string) 0) "a"
      (string (increment-char (char string 0)))))

;; INCREMENT-CHAR Maps #\a -> #\b, #\b -> #\c, ... , #\z -> #\a
(defun increment-char (char)
  (case (char-int char)
    (122 #\a)
    (t (code-char (1+ (char-int char))))))


(defun limit-string-span (string max-characters)
  (let ((current-line-length-w-new-word 0))
    (loop for word in (listify-words-in-string string) do
	  (setq current-line-length-w-new-word
		(+ current-line-length-w-new-word
		   1			; for space
		   (length word)))
	  ; (format t "current-line-length-w-new-word ~A @ ~A~%" current-line-length-w-new-word word)
	  when (> current-line-length-w-new-word max-characters)
	  collect #\Newline into out
	  and do (setq current-line-length-w-new-word (length word)) ;  (format t "clear @ ~A~%" word)
	  collect word into out
	  collect " " into out
	  finally
	  (return (concatenate-atoms-to-string-list out))))) 
	

;; *********************************
;; Strings relevant to files
;; *********************************

(defun clean-name (name &key (remove-chars '(#\( #\) #\\ #\/ #\)))
			(front-end-trim-chars '(#\Space #\- #\Tab #\Newline))
			(substitute-space-char '#\_))
  (let ((name (string-trim front-end-trim-chars name)))
    (loop for char in remove-chars  do (setq name (remove char name :test #'char=)))
    (substitute substitute-space-char #\Space (REDUCE-REPEATS name #\Space) :test #'char=)))


(defun make-nice-filename (old-name)
  (clean-name old-name))

(defun make-nice-filename (old-name) 
  (let* ((old-name (typecase old-name
		     (string old-name)
		     (t (format nil "~A" old-name))))
	 (new-name
	  (replace-chars-w-another-in-string
	   (replace-space-with-underscore (replace-char-w-space-in-string old-name #\/))
	   '("["			; Why does this kill %make-pathname ???
	     "]"			; For "symmetry"
	     "*" "'") ":")))
    (if (> (length new-name) 0)
	(if (or (eq (schar new-name 0) #\_) (eq (schar new-name 0) #\-))
	    (make-nice-filename (string-tail new-name (1- (length new-name))))
	    new-name)
	"")))


;; same as pathname-name, dummy
(defun remove-type-from-path (filename)
  (let ((end (search "." filename)))
    (if end (subseq filename 0 end)
	filename)))


#|
(defun remove-dirs-from-path (filename)
  (string-tail filename
	       (- (length filename)
		  (1+  (search "/" filename :from-end t)))))
|#
;; also works
(defun remove-dirs-from-path (filename)
  (let ((start (search "/" filename))
	(end (length filename)))
    (if start
	(remove-dirs-from-path (subseq filename (1+ start) end))
	filename)))

(defun get-file-of-type (directory types &optional rejection-substrings)
  (let ((directory-path (probe-file directory)))
    (if directory-path
	(loop for path in (directory directory-path)
	      when (and (loop for type in (if (listp types) types (list types))
			      when (not (search type (namestring path)))
			      do (return nil)
			      finally (return t))
			(loop for rejection-substring in (if (listp rejection-substrings) rejection-substrings
							     (list rejection-substrings))
			      when (search rejection-substring (namestring path))
			      do (return nil)
			      finally (return t)))
	      collect (namestring path)))))

;; *********************************
;; Converting symbols to nice strings
;; *********************************

#|
(defun make-nice-atom (atom)
  (replace-chars-w-space-in-string
   (string-capitalize (string-trim ":" (format nil "~A" atom)))
   '(#\- #\_)))
|#

(defun nice-string-from-atom (atom &optional (chars-to-replace-w-string '( #\_) ;; '(#\- #\_)
				       ))
  (let ((string (typecase atom
		  (string atom)
		  (t (format nil "~S" atom)))
	  ; (string-trim ":" (format nil "~A" atom))
	  ))
    (replace-chars-w-space-in-string
     (if (all-caps-p string)
	 (string-capitalize string)
	 string)
     chars-to-replace-w-string)))

;; backward comp
(defun make-nice-atom (atom &optional chars-to-replace-w-string)
  (nice-string-from-atom atom chars-to-replace-w-string))

(defun nice-string-from-keyword (keyword)
  ;; Replaces underscores '_' with space, removes leading ':', and capitalizes remaining words. 
  (let ((nice-atom (make-nice-atom keyword)))
    (if (eq (aref nice-atom 0) #\:)
      (string-tail nice-atom (1- (length nice-atom)))
      nice-atom)))

(defun simple-make-nice-atom (atom) (format nil "~A" atom))
(defun SIMPLE-NICE-STRING-FROM-ATOM (atom) (format nil "~A" atom))



;; *********************************
;; Number Formatting
;; *********************************

#|
(defun ANOTHER-nice-FLOAT-FORMAT (x &key range (decimals 1))
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x))
  (if (= x 0) "0.0"
      (let ((decimals (if (or (not range)  (= range 0))
			  (or decimals 1)
			  (max
			   (+ (if (> x 1) 1 2) (floor (abs (log (abs (/ x range)) 10))))
			   (or decimals 1)))))
			
	(if (not (numberp x))
	    (format nil "~A" x)
	    (let* ((format-string (concatenate 'string "~,," (format nil "~D" decimals)
					       (cond ((integerp x) "d")
						     ((<= -2 (floor (log (abs x) 10)) 2) "f")
						     (t "e"))))
		   (formatted-string (format nil format-string x))
		   (formatted-string-exponent-position
		    (or (search "e" formatted-string)
			(search "d" formatted-string)))
		   (formatted-string-mantissa
		    (subseq formatted-string 0 formatted-string-exponent-position))
		   
		   (formatted-string-exponent
		    (when formatted-string-exponent-position
		      (subseq formatted-string formatted-string-exponent-position))))
	      (print decimals)
	      (print format-string)
	      (if formatted-string-exponent-position
		  (format nil "~A~A"
			  (string-right-trim (if (search ".0" formatted-string-mantissa) "" "0")
					     formatted-string-mantissa)
			  formatted-string-exponent)
		  formatted-string))))))
|#

(defun ANOTHER-nice-FLOAT-FORMAT (x &key range (decimals 1))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (typecase x
    (single-float (float-ANOTHER-nice-FLOAT-FORMAT x :range range :decimals (or decimals 1)))
    (fixnum (format nil "~d" x))
    (t (format nil "~A" x))))




(defvar *debug-nice-float-format* nil)
#|
(defun float-ANOTHER-nice-FLOAT-FORMAT (x &key range (decimals 1))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (single-float x)(fixnum decimals))
;  (format t "x ~A, range ~A, origndec ~A ~%" x range decimals)
  (if (= x 0)
      "0.0"
      (let* ((abs-x (abs x))
	     (log-10-x (the sf (log abs-x 10)))
	     (decimals (if (or (not range) (= (the sf range) 0.0))
			   decimals
			   (max (the fn (+ (if (> abs-x 1)
					       1
					       (ceiling (abs log-10-x)))
					    (ceiling (abs (the sf (log (/ abs-x (the sf range)) 10))))))
				decimals)))
	     (x-power (floor log-10-x))
	     (x (/ (round x (expt 10 (- decimals))) (expt 10.0 decimals)))
	     other-candidate-string)
	(declare (fixnum x-power)
		 (single-float abs-x x log-10-x))
;        (format t "Decimals ~A~%" decimals)
	(when (< abs-x 1)
	  (setq other-candidate-string
		(format nil (concatenate 'string "~," (format nil "~D" (max 1 (+ x-power decimals))) "e") x)))
	(let ((formatted-string (format nil "~,f" x)))
;	  (format t "other ~A this ~A ~%" other-candidate-string formatted-string)
	  (if (and other-candidate-string (< (length other-candidate-string) (length formatted-string)))
	      other-candidate-string formatted-string)))))

|#
(defun float-ANOTHER-nice-FLOAT-FORMAT (x &key range (decimals 1))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (single-float x)(fixnum decimals))
  (if (= x 0)
      "0.0"
      (let* ((abs-x (abs x))
	     (log-10-x (the sf (log abs-x 10)))
	     (decimals (if (or (not range) (= (the sf range) 0.0))
			   decimals
			   (max (the fn (+ (if (> abs-x 1)
					       1
					       (+  5 ; 4 ; 3 ;1 ; 2
						  (floor ; (abs log-10-x)
						      log-10-x)))
					   (1+ (floor (abs (the sf (log (/ abs-x (the sf range)) 10)))))))
				decimals)))
	     (x-power (floor log-10-x))
	     (x (/ (round x (expt 10 (- decimals))) (expt 10.0 decimals)))
	     other-candidate-string)
	(declare (fixnum x-power)
		 (single-float abs-x x log-10-x))
	(when *DEBUG-NICE-FLOAT-FORMAT*
        (format t "Decimals ~A, ~A~%" decimals
                (+ (if (> abs-x 1)
                       1
                       (+ 2 (floor log-10-x)))
                   (1+ (floor (abs (the sf (log (/ abs-x (the sf range)) 10))))))))
	(when (< abs-x 1)
	  (setq other-candidate-string
		(format nil (concatenate 'string "~," (format nil "~D" (max 1 (+ x-power decimals))) "e") x)))
	(let ((formatted-string (format nil "~,f" x)))
	  (when *DEBUG-NICE-FLOAT-FORMAT*
	    (print formatted-string)
	    (format t "other ~A this ~A ~%" other-candidate-string formatted-string))
	  (if (and other-candidate-string (< (length other-candidate-string) (length formatted-string)))
	      other-candidate-string formatted-string)))))

(defun float-ANOTHER-nice-FLOAT-FORMAT (x &key range (decimals 1))
  (if (= x 0)
      "0.0"
      (format nil "~f" x)))

(defun trailing-nines (number)
  (loop for char across (reverse (format nil "~f" number))
	unless (eq #\9 char) do (return out)
	sum 1 into out))

(defun float-ANOTHER-nice-FLOAT-FORMAT (x &key range (decimals 1))
  ;;  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))	   (single-float x)(fixnum decimals))
  (if (= x 0)
      "0.0"
      (let* ((abs-x (abs x))
	     (x-power (floor (the sf (log abs-x 10))))
	     (abs-range (when range (abs range)))
	     (range-power (when range (floor (the sf (log abs-range 10)))))
	     (temp (when range (/ (round (/ abs-range (expt 1.0e1 range-power)) 0.0001) 10000)))
	     (range-string (when range (format nil "~f" (nth-value 1 (round temp)))))
	     (trailing-nines (when range (trailing-nines range)))
	     (decimals (if (or (not range) (= (the sf range) 0.0))
			   decimals
			   (max
			    (round (- x-power (or range-power 0)))
			     2 ; 4 ; (if (< range-power 0) (abs range-power) 0)
			    (- 
			     (- (length range-string)
				2)	; for decimal point and leading "0"
			     (max 0 (- trailing-nines 2)))
			    decimals))))
	(when *DEBUG-NICE-FLOAT-FORMAT*
	  (format t "x ~A, range ~A, range-power ~A, temp ~A, range-string ~A, Decimals ~A, trailing-nines ~A~% "
						x range range-power temp range-string decimals trailing-nines))
	
	(let* ((format-args (concatenate 'string "~," (format nil "~D" decimals)
					(if (> (abs (or range-power 0)) 3) "e" "f")))
	       (formatted-string 
		(format nil format-args
		       (* (round (* (/ x (expt 10 (or range-power 0))) 1000.0))
			  (/ (expt 10 (or range-power 0))  1000.0) )
		       ; x
		       )
					; (format nil "~,f" x)
		)
	      (other-candidate-string
	       (when (or (< (abs x) 1) (> (abs x) 1e5))
		 (format nil (concatenate 'string "~," (format nil "~D" (max 1 decimals)) "e") x))))
	  (when *DEBUG-NICE-FLOAT-FORMAT*
	    (print formatted-string)
	    (format t "other ~A this ~A (args ~A) ~%" other-candidate-string formatted-string format-args))
	  (if (and other-candidate-string (< (length other-candidate-string) (length formatted-string)))
	      other-candidate-string formatted-string)))))




(defun my-float-format (x &optional (decimal-digits 1) total-chars left-justify (right-pad-char '#\0))
  (ANOTHER-nice-FLOAT-FORMAT x :decimals decimal-digits))

(defun simple-format-number (number)
  (cond ((< 0 (abs number) .1)
	 (format nil "~,2e" number))
	((< 0 (abs number) 1)
	 (format nil "~,2f" number))
	((< 0 (abs number) 100)
	 (format nil "~,1f" number))
	((< 0 (abs number) 100)
	 (format nil "~,0f" number))
	(t (format nil "~,2e" number))))


#|
(format nil "~/dollars/" 123456789.87) => "$123,456,792.00"
(format nil "~/dollars/" 123456789.87d0) => "$123,456,789.87"
(format nil "~/dollars/" 12345678987/100) => "$123,456,789.87"

note that 123456789.87 requires more significant bits than are available in
single-precision floats and that we suffer massive loss of precision in
floating point computations.  however, this function handles rationals
correctly, which the ~$ format control is not guaranteed to do, as in:

(format nil "~$" 123456789.87) => "123456790.00"
(format nil "~$" 123456789.87d0) => "123456789.87"
(format nil "~$" 12345678987/100) => "123456790.00"
|#
(defun dollars (stream amount colon-p atsign-p
		&optional (width 0) padchar commachar)
  "Print an amount of dollars for humans to read.
The full form is ~width,padchar,commachar:@/dollars/, where width is the
minimum width of the field (default 0), padchar is the character used to pad
to the width on the left end, and commachar is the separator between each
group of three digits.
The @ modifier controls printing of the sign of positive amounts.  If
omitted, a positive value prints without a sign.  Otherwise, a positive
amount has an explicit + sign.
The : modifier controls the position of the sign and the padding.  If
omitted, the dollar sign is printed in the leftmost position, then any
intervening pad characters, then the signed value.  Otherwise, the sign
occupies the leftmost position, and the dollar sign follows any intervening
padchars.
Copyright 1997 by Erik Naggum.  Any use is permitted provided that this
copyright notice is retained and that all changes are duly identified."
  (let* ((digits 
	  (multiple-value-bind (dollars cents) (floor (abs amount) 1)
	    (format nil "~,,V:D.~2,'0D" commachar dollars (round cents 0.01))))
	 (sign (if (minusp amount) #\- (if atsign-p #\+ nil)))
	 (padding (max 0 (- width 1 (if sign 1 0) (length digits)))))
    (format stream "~@[~C~]~V,,,VA~@[~C~]~A"
	    (if colon-p sign #\$)
	    padding padchar ""
	    (if colon-p #\$ sign)
	    digits)))


#|
From: gxcst+@pitt.edu (Gil Citro)
Newsgroups: comp.lang.lisp
Subject: Re: Testing for EOF
Date: 13 Jul 1995 19:34:35 GMT
Organization: University of Pittsburgh
NNTP-Posting-Host: unixs4.cis.pitt.edu
X-Newsreader: TIN [version 1.2 PL2]

From gci@smi.med.pitt.edu Thu Jul 13 15:08:23 1995
Date: Thu, 13 Jul 95 15:06:59 -0400 (EDT)
From: Gil Citro <gci@smi.med.pitt.edu>
To: gxcst+@pitt.edu


Thanks again to everyone who helped me.  I got a request to post what
I did with it so here are the functions I wrote.  I'm sure by looking
at this code you will be able to tell I am a Lisp novice. Probably I
have reinvented the wheel with little pointy corners.  It does seem to
work however.

-Gil

;
;Return the concatenation of all strings in the list GIVEN-STRINGS.
;

(defun join (given-strings)
  (cond ((and (atom given-strings) (stringp given-strings)) given-strings)
	((null given-strings) nil)
	((and (atom (car given-strings)) (not (stringp (car given-strings))))
	 (join (cdr given-strings)))
	((stringp (car given-strings))
	 (coerce (append (coerce (car given-strings) 'list)
			 (coerce (join (cdr given-strings)) 'list))
		 'string))
	((listp (car given-strings))
	 (coerce (append (coerce (join (car given-strings)) 'list)
			 (coerce (join (cdr given-strings)) 'list))
		 'string))))

;
;Return a stream to the file FILE-NAME, with the optional extention
;EXTENTION, if it exists, and nil otherwise.
;

(defun extended-open (file-name &optional extention)
  (if (probe-file file-name)
      (open file-name)
      (when (and (not (null extention))
		 (probe-file (join (list file-name "." extention))))
	(open (join (list file-name "." extention))))))

;
;Return a list of strings each element of which is a line from the 
;file FILE-NAME, with optional extention EXTENTION, if it exists,
;and nil otherwise.
;

(defun lines-to-list (file-name &optional extention)
  (let ((stream (extended-open file-name extention)))
    (when stream
      (do ((line (read-line stream) (read-line stream nil 'eof))
	   (list-of-lines nil (append list-of-lines (list line))))
	  ((eq line 'eof) list-of-lines)))))

|#



#|
> Klaus Berndl (berndl@vogelweide.uni-passau.de) wrote:
> 
> : How to convert a suitable string (i.e. the string contents only digits and perhaps one dot) into a number-format like integer (only digits allowed) or float (only digits and one dot allowed)?


Here's a solution:
|#




(defun parse-float (string)
  "Return a float read from string, and the index to the remainder of string."
  (multiple-value-bind (integer i)
      (parse-integer string :junk-allowed t)
    (multiple-value-bind (fraction j)
	(parse-integer string :start (+ i 1) :junk-allowed t)
      (values (float (+ integer (/ fraction (expt 10 (- j i 1))))) j))))



#|
CL-USER> (parse-float "  123.456 ")
123.456
9
-- 
Peter Norvig                  | Phone: 415-833-4022           FAX: 415-833-4111
Harlequin Inc.                | Email: norvig@harlequin.com
1010 El Camino Real, #310     | http://www.harlequin.com
Menlo Park CA 94025           | http://www.cs.berkeley.edu/~russell/norvig.html


|#


(export '(number-of-newlines
	  sequence-to-string-list
	  STRING-HAS-SPECIAL
	  string-has-non-number
	  STRING-MEMBER
	  SEQ-TO-STRING
	  ARRAY-TO-STRING
	  char-seq-to-string
	  string-remove

	  MAKE-NICE-FILENAME
	  get-file-of-type
	  remove-type-from-path
	  substring-found
	  remove-dirs-from-path
	  
	  find-tail-- find-first-. string-remove-head string-remove-tail string-tail string-head find-tail-char
	  trim-long-name 

	  my-float-format	  
	  simple-format-number
	  ANOTHER-nice-FLOAT-FORMAT
	  FLOAT-ANOTHER-NICE-FLOAT-FORMAT

	  strings=
	  increment-char increment-version-letter

	  replace-repeated-character-w-single REPLACE-CHAR-W-SPACE-IN-STRING
	  replace-space-with-underscore replace-tabs-in-string
	  REPLACE-UNDERSCORE-WITH-SPACE
	  replace-chars-w-space-in-string
	  REPLACE-CHAR-W-ANOTHER-IN-STRING
	  REPLACE-CHARS-W-ANOTHER-IN-STRING

	  REMOVE-SPACES

	  CONCATENATE-STRING-LIST
	  CONCATENATE-ATOMS-TO-STRING-LIST
	  CONCATENATE-STRINGS
	  ADD-LINEFEEDS-TO-STRING-LIST

	  parse-float
	  PRINT-SPACES

	  find-string-in-strings
	  find-strings-in-string
	  string-in-lists

	  convert-string-to-vertical
	  ALL-NUMBERS-IN-STRING
	  listify-words-in-string
	  dollars

	  limit-string-span
	  all-caps-p	  
	  make-nice-atom
	  nice-string-from-keyword
	  nice-string-from-atom
	  simple-make-nice-atom
	  SIMPLE-NICE-STRING-FROM-ATOM))  
