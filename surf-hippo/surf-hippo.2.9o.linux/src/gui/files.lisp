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


;; GUI Source file: files.lisp

(IN-PACKAGE "WINDOWS-HACK")

#|
(defconstant disk-block-size 8192)

(defun read-number-file (filename &key max-length)
  "Reads sequential numbers from FILENAME, returns these numbers as a list. File is read until end,
unless MAX-LENGTH is supplied in which case this value, which must be a fixnum, sets the number of
numbers read."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((seq (make-array (or max-length DISK-BLOCK-SIZE) :adjustable t)))
    (with-open-file (input filename :direction :input)
      (adjust-array seq
		    (COMMON-LISP::READ-SEQUENCE
		     seq
		     input)))
    (sequence-to-list seq)))

|#



(defun read-number-file (filename &key max-length)
  "Reads sequential numbers from FILENAME, returns these numbers as a list. File is read until end,
unless MAX-LENGTH is a number in which case this [rounded] value sets the number of numbers read."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((max-length (when (numberp max-length) (round max-length))))
    (with-open-file (input filename :direction :input)
      (let (value)
	(loop for count fixnum from 1
	      until (or (and max-length (= count (the fn max-length)))
			(eq 'eof (setq value (read input nil 'eof))))
	      collect value)))))

(defun replace-tilde-in-path (pathstring &optional (user-directory *Surf-user-home*))
  (let ((slashtilde-position (search "/~/" pathstring :test 'string= :from-end t))
	(tilde-position (search "~/" pathstring :test 'string=)))
    (if (or slashtilde-position (and tilde-position (= tilde-position 0)))
	(concatenate 'string
		     user-directory
		     (string-tail pathstring
				  (- (length pathstring) (or (when slashtilde-position
							       (+ 3 slashtilde-position))
							     (+ 2 tilde-position)))))
	pathstring)))


(defvar *default-lisp-data-package-name* "USER")
(defvar *results-filename* "")

(defun fixup-pathname-directory (pathname)
  (typecase pathname
    (string
     (replace-char-w-another-in-string
      (replace-repeated-character-w-single (format nil "~A/" pathname) "/")
      "'" ":"))
    (pathname pathname)))
    
(defun directory-namestring-no-colons (pathname)
  (directory-namestring (string-head pathname (search ":" pathname))))

#|
  (let ((command-string
	 (concatenate 'string "test -d " pathname " && echo 1")))
    (unless (equal "" (shell-exec command-string))
	    T))
|#

#|
UNIX-FILE-KIND is an external symbol in the UNIX package.
Function: #<Function UNIX:UNIX-FILE-KIND {1128001}>
Function arguments:
  (name &optional check-for-links)
Function documentation:
  Returns either :file, :directory, :link, :special, or NIL.
Its defined argument types are:
  (SIMPLE-BASE-STRING &OPTIONAL T)
Its result type is:
  (MEMBER :DIRECTORY :FILE :LINK :SPECIAL NIL)
|#

(defun full-pathname-p (pathname)
  "Given a PATHNAME string or pathname, returns T if this refers to a full pathname including an
existing directory."
  (let ((pathname (typecase pathname
		    (string (when (> (length pathname) 0) pathname))
		    (pathname pathname))))
    (and pathname
	 (let ((directory-namestring (directory-namestring-no-colons pathname)))
	   (and
	    (> (length directory-namestring) 0)
	    (probe-file directory-namestring)
	    (> (length (namestring pathname)) (length directory-namestring)))))))

(defun write-file-overwrite-authorization (pathname)
  (or (not (probe-file (ext:unix-namestring pathname nil)))
      (go-ahead-menu (format nil "File ~A already exists. Overwrite?" pathname))))


(defun simple-format-list (thing &optional (stream t) (count 0))
  "Print out list THING to STREAM [default T]."
  (typecase thing
    (cons (let ((how-many (length thing)))
	    (loop for elt in thing
		for count from 1
		when (= count 1) do (format stream " (")
		do (simple-format-list elt stream count)
		when (= count how-many) do (format stream ")"))))
    (t (format stream "~a~S" (if (= 1 count) "" " ") thing))))

;; FORMAT-LIST Writes out a numerical LIST to STREAM in a reasonable format for both legibility and
;; space saving. At some point we will need to save data in a true binary format.
(defun format-list (list &optional (values-per-line 10) (stream t) (precision-for-text 6) (indent 0)
			 (space-char " ")
			 (numbers t)
			 include-parens )
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((*print-pretty* nil)
	(value-counter 1)
	(format-string (concatenate 'string "~"
				    (if numbers
					(format nil "~df" (if (numberp precision-for-text) precision-for-text 6))
					(format nil "s")))))
    (declare (fixnum value-counter values-per-line))
    (print-spaces stream indent)
    (when include-parens
      (format stream "(~%")
      (print-spaces stream indent))
    (loop for data-point in list do
	  (format stream format-string (the single-float data-point)) ; Enough for text output.
	  (when space-char (format stream "~A" space-char))
	  (if (= value-counter values-per-line) ;Print 10 values per line for legibility
	      (progn
		(format stream "~%")
		(print-spaces stream indent)
		(setq value-counter 1))
	      (setq value-counter (1+ value-counter))))
    (when include-parens
      (format stream ")~%"))
    (format stream "~%")))


(defun formatted-list-dump (list &optional (stream t) start-line-with-semicolon (values-per-line 10) (indent 0)
				 suppress-quote)
  (print-spaces stream indent)
  (if suppress-quote
      (format stream "(")
      (format stream "`("))
  (loop for element in (if (consp list) list (list list)) for i from 1
	do (if (consp element)
	       (progn  (format stream "~% ")
		       (formatted-list-dump element stream start-line-with-semicolon values-per-line
					    (+ 1 indent) t))
	       (format stream " ~s " element))
	when (= (mod i values-per-line) 0)
	do (format stream "~% ")
	(print-spaces stream indent)
	(when start-line-with-semicolon (format stream ";;  ")))
  (print-spaces stream indent)
  (format stream ")"))


(defun pathname-from-filename-or-pathname-or-default
    (filename pathname-directory &optional (default-directory-type :data))
  (let ((pathname-directory
	 (replace-repeated-character-w-single
	  (format nil "~A/" pathname-directory) "/")))
    (if (> (length (directory-namestring filename)) 0)
	(directory-namestring filename)
	(if (> (length (directory-namestring pathname-directory)) 0)
	    (directory-namestring pathname-directory)
	    (case default-directory-type
	      (:data (get-data-directory))
	      (:plot (get-plot-directory)))))))

(defun write-lisp-header (filename &optional supersede)
  (unless (probe-file filename)
    (with-open-stream
	(*standard-output*
	 (open filename :direction :output :if-exists (if supersede :supersede :append) :if-does-not-exist :create))
      (format t ";;; -*- Package: ~a; Mode: LISP -*-~%~%" *default-lisp-data-package-name*)
      (format t "~%~%~%"))))

#|
(defmacro provide-pathname-and-filename ((filename pathname-directory)  &body body)
  `(when (> (length ,filename) 0)
    (let* ((pathname (pathname-from-filename-or-pathname-or-default ,filename ,pathname-directory))
	   (filename (if (full-pathname-p ,filename)
			 ,filename
			 (format nil "~A~A" pathname (remove-dirs-from-path ,filename)))))
      (when (full-pathname-p filename)
	,@body))))
|#

(defun provide-pathname-and-filename (&optional (filename (gensym)) (pathname-directory ""))
  (let ((filename (format nil "~A" filename))
	Pathname)
    (when (> (length filename) 0)
      (setq pathname (pathname-from-filename-or-pathname-or-default filename pathname-directory))
      (setq filename (if (full-pathname-p filename)
			 filename
			 (format nil "~A~A" pathname (remove-dirs-from-path filename))))
      (when (full-pathname-p filename)
	(unix:unix-mkdir (ext:unix-namestring pathname nil) #o777))
      (values (when (full-pathname-p filename) filename)
	      (when (probe-file (ext:unix-namestring pathname nil)) pathname)))))

(defun dump-result (result &key (filename *results-filename*) (pathname-directory "") announce-write indent)
  (multiple-value-bind (filename pathname)
      (provide-pathname-and-filename filename pathname-directory)
    (when pathname
      ;; Write RESULT to file.
      (with-open-stream (*standard-output* (open filename :direction :output :if-exists :append
						 :if-does-not-exist :create))
	(format t "~%")
	(print-spaces t indent)
	(format t "~A" result))
      (when announce-write (format t ";; File ~a written~%" filename))
      t)))

#|
(defun dump-result (result &key (filename *results-filename*) (pathname-directory "") announce-write indent)
  (when (> (length filename) 0)
    (let* ((pathname (pathname (pathname-from-filename-or-pathname-or-default filename pathname-directory)))
	   (filename (if (full-pathname-p filename)
			 filename (format nil "~A~A" pathname (remove-dirs-from-path filename)))))
      (when (full-pathname-p filename)
	(unix:unix-mkdir (ext:unix-namestring pathname nil) #o777)
	(when (probe-file (ext:unix-namestring pathname nil))
	  ;; Write RESULT to file.
	  (with-open-stream (*standard-output* (open filename :direction :output :if-exists :append))
	    (format t "~%")
	    (print-spaces t indent)
	    (format t "~A" result))
	  (when announce-write (format t ";; File ~a written~%" filename))
	  t)))))
|#


(defun dump-result-to-lisp-file (result &key (filename *results-filename*) (pathname-directory "") announce-write indent
					(if-file-exists :append))
  (multiple-value-bind (filename pathname)
      (provide-pathname-and-filename filename pathname-directory)
    (when pathname
      (write-lisp-header filename)
      ;; Write RESULT to file.
      (with-open-stream (*standard-output* (open filename :direction :output :if-exists if-file-exists))
	(format t "~%")
	(print-spaces t indent)
	(typecase result
	  (cons (formatted-list-dump result))
	  (string (format t "~a" result))
	  (number (my-float-format result))
	  (t (format t "~A" result))))
      (when announce-write (format t ";; File ~a written~%" filename))
      t)))

(defun dump-lists-2-column (list1 list2 &key (filename *results-filename*) (pathname-directory "") announce-write indent
				  comment supersede)
  "Writes the values in LIST1 and LIST2 in a 2 column format to FILENAME under PATHNAME-DIRECTORY. If
PATHNAME-DIRECTORY not supplied, then derive one under the surf-hippo data directory. If included,
COMMENT is written to file first, followed by the data."
  (multiple-value-bind (filename pathname)
      (provide-pathname-and-filename filename pathname-directory)
    (when pathname
      ;; Write RESULT to file.
      (with-open-stream
	  (*standard-output*
	   (open filename :direction :output :if-exists (if supersede :supersede :append) :if-does-not-exist :create))
	(format t "~%")
	(when comment (format t "~A~%" comment))
	(loop for val1 in list1
	      for val2 in list2 do
	      (print-spaces t indent)
	      (format t "~A  ~A~%" val1 val2)))
      (when announce-write (format t ";; File ~a written~%" filename))
      t)))

(defun write-lists-multi-column (lists &key (filename *results-filename*) (pathname-directory "") announce-write indent
				       comment supersede (column-width 20) QUIT-ON-FIRST-NULL)
  "Writes the values in the sublists of LISTS in a multi-column format to FILENAME under
PATHNAME-DIRECTORY. If PATHNAME-DIRECTORY not supplied, then derive one under the surf-hippo data
directory. If included, COMMENT is written to file first, followed by the data. Columns are
tabbed by COLUMN-WIDTH spaces [default 20]. If a given sublist is empty while running through all
the sublists of list, then a space is output for the corresponding column entry, unless
QUIT-ON-FIRST-NULL is T [default NIL], in which case the file is closed."
  (multiple-value-bind (filename pathname)
      (provide-pathname-and-filename filename pathname-directory)
    (when pathname
      ;; Write RESULT to file.
      (with-open-stream
	  (*standard-output*
	   (open filename :direction :output :if-exists (if supersede :supersede :append) :if-does-not-exist :create))
	(format t "~%")
	(when comment (format t "~A~%" comment))
	(let ((still-printing t)
	      force-quit
	      (format-string  (format nil "~~{~~A ~~,~DT ~~} ~~%" (round column-width))))
	  (loop for count from 0
		while still-printing do
		(print-spaces t indent)
		(setq still-printing nil)
		(let ((out (loop for list in lists
				 collect 
				 (if (< count (length list))
				     (progn (setq still-printing t)
					    (nth count list))
				     (progn (when QUIT-ON-FIRST-NULL (setq force-quit t))
					    "")))))
		  (when force-quit (setq still-printing nil))
		  (when still-printing (format t format-string out))))))
      (when announce-write (format t ";; File ~a written~%" filename))
      t)))

(export '(read-number-file
	  PROVIDE-PATHNAME-AND-FILENAME
	  replace-tilde-in-path
	  dump-RESULT
	  WRITE-LISTS-MULTI-COLUMN
	  DUMP-LISTS-2-COLUMN
	  WRITE-LISP-HEADER
	  PATHNAME-FROM-FILENAME-OR-PATHNAME-OR-DEFAULT
	  *default-lisp-data-package-name* *RESULTS-FILENAME*
	  fixup-pathname-directory full-pathname-p
	  DIRECTORY-NAMESTRING-NO-COLONS
	  write-file-overwrite-authorization
	  dump-result-to-lisp-file simple-format-list FORMAT-LIST FORMATTED-LIST-DUMP	  ))

