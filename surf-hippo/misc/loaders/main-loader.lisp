;;; ;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-


;; This loads the Garnet and Surf-Hippo systems, according to the Unix
;; environment variables GARNETHOME and SURFHOME.
;; LBG 9/30/92


;; This is loaded into a fresh CMUCL Ilisp environment, or one with GARNET added.
;; LBG 10/0/94





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;; Define Various Globals, Macros and Functions Used During Compiling/Loading
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar surf-hippo-compile nil)
(defvar *force-all-compile* nil)
(defvar *clear-sh-packages* nil)	; Clears all symbols in the WH, PH, and SURF-HIPPO packages
					; at the beginning of the load.

#|
(defvar garnet-version nil)

(cond ((string= "17f" (lisp-implementation-version)))
      ((string= "17c" (lisp-implementation-version))
       (setq garnet-version :cmu-17c)))
|#

(defmacro defvars (&rest list)
  `(progn ,@(loop for variable in list collect `(defvar ,variable))))

(defmacro defvars-w-value (&rest list)
   `(progn ,@(loop for (variable value) in list collect `(defvar ,variable ,value))))

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

(defun fixup-pathname (pathname)
  (when pathname (replace-repeated-character-w-single (format nil "~A/" pathname) "/")))

(defun touch-file (filename)
  (when (probe-file filename) (unix::run-program "touch" (list filename))))

(defun find-files-to-compile (src-dir bin-dir candidates &key (all *force-all-compile*) files-to-force-compile-all)
  (unless all
    (when files-to-force-compile-all
      (setq all (or all (find-files-to-compile src-dir bin-dir files-to-force-compile-all)))
      (setq *force-all-compile* all)))
  (loop for file in candidates
	when (let* ((src-file (merge-pathnames (format nil "~A.lisp" file) Src-dir))
		    (bin-file (merge-pathnames (format nil "~A~a" file *compiler-extension*) bin-dir))
		    (src-exists (probe-file src-file))
		    (bin-exists (probe-file bin-file))
		    (src-write-date (when src-exists (file-write-date src-file)))
		    (bin-write-date (when bin-exists (file-write-date bin-file))))
	       (when (or (and src-exists (not src-write-date))
			 (and bin-exists (not bin-write-date)))
		 (format t "Funny, FILE-WRITE-DATE is not always reliable...~%"))
	       (when (and src-exists (or all (not bin-exists) (> src-write-date bin-write-date)))
		 ;; In case compile bugs out in middle, uncompiled files will be picked up on the next
		 ;; try at compiling the system.
		 (when (and all (not *enable-surf-hippo-compile*))
		   (when (or (not bin-exists) (> bin-write-date src-write-date))
		     ;; (when all (format t "Touching ALL!!") (break))
		     (touch-file (namestring src-file)))
		   (setq *force-all-compile* t))	      
		 t))
	collect file))

#|
(defun compile-source-directory (src-dir bin-dir candidates &key files-to-force-compile-all enable-compile)
  (format t "*****************  Compile source directory ~A.....~%" src-dir)
  (let ((files-to-compile
	 (when enable-compile
	   (FIND-FILES-TO-COMPILE src-dir bin-dir candidates :files-to-force-compile-all files-to-force-compile-all))))
    (dolist (file candidates)
      (if enable-compile
	  (progn
	    (when (member file files-to-compile) (compile-file (merge-pathnames file src-dir)))
	    (load (merge-pathnames file (if (member file files-to-compile) src-dir bin-dir)))
	    (when (member file files-to-compile)
	      (unix::run-program
	       "csh" (list "-fc"  
			   (format nil
				   "mv ~a*~a  ~a"
				   ;; "mv $SURFHOME/src/cmucl-fixes/*.sparcf  $SURFHOME/bin/cmucl-fixes/"
				   src-dir  *compiler-extension* bin-dir)))				   
	      nil))	    
	  (load (merge-pathnames file bin-dir) :verbose T)))))
|#

(defvar *enable-surf-hippo-compile* t)

(defun compile-source-directory (src-dir bin-dir candidates &key files-to-force-compile-all enable-compile)
  (when *enable-surf-hippo-compile*
    (format t "*****************  Compile source directory ~A.....~%" src-dir))
  (let ((files-to-compile
	 (when enable-compile
	   (FIND-FILES-TO-COMPILE src-dir bin-dir candidates :files-to-force-compile-all files-to-force-compile-all))))
    (when *enable-surf-hippo-compile*
      (dolist (file candidates)
	(when (and enable-compile (member file files-to-compile))
	  (compile-file (merge-pathnames file src-dir)
			:output-file (merge-pathnames (format nil "~a~a" file *compiler-extension*) bin-dir)))
	(load (merge-pathnames file bin-dir) :verbose T)))))

(defvar *SURF-HOME* nil "The pathname for the top-level Surf-Hippo home directory. Set by the SURFHOME
environment variable, if exists, otherwise by the HOME environment variable.")

(defvar *surf-user-home* "" "The pathname for the user home directory. Set by the HOME environment variable.")
(defvar Surf-Hippo-Pathname nil)
(defvar *Surf-parameters-dir*)

;;; $$$$ CUSTOMIZE $$$$
(defvar *Surf-user-dir* nil "This is the top level Surf-Hippo directory for the user, set by the SURFUSERHOME environment
variable, otherwise by $HOME/surf-hippo/. Directory for simulation data, etc.")

(defvar *printer* "")

(defun update-main-system-variables ()
  (setq *SURF-HOME*
	;; **** SET THIS LINE IF NECESSARY to another value for *SURF-HOME* for example,
	;; "/usr/your-name/surf-hippo/"
	(let ((surfhome (cdr (assoc :SURFHOME lisp::*environment-list*)))
	      (home (cdr (assoc :HOME lisp::*environment-list*))))
	  (if surfhome
	      (concatenate 'string (string-right-trim '(#\/) surfhome) "/")
	      (concatenate 'string (string-right-trim '(#\/) home) "/surf-hippo/"))))
  (setq *printer* (cdr (assoc :PRINTER lisp::*environment-list*)))
  (setq Surf-Hippo-Pathname *surf-home*)
  (setq *Surf-parameters-dir* (fixup-pathname (concatenate 'string *surf-home* "/src/parameters/"))))

(update-main-system-variables)

(export '(compile-source-directory
	  update-main-system-variables
	  *printer*
	  *SURF-HOME*
	  *Surf-parameters-dir*
	  *SURF-USER-DIR* 
	  *SURF-USER-HOME*
	  Surf-hippo-pathname))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;            Load The Surf-Hippo and Garnet System Files
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when surf-hippo-compile (load (concatenate 'string *surf-home* "/misc/loaders/surf-hippo-prepare-compile")))

(load (concatenate 'string *surf-home* "/lib/surf-hippo-version"))

(defparameter Surf-Hippo-Version-Date
  (system::format-universal-time
   nil
   (loop for file in (loop for src-dir in (directory (concatenate 'string *surf-home* "/src/*"))
			   nconc (directory (format nil "~A/*.lisp" (namestring src-dir))))
	 maximize (file-write-date file))
   :PRINT-SECONDS nil :print-weekday nil))
  
(export '(Surf-Hippo-Version-Date))



;; surf-hippo-setup loads cmucl fixes also.
(load (concatenate 'string *surf-home* "/misc/loaders/surf-hippo-setup"))

(unless (find :garnet *features*)
  (load (concatenate 'string (cdr (assoc :GARNETHOME lisp::*environment-list*)) "/garnet-loader" )))

(load (concatenate 'string *surf-home* "/misc/loaders/surf-hippo-loader"))

;; All bin files should now be moved after compile at the end of the COMPILE-SOURCE-DIRECTORY call
;; for each subssytem.
(when surf-hippo-compile (unix::run-program (concatenate 'string *surf-home*
							 (cond
							  ((string= ".fasl" *compiler-extension*)
							   "/misc/loaders/move_bins_fasl")
							  (t "/misc/loaders/move_bins_sparcf")))
					    nil))   

(setq surf-hippo-compile nil
      *force-all-compile* nil
      *clear-sh-packages* nil)

(lisp::purify)