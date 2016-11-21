
( provide "OUR-FS")

#-:kcl (defpackage OUR-FS)
(in-package "OUR-FS")

(use-package '("USER" "LISP" #+Genera "SCL"))

#-:our-fs
(progn 
  (print "loading pathname abstraction our-fs")
  (defvar *compile-in-use* nil)
  (defvar compile-list nil)
  (defvar compile-forward-list nil)
  (defvar compile-taxon-list nil)
  (defvar compile-contax-list nil)
  (defvar compile-relfun-list nil)
  ;(export '(compile-all))
  
  
  ; Not usefull now, because comiple-list is allways empty
  ; use the special compile comands
  #|
  (defun compile-all () 
	 (mapcar
	  `(lambda (x)
		   (let ((*our-current-pck* *package*))
			(in-package 'user)
			(compile-file  x)
			(setf *package* *our-current-pck*)))
	  (reverse our-fs::compile-list)))
  |#
  
  
  ; compile special compile-lists
  (defun compile-subsystem (y) 
	 (mapcar
	  `(lambda (x)
		   (let ((*our-current-pck* *package*))
			(in-package 'user)
			(compile-file  x)
			(setf *package* *our-current-pck*)))
	  (reverse y)))
  
  (defun compile-forward ()
	 (compile-subsystem our-fs::compile-forward-list))
  
  (defun compile-taxon ()
	 (compile-subsystem our-fs::compile-taxon-list))
  
  (defun compile-contax ()
	 (compile-subsystem our-fs::compile-contax-list))
  
  (defun compile-relfun ()
	 (compile-subsystem our-fs::compile-relfun-list))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; abstract from physical pathnames for Lucid, Kcl and Genera !!!
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  
  
  (export '(*compile-in-use* 
	    use-file        
	    use-file2        ; alte Version
	    logdir-define
	    logdir-undef
	    logdir-redef
	    logdir-push-typelist
	    logdir-push-typelist-front 
	    logdir-push-typelist-end
	    logdir-pop-typelist
	    logdir-extend1  
	    logdir-extend
	    logdir-file
	    logdir-dir
	    logdir-pwd
	    logdir-cd))
  
  
  
  ; ------------------------------
  ; ----- Global definitions -----
  ; ------------------------------
  
  
  (defvar table nil)                            ; assoc list for keys
  (defvar types `("lisp" "lsp" NIL))            ; default file types
  (defvar type-stack nil)
  (defvar up-symbol #+Kcl :parent               ; symbols for moving one Directory back
	  #+Lucid ".."                
	  #-(or Kcl Lucid) :up)       ; X3J13: ':up' or ':back'
  
  
  
  
  ; ---------------------------------
  ; ----- some intern functions -----
  ; ---------------------------------
  
  
  ;
  ;     key tools
  ;
  
  (defun stop-if-key-invalid (key fname)
	 (declare (special table))
	 (cond ((not (assoc key table))
		(error "The key ~S doesn't exists in Function ~S" key fname ))))
  
  (defun look-up (key)
	 "returns internal representation of key"
	 (declare (special table))
	 (cdr (assoc key table)))
  
  
  ;
  ;     pathname tools  
  ;
  
  (defun add-slash (path)
	 "If path is a string then add a slash if the last character isn't a slash.
	 If path is a pathname simply return it"
	 (cond ((pathnamep path) path)
	       (t (cond ((eql #\/ (char path (- (car (array-dimensions path)) 1)))
			 (string path))
			( t
			 (concatenate 'string path (string #\/)))))))
  
  
  
  (defun remove-up (path)
	 "remove the first up-symbol and its preceding directroy commponent
	 in the path string-list. Path must have a up-symbol and
	 it must not be the first commponent."
	 (let ((result-path ()))
	      (do* ((spath (cdr path) (cdr spath))
		    (dname (car path) testsym)
		    (testsym (car spath) (car spath)))
		   ((equal testsym up-symbol)
		    (cond ((eql ':root dname) (error "Can't remove ROOT!")))
		    (nconc result-path (cdr spath)))
		   (setq result-path (nconc result-path (list dname))))))
  
  (defun clean-path (path)
	 "remove all up-symbols from a pathname
	 the value is a string-list."
	 (cond ((not (listp path)) path)
	       ( t
		(cond ((equal up-symbol (car path))
		       (error "the key ~S starts with ~S" key up-symbol)))
		(let ((lpath path))
		     (loop (cond ((member up-symbol lpath :test #'equal)
				  (setq lpath (remove-up lpath)))
				 ( t (return lpath))))))))
  
  
  
  ;
  ;     Shell specific tests
  ;
  
  #-Kcl (defun relative-path (path)
	       (cond ((eql ':relative (car path)) t)
		     ( t nil)))
  
  #+Kcl (defun relative-path (path)
	       (cond ((eql ':root (car path)) nil)
		     ( t t)))
  
  
  
  ;
  ;     Shell specific function-calls
  ;
  
  #-Genera (defun special-make-pathname (file key suf)
		  (if suf
		      (make-pathname :name file
				     :directory (look-up key)
				     :type suf)
		      (make-pathname :name file
				     :directory (look-up key)
				     )))
  
  
  
  #+Genera (defun special-make-pathname (file key suf)
		  (cond (suf (make-pathname :raw-name file
					    :directory (look-up key)
					    :raw-type suf))
			(t   (make-pathname :name (pathname-name file)
					    :directory (look-up key)
					    :type (pathname-type file)))))
  
  ;
  ;     Shell specific functions/macros
  ;
  
  #-Genera (defun no-type (file)
		  (not (pathname-type file)))
  
  #+Genera (defun no-type (file)
		  (cond ((eql ':unspecific (pathname-type file)) t)
			( t nil)))
  
  
  #-Kcl (defun rem-relative (path) (cdr path))
  
  #+Kcl (defun rem-relative (path) path)
  
  ; --------------------------
  ; ----- User functions ----- 
  ; --------------------------
  
  
  ;
  ; Define Enviroment
  ;
  
  (defun logdir-push-typelist (new-typelist)
	 "redefines typelist and pushs the old on stack"
	 (declare (special type-stack))
	 (push types type-stack)
	 (setq types new-typelist))
  
  
  
  
  (defun logdir-pop-typelist ()
	 "get back last typelist from stack"
	 (declare (special type-stack))
	 (let ((old-types (pop type-stack)))
	      (cond (old-types (setq types old-types))
		    (t types))))
  
  
  
  
  ;
  ; Define Keys
  ;
  
  
  (defun logdir-define (key path)
	 "Define a new key"
	 (declare (special table))
	 (cond ((assoc key table)
		(error "in 'logdir-define' the key ~S already exists!" key)))
	 (push (cons key (clean-path (pathname-directory (add-slash path)))) table))
  
  
  (defun logdir-undef (key)
	 "Removes a key from the key-list"
	 (declare (special table))
	 (setq table (remove (assoc key table) table)))
  
  (defun logdir-redef (key path)
	 "Redefines a Key (first delete old key an then define new)"
	 (declare (special table))
	 (logdir-undef key)
	 (logdir-define key path))
  
  
  
  
  ;
  ; Extending the Value of a Key
  ;
  
  ;(defun logdir-extend1 (key one-step)
	  ;	(declare (special table))
	  ;	(make-pathname :directory (append (look-up key)
						  ;	                                  (list one-step))))
  
  (defun logdir-extend1 (key one-step)
	 "Add a direname to a key"
	 (declare (special table))
	 (stop-if-key-invalid key "logdir-extend1")
	 (make-pathname :directory 
			(append (look-up key)
				(rem-relative
				 (pathname-directory (add-slash one-step))))))
  
  
  (defun logdir-extend (key one-step)
	 (declare (special table))
	 (stop-if-key-invalid key "logdir-extend")
	 (let ((path (pathname-directory (add-slash one-step))))
	      (cond ((not (relative-path path)) nil)
		    ( t
		     (make-pathname :directory 
				    (clean-path (append (look-up key)
							(rem-relative path))))))))
  
  
  
  ;
  ; Request File- or Dirname
  ;
  
  
  (defun logdir-file (key file &key (force nil)
			  (suffixl types))
	 (declare (special table))
	 (stop-if-key-invalid key "logdir-file")
	 (do* ((typelist suffixl (cdr typelist))
	       (suf (if (no-type file)
			(car typelist)
			nil)
		    (car typelist))
	       (filen (special-make-pathname file key suf)
		      (special-make-pathname file key suf)))
	      ((or (not suf) (probe-file filen) force)
	       (cond ((or force  (not (no-type file))) filen)
		     (t
		      (probe-file filen))))))
  
  
  (defun logdir-dir (key)
	 (declare (special table))
	 (stop-if-key-invalid key "logdir-dir")
	 (make-pathname :directory (look-up key)))
  
  
  
  ;
  ; Handel the current directroy
  ;
  
  
  (defun logdir-pwd ()
	 (logdir-dir ':current-directory))
  
  
  (defun logdir-cd (path)
	 (declare (special table))
	 (cond ((relative-path (pathname-directory (add-slash path)))
		(logdir-redef ':current-directory (logdir-extend ':current-directory path)))
	       ( t
		(logdir-redef ':current-directory path)))
	 (logdir-dir :current-directory))
  
  
  ;
  ; Load a file
  ;
  
  
  ; Falls noch Problem (mit use-file) auftreten sollten kann 
  ; vorl"aufig diese alte Version benutzt werden.
  (defun use-file (x)
	 ; (compile-file x)
	 (if *compile-in-use* (push x compile-list))
	 ;(if *compile-in-use* (compile-file x))
	 (load x))
  
  
  (defun use-file2 (file &key (suffixl types)
			 (key :current-directory))
	 "Load files with relative or absolute pathnames"
	 (stop-if-key-invalid key "use-file")
	 ; ---------------------------------------------------------------------------
	 ; --- sobald jeder Defsystem benutzt, kann die n"achste Zeile gl"oscht werden
	 ; ---------------------------------------------------------------------------
	 (if *compile-in-use* (push file compile-list))
	 (cond ((relative-path (pathname-directory (add-slash file)))
		(let ((fname (logdir-file key file :suffixl suffixl)))
		     (cond (fname (load fname))
			   ( t (error "in USE-FILE the file ~S doesn't exists in ~S" file key)))))
	       ( t (load file))))
  
  
  ; ----------------------
  ; ----- initialize -----
  ; ----------------------
  
  (logdir-define :root "/")
  
  (logdir-define :current-directory "/")

  
  ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; additional push-typelist function definitions, using the existing functions
;;; "logdir-push-typelist" and "logdir-pop-typelist"  
  
;;; added 25/11/92 Michael Kreinbihl
;;; 
  
  (defun logdir-push-typelist-front (new-elements)
	 " Gets the old typelist and pushes the new-elements to the front of the old 
	 typelist "
	 (our-fs:logdir-push-typelist 
	  (append new-elements
		  (remove NIL (our-fs:logdir-pop-typelist))
		  '(NIL))))
  
  (defun logdir-push-typelist-end (new-elements)
	 " Gets the old typelist and pushes the new-elements to the end of the old 
	 typelist "
	 (our-fs:logdir-push-typelist 
	  (append (remove NIL (our-fs:logdir-pop-typelist))
		  new-elements
		  '(NIL))))



;;;
#+lucid (setf lcl:*load-source-pathname-types* (adjoin "lsp" lcl:*load-source-pathname-types*  )) 
;;;

(pushnew :our-fs user::*features*)


