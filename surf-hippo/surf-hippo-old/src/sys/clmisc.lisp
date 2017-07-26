;;; ;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-

;;; clmisc.lisp
;;;    misc funcs needed for CL system
;;;    load and compile files for both akcl and lucid systems
;;;    created 6/91 - DLS
;;;    modified 4/92 - lbg
;;; also set up for cmucl

;;; A cheap version for now.
#+cmu
(defun prompt-and-read (type prompt)
  (declare (ignore type))
  (print prompt)
  (read))


;; cmucl needs this apparently - this was taken from clx.l lbg 4-25-92
#+cmu
(deftype boolean () '(or null (not null)))

#+akcl
(set-macro-character #\%
    #'(lambda (stream char) (values (read-line stream))))

(defun display-message (m) (format t (concatenate 'string m "~%")))

(defun error-message (m &rest args)
  (format *error-output* (concatenate 'string m "~%") args))

;;; Add default path/prefix/suffix for loading and compiling files.
;;;; See READ-ME file for setting SURFHOME environment variable.

(defparameter *Surfdir*
  #+cmu
  (cdr (assoc :SURFHOME lisp::*environment-list*))
  #+akcl
  (if (system:getenv "SURFHOME")
      (system:getenv "SURFHOME")
      (concatenate 'string (system:getenv "HOME") "/surf/surf-n-turf"))
  )

;;; make this accessible from surf package. 4-25-92 lbg
(export '(*Surfdir* boolean  display-message error-message  prompt-and-read))


(defparameter *default-load-file-prefix (concatenate 'string *Surfdir* "/sun/sys/")
  "Default file prefix for loading and compiling files.")

(defparameter *default-load-file-suffix ""
  "Default file suffix for loading and compiling files.")

;;; compile-and-load - compile (if necessary) and load a list of files

;; LBG 7/5/92 - If this is set then update-compiled compiles its file arg
;; whether the source is more recent or not.
(defvar *compile-everything*  t)

(defun compile-and-load (filelist &key (prefix *default-load-file-prefix)
				  (suffix *default-load-file-suffix))
  (cond ((null filelist) t)
	(t (load (update-compiled (concatenate 'string prefix (car filelist) suffix)))
	   (compile-and-load (cdr filelist) :prefix prefix :suffix suffix))))


;;; update-compiled - function which determines if compiled version exists and updates
;;;
;;;    SYNTAX: (update-compiled name :file-prefix :file-suffix :file-extension)
;;;
;;;
;;;    ARGS:   name                name of file to compile
;;;           :file-prefix         prefix to append to file (in place of *default-load-file-prefix)
;;;           :file-suffix         suffix to append to file
;;;           :file-extension      extension for file
;;;           :object-extension    extension for compiled file

;; Added explicit filename for the compile so that the same sys
;; directory can have both akcl (with ".o") and lucid (with ".lbin")
;; object file versions. 4/92 lbg

(defparameter *obj-ext*
#+akcl     ".o"
#+lucid     ".sbin"
#+cmu     ".fasl"
)

(defun update-compiled (name &key (file-prefix "") (file-suffix "")
			     (file-extension ".lisp") (object-extension *obj-ext*))
  (let ((lisp-file (concatenate 'string file-prefix name file-suffix file-extension))
	(compiled-file (concatenate 'string file-prefix name
				    file-suffix object-extension)))
    (cond ((not (probe-file lisp-file)) (error-message "File ~A not found." lisp-file))
	  ((or *compile-everything*
	       (needs-updating (probe-file lisp-file) (probe-file compiled-file)))
	   (compile-file (probe-file lisp-file) :output-file compiled-file)))
    (probe-file compiled-file)))


;;; check to see if compiled version exists or if it is older than the source
;;;   if so, needs-updating it true

(defun needs-updating (lisp-file compiled-file)
 (or (not compiled-file)
	 (< (file-write-date (probe-file compiled-file))
		(file-write-date (probe-file lisp-file)))))



;;; LBG 7/5/92 - This just takes advantage of cmucl's undefined warning suppressor wrapper.
#+cmu
(defun compile-system-and-load (filelist &key (prefix *default-load-file-prefix)
					 (suffix *default-load-file-suffix))
  (with-compilation-unit
      (:optimize '(optimize (speed 0)))
    (compile-and-load filelist :prefix prefix :suffix suffix)))


;(with-compilation-unit (:optimize  '(optimize (speed 2)))
; (break))



#-cmu
(defun compile-system-and-load (filelist &key (prefix *default-load-file-prefix)
					 (suffix *default-load-file-suffix))
  (compile-and-load filelist :prefix prefix :suffix suffix))



(export '(compile-system-and-load compile-and-load))



