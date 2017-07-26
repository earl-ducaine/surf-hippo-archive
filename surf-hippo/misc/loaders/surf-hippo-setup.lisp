;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
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
;;;


;;; This file setups Surf-Hippo pathnames, etc. and loads some CMUCL fixes.

;; For possible user/site variables to set search for "$$$$ CUSTOMIZE $$$$"

(defvar *force-all-compile* nil)

(push (read-from-string (format nil ":surf-hippo-V~a" Surf-Hippo-Version-Number)) *features*)
(push :SURF-HIPPO *features*)

#|
(rplaca
 (cdr (member :bugs *herald-items*))
 '("Send bug reports and questions to your local CMU CL maintainer, " terpri
   "or to " 
   "pvaneynd@debian.org" terpri
   "or to " 
   "cmucl-help@cons.org. (prefered)" terpri terpri
   "type (help) for help, (quit) to exit, and (demo) to see the demos" terpri
   terpri
   "Loaded subsystems:" terpri))
|#

;(setf (getf ext:*herald-items* :bugs)
; '("Send bug reports and questions to surf-hippo-bugs@ai.mit.edu or cmucl-help@cons.org."))

(setf (getf ext:*herald-items* :garnet)
      `("    Garnet Version " ,Garnet-Version-Number))
      
(setf (getf ext:*herald-items* :surf-hippo)
      `("    Surf-Hippo Version " ,Surf-Hippo-Version-Number ", " ,Surf-Hippo-Version-Date TERPRI
	"               http://www.cnrs-gif.fr/iaf/iaf9/surf-hippo.html" terpri
	terpri terpri
	"To start the Surf-Hippo menus enter (SURF) at the Lisp prompt." terpri
	"For help, read the User Manual or see doc/old-doc-files/running.doc in the surf-hippo directory." terpri
	"To quit Lisp, enter (QUIT) at the Lisp prompt."
	terpri terpri
	))

(if (> (length Surf-Hippo-Version-Comment) 0)
    (setf
     (getf ext:*herald-items* :surf-hippo-comment)
     `("               " ,Surf-Hippo-Version-Comment)))


(defvar load-cmucl-fixes-p t)
(defvar compile-cmucl-fixes-p nil)
(defvar Surf-Hippo-cmucl-fixes-PathName)
(defvar Surf-Hippo-cmucl-fixes-Src)
(setq Surf-Hippo-cmucl-fixes-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/cmucl-fixes/"))
      Surf-Hippo-cmucl-fixes-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/cmucl-fixes/")))

#+cmu
(progn
  (setf (ext:search-list "cmucl-fixes:")
	(list Surf-Hippo-cmucl-fixes-PathName))
  (setf (ext:search-list "cmucl-fixes-src:")
	(list Surf-Hippo-cmucl-fixes-Src)))

(defparameter Surf-Hippo-cmucl-fixes-Loader
  (merge-pathnames "cmucl-fixes-loader"
		   #+cmu "cmucl-fixes-src:" ;"cmucl-fixes:"
		   #+(not cmu) Surf-Hippo-cmucl-fixes-PathName))

;;; *dont-load-modules-twice* tells whether to re-load modules
;;; if a user loads Surf-Hippo-loader.lisp a second time.
(defparameter *dont-load-modules-twice* nil)


;; Load some cmucl fixes before loading Garnet.
(if load-cmucl-fixes-p
    (if (and *dont-load-modules-twice* (get :Surf-Hippo-modules :cmucl-fixes))
	(format T "~%****** CMUCL-FIXES already loaded *******~%")
	(progn
	  (format T "~% %%%%%%%% Loading CMUCL-FIXES %%%%%%%%~%")
	  (load Surf-Hippo-CMUCL-FIXES-Loader)))
    (format T "~%****** NOT Loading CMUCL-FIXES *******~%"))








