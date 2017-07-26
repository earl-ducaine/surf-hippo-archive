;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Surf-Hippo Neuron Simulator                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and has been placed in the public                   ;;;
;;; domain.  If you are using this code or any part of Surf-Hippo,  ;;;
;;; please contact lyle@ai.mit.edu to be put on the mailing list.   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; This file loads all the Surf-Hippo modules.
;;; 
;;; ** To prevent certain parts from being loaded, first set
;;;      user::load-XX-p to NIL.
;;; ** To get some of the parts which are not loaded by default to be loaded,
;;;    set user::load-XX-p to T.
;;; ** If you are a non-MIT user, set Your-Surf-Hippo-Pathname to be your local
;;;    Surf-Hippo directory.
;;; ** To override where something is loaded from, set Surf-Hippo-xx-PathName
;;;    before loading this file and/or Surf-Hippo-xx-src
;;;
;;; The controlling variables are:
;;; 
;;;      load-sys-p           (Default: T   => sys loaded)
;;;      load-hippocampus-p   (Default: T   => hippocampus loaded)
;;;      load-rabbit-p        (Default: T   => rabbit loaded)
;;;      load-debug-p         (Default: T   => debug loaded)
;;;
;;; The first part of this file lists the file names where the various
;;; parts of Surf-Hippo come from.  This will need to be modified for each new
;;; installation of Surf-Hippo.
;;;
;;; To override any particular file name place, it is only necessary to
;;; assign the variable name Surf-Hippo-XX-Pathname before this file is loaded
;;; (since they are defined here using defvar, the old name will stay in
;;; affect).
;;;

;;; This loader file was adapted from garnet-loader.lisp:
;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#|
============================================================
Change log:

============================================================
|#

(in-package "USER" :use '("LISP"))


#+cmu
(progn
  (unless (find-package "SURF-HIPPO")
    (make-package "SURF-HIPPO" :use '("COMMON-LISP-USER" "COMMON-LISP"
				      "LISP" "USER")
		  :nicknames '("SURF"))))

(defparameter Surf-Hippo-Version-Number "1.0.0")
(push :SURF-HIPPO *features*)
(push :SURF-HIPPO-V1.0.0 *features*)

;;; The :SURF-HIPPO-DEBUG option allows many different kinds of run-time checking,
;;; and also loads some extra test code.  After you have debugged your code
;;; and want it to run faster, remove :SURF-HIPPO-DEBUG from the *features* list
;;; and RECOMPILE all of Surf-Hippo and your code.  The result will be smaller and
;;; somewhat faster.
;;; To remove :SURF-HIPPO-DEBUG from the *features* list, either defvar
;;; Surf-Hippo-Surf-Hippo-Debug to NIL before you load the Surf-Hippo-loader, or simply
;;; comment out the next few lines.
(defvar Surf-Hippo-Surf-Hippo-Debug nil)
(if Surf-Hippo-Surf-Hippo-Debug
    (pushnew :Surf-Hippo-debug *features*)
    (setf *features* (delete :Surf-Hippo-debug *features*)))

;;; Surf-Hippo-Version controls where the files are loaded from/
;;; Because this is a defvar, if Surf-Hippo-Version is set before this file is
;;; loaded, its original value will be used.

(defvar Surf-Hippo-Version :mit)
	;; options are:
	;;	:test for the testing version
        ;;      :mit  for version running at MIT
	;;	:external for all other non-MIT versions


(format T "** Loading Surf-Hippo Version ~a from ~s~%" Surf-Hippo-Version-Number Surf-Hippo-Version)

;;; *dont-load-modules-twice* tells whether to re-load modules
;;; if a user loads Surf-Hippo-loader.lisp a second time.
(defparameter *dont-load-modules-twice* t)

;;; load-XX-p control whether the various parts are loaded or not
;;; Because these use defvar, if they are set before this file is
;;; loaded, their original value will be used.

(unless (boundp '*Surf-Hippo-Going-To-Compile*)
  (defvar load-sys-p T)
  (defvar load-hippocampus-p T)
  (defvar load-rabbit-p T)
  (defvar load-debug-p #+Surf-Hippo-debug T #-Surf-Hippo-debug NIL)
)


;;; Insert your pathname of Surf-Hippo into Your-Surf-Hippo-Pathname
;;; For example:
;;; (defvar Your-Surf-Hippo-Pathname "/usr/your-name/Surf-Hippo/")
;;; All the :external pathnames will depend on these two pathnames.

(defvar Your-Surf-Hippo-Pathname "") ;;; SET THIS

(defvar Surf-Hippo-SYS-PathName
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "bin/sys/"))
    (:mit "/home/hc/lyle/surf-hippo/bin/sys/")
    (T (error "No version for SYS"))))

(defvar Surf-Hippo-SYS-Src
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "src/sys/"))
    (:mit "/home/hc/lyle/surf-hippo/src/sys/")
    (T (error "No version for SYS"))))

(defvar Surf-Hippo-Hippocampus-PathName
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "bin/hippocampus/"))
    (:mit "/home/hc/lyle/surf-hippo/bin/hippocampus/")
    (T (error "No version for Hippocampus"))))
(defvar Surf-Hippo-Hippocampus-Src
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "src/hippocampus/"))
    (:mit "/home/hc/lyle/surf-hippo/src/hippocampus/")
    (T (error "No version for Hippocampus"))))

(defvar Surf-Hippo-Rabbit-PathName
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "bin/rabbit/"))
    (:mit "/home/hc/lyle/surf-hippo/bin/rabbit/")
    (T (error "No version for Rabbit"))))
(defvar Surf-Hippo-Rabbit-Src
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "src/rabbit/"))
    (:mit "/home/hc/lyle/surf-hippo/src/rabbit/")
    (T (error "No version for Rabbit"))))

(defvar Surf-Hippo-Debug-PathName
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "bin/debug/"))
    (:mit "/home/hc/lyle/surf-hippo/bin/debug/")
    (T (error "No version for Debug"))))
(defvar Surf-Hippo-Debug-Src
  (case Surf-Hippo-Version
    (:external (concatenate 'string Your-Surf-Hippo-Pathname "src/debug/"))
    (:mit "/home/hc/lyle/surf-hippo/src/debug/")
    (T (error "No version for Debug"))))

;;;----------------------------------------------------------

;;; When compiling, the binaries will be in the same directories as the
;;; source files, so make all the path names be the same
;;;
;;; After compilation is finished, the user should move all the binaries
;;; into their own directories, as specified the the pathnames above.
(defvar *Surf-Hippo-Going-To-Compile*)

(when (and (boundp '*Surf-Hippo-Going-To-Compile*)
	   *Surf-Hippo-Going-To-Compile*)
  (setf Surf-Hippo-SYS-Pathname Surf-Hippo-SYS-Src)
  (setf Surf-Hippo-Hippocampus-Pathname Surf-Hippo-Hippocampus-Src)
  (setf Surf-Hippo-Rabbit-Pathname Surf-Hippo-Rabbit-Src)
  (setf Surf-Hippo-Debug-Pathname Surf-Hippo-Debug-Src)
  )


;; some misc. stuff -  lbg

#+cmu
(defun prompt-and-read (type prompt)
  (declare (ignore type))
  (print prompt)
  (read))


;; cmucl needs this apparently - this was taken from clx.l lbg 4-25-92
#+cmu
(deftype boolean () '(or null (not null)))

(defun display-message (m) (format t (concatenate 'string m "~%")))

(defun error-message (m &rest args)
  (format *error-output* (concatenate 'string m "~%") args))

(defparameter *Surfdir*
  (cdr (assoc :SURFHOME lisp::*environment-list*)))


;;; make this accessible from surf package. 4-25-92 lbg
(export '(*Surfdir* boolean  display-message error-message  prompt-and-read))

(pushnew :neuron *features*)		; Include neuron model
(pushnew :off-diag *features*)		; Use tri-diagonal matrices.
(pushnew :sun *features*)		; You are on a sun

(defun setsurf () 
  (in-package "SURF"))


;;;----------------------------------------------------------

;;; If at cmu, then set up the search lists
#+cmu
(progn
  (setf (ext:search-list "sys:")
	(list Surf-Hippo-SYS-PathName))
  (setf (ext:search-list "sys-src:")
	(list Surf-Hippo-SYS-Src))



  (setf (ext:search-list "hippocampus:")
	(list Surf-Hippo-Hippocampus-PathName))
  (setf (ext:search-list "hippocampus-src:")
	(list Surf-Hippo-Hippocampus-Src))

  (setf (ext:search-list "rabbit:")
	(list Surf-Hippo-Rabbit-PathName))
  (setf (ext:search-list "rabbit-src:")
	(list Surf-Hippo-Rabbit-Src))
  
  (setf (ext:search-list "debug:")
	(list Surf-Hippo-Debug-PathName))
  (setf (ext:search-list "debug-src:")
	(list Surf-Hippo-Debug-Src))
)

(defparameter Surf-Hippo-SYS-Loader
  (merge-pathnames "sys-loader"
		   #+cmu "sys:"
		   #+(not cmu) Surf-Hippo-SYS-PathName))


(defparameter Surf-Hippo-Hippocampus-Loader
  (merge-pathnames "hippocampus-loader"
		   #+cmu "hippocampus:"
		   #+(not cmu) Surf-Hippo-Hippocampus-PathName))

(defparameter Surf-Hippo-Rabbit-Loader
  (merge-pathnames "rabbit-loader"
		   #+cmu "rabbit:"
		   #+(not cmu) Surf-Hippo-Rabbit-PathName))



(defparameter Surf-Hippo-Debug-Loader
  (merge-pathnames "debug-loader"
		   #+cmu "debug:"
		   #+(not cmu) Surf-Hippo-Debug-PathName))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(format t "...Loading Surf-Hippo ...~%")
(setf *load-verbose* t)

(if load-sys-p
    (if (and *dont-load-modules-twice* (get :Surf-Hippo-modules :sys))
	(format T "~%****** SYS already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading SYS %%%%%%%%~%")
          (load Surf-Hippo-SYS-Loader)))
    (format T "~%****** NOT Loading SYS *******~%"))


(if load-hippocampus-p
    (if (and *dont-load-modules-twice* (get :Surf-Hippo-modules :hippocampus))
	(format T "~%****** Hippocampus programs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Hippocampus programs %%%%%%%%~%")
          (load Surf-Hippo-Hippocampus-Loader)))
    (format T "~%****** NOT Loading Hippocampus Files *******
** To load hippocampus programs, execute (load Surf-Hippo-Hippocampus-Loader)~%"))

(if load-rabbit-p
    (if (and *dont-load-modules-twice* (get :Surf-Hippo-modules :rabbit))
	(format T "~%****** Rabbit programs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Rabbit programs %%%%%%%%~%")
          (load Surf-Hippo-Rabbit-Loader)))
    (format T "~%****** NOT Loading Rabbit Files *******
** To load rabbit programs, execute (load Surf-Hippo-Rabbit-Loader)~%"))


(if load-debug-p
    (if (and *dont-load-modules-twice* (get :Surf-Hippo-modules :debug))
	(format T "~%****** Debugging programs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Debugging programs %%%%%%%%~%")
          (load Surf-Hippo-Debug-Loader)))
    (format T "~%****** NOT Loading Debug Files *******
** To load Debug programs, execute (load Surf-Hippo-Debug-Loader)~%"))

;--------------------------------------------------------------------
(defun user::Surf-Hippo-Load (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (prefix (cond
			 ((string= head "sys") Surf-Hippo-SYS-PathName)
			 ((string= head "hippocampus") Surf-Hippo-Hippocampus-PathName)
			 ((string= head "rabbit") Surf-Hippo-Rabbit-PathName)
			 ((string= head "debug") Surf-Hippo-Debug-PathName)
			 (t (error "Bad prefix ~%" head))))
	       (finalname (merge-pathnames tail prefix)))
	  (format T "Loading ~s~%" finalname)
	  (load finalname))
	;; else no colon, load regular
	(progn
	  (format T "NO COLON, Loading ~s~%" filename)
	  (load filename)))))

;;; 
;;; This function will compile your Surf-Hippo files while keeping the
;;; sources and binaries separated.  If you want to just compile one
;;; file from Surf-Hippo, like the gadget file gauge.lisp, then you could
;;; use this function to compile the source file and automatically
;;; save the binary file in the bin directory.
;;;
;;; Example:
;;;    (Surf-Hippo-compile "sys:gauge") 
;;;    Takes the source file from Surf-Hippo-Sys-Src, compiles it, and
;;;    saves the binary file in Surf-Hippo-Sys-Pathname (the binary
;;;    sys directory).
;;;
(defvar *compiler-extension*
  #+(and cmu sparc)       ".sparcf"
  #+(and cmu (not sparc)) ".fasl")

(defun user::Surf-Hippo-Compile (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (src-prefix (cond
		 ((string= head "sys") Surf-Hippo-SYS-Src)
		 ((string= head "hippocampus") Surf-Hippo-Hippocampus-Src)
		 ((string= head "rabbit") Surf-Hippo-Rabbit-Src)
		 ((string= head "debug") Surf-Hippo-Debug-Src)
		 (t (error (concatenate 'string "Bad prefix " head)))))
	       (bin-prefix (cond
		 ((string= head "sys") Surf-Hippo-SYS-PathName)
		 ((string= head "hippocampus") Surf-Hippo-Hippocampus-PathName)
		 ((string= head "rabbit") Surf-Hippo-Rabbit-PathName)
		 (t (error (concatenate 'string "Bad prefix " head)))))
	       (src-finalname (merge-pathnames
				(concatenate 'string tail ".lisp")
				src-prefix))
	       (bin-finalname (merge-pathnames
			        (concatenate 'string tail *compiler-extension*)
				bin-prefix)))
	  (format T "Compiling ~s~%" src-finalname)
	  (format T "for output to ~s~%" bin-finalname)
	  (compile-file src-finalname :output-file bin-finalname))
	;; else no colon, abort
	(error "NO COLON, aborting compile"))))


(format t "~%... Surf-Hippo Load Complete ...~%")



