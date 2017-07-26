;;; -*- Mode: Lisp, Fill, Save; Package: USER -*-
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
;;; This file prepares to compile all the surf-hippo modules.  Of course, you
;;; need to have write priviledges on all the directories where the files
;;; are stored.  (These directories are set in surf-hippo-loader).
;;; 
;;; First load this file: 	surf-hippo-prepare-compile
;;; Then load 			surf-hippo-loader
;;; Then load 			surf-hippo-compiler
;;;
;;; The result will be that all the files will be compiled and loaded (the
;;; initial files need to be loaded before later files can be compiled
;;; anyway).  Note that this process does *NOT* check for compile errors,
;;; that is up to you.
;;;
;;; ** To prevent certain parts from being compiled, first set
;;;      user::compile-XX-p to NIL.  To compile lapidary, set
;;; 	 compile-lapidary-p to T (the default is not to compile it)
;;; ** To have the demos or lapidary be loaded after they are
;;; 	 compiled, set user::load-demos-p and user::load-lapidary-p
;;; 	 to T (the default is to NOT load these after compiling them
;;; ** To override where something is loaded from, set Surf-Hippo-xx-PathName before
;;;      loading this file.
;;;
;;; The controlling variables are:
;;; 
;;;  compile-sys-p           (Default: T   => sys compiled and loaded)
;;;  compile-debug-p        (Default: T   => debug compiled and loaded)
;;;
;;; To override any particular file name place, it is only necessary to
;;; assign the variable name Surf-Hippo-XX-Pathname before this file is loaded
;;; (since they are defined here using defvar, the old name will stay in affect). 
;;;

;;; This loader file was adapted from garnet-prepare-compile.lisp:
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


(defvar compile-sys-p T)
(defvar compile-hippocampus-p T)
(defvar compile-rabbit-p T)
(defvar compile-debug-p nil)

; first, don't load anything, just load surf-hippo-loader to set up file names

(defvar load-sys-p NIL)
(defvar load-hippocampus-p nil)
(defvar load-rabbit-p nil)
(defvar load-debug-p NIL)

(defparameter load-sys-p-copy (if (boundp 'load-sys-p)
				 load-sys-p T))
(defparameter load-hippocampus-p-copy (if (boundp 'load-hippocampus-p)
				 load-hippocampus-p T))
(defparameter load-rabbit-p-copy (if (boundp 'load-rabbit-p)
				 load-rabbit-p T))
(defparameter load-debug-p-copy (if (boundp 'load-debug-p)
				    load-debug-p T))

;; tell surf-hippo-loader to load the binaries from the same place as the
;; source files.

(defvar *Surf-Hippo-Going-To-Compile* nil)

(format T "** Now load surf-hippo-loader, and then load surf-hippo-compiler~%")

