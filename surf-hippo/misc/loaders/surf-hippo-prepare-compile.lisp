;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-
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
;;; This file prepares to compile all the surf-hippo modules.  Of course, you
;;; need to have write privilege on all the directories where the files
;;; are stored.  (These directories are set in surf-hippo-loader).
;;; 
;;; First load this file: 	surf-hippo-prepare-compile
;;; Then load 			surf-hippo-loader
;;;
;;; The result will be that all the files will be compiled and loaded (the
;;; initial files need to be loaded before later files can be compiled
;;; anyway).  Note that this process does *NOT* check for compile errors,
;;; that is up to you.
;;;
;;; ** To prevent certain parts from being compiled, first set
;;;      user::compile-XX-p to NIL.
;;; ** To override where something is loaded from, set Surf-Hippo-xx-PathName before
;;;      loading this file.
;;;
;;; The controlling variables are:
;;; 
;;;  compile-sys-p           
;;;  compile-parameters-p           
;;;  compile-hippocampus-p 
;;;  compile-rabbit-p 
;;;  compile-roylance-clmath-p
;;;  compile-development-p
;;;
;;; The default for each of these is T => module is compiled and loaded.
;;;
;;;
;;; To override any particular file name place, it is only necessary to
;;; assign the variable name Surf-Hippo-XX-Pathname before this file is loaded
;;; (since they are defined here using defvar, the old name will stay in affect). 
;;;

;;; This loader file was adapted from garnet-prepare-compile.lisp,
;;; part of the Garnet project at CMU.

#|
============================================================
Change log:
============================================================
|#

(defvar compile-cmucl-fixes-p t)
(defvar compile-garnet-fixes-p t)
(defvar compile-gui-p t)
(defvar compile-sys-p t)
(defvar compile-parameters-p t)
(defvar compile-hippocampus-p t)
(defvar compile-rabbit-p t)
(defvar compile-roylance-clmath-p t)
(defvar compile-development-p t)


;; If any of the compile-XX-p flags are nil, let surf-hippo-loader defvar the associated load-XX-p
;; flags.

(if compile-cmucl-fixes-p (defvar load-cmucl-fixes-p t))
(if compile-garnet-fixes-p (defvar load-garnet-fixes-p t))
(if compile-gui-p (defvar load-gui-p t))
(if compile-sys-p (defvar load-sys-p t))
(if compile-parameters-p (defvar load-parameters-p t))
(if compile-hippocampus-p (defvar load-hippocampus-p t))
(if compile-rabbit-p (defvar load-rabbit-p t))
(if compile-roylance-clmath-p (defvar load-roylance-clmath-p t))
(if compile-development-p (defvar load-development-p t))

;; Tell surf-hippo-loader to load the binaries from the same place as the
;; source files.

(defvar *Surf-Hippo-Going-To-Compile* nil)





