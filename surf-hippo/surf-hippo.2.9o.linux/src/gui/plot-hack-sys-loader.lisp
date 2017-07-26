;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
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


;; GUI Source file: plot-hack-sys-loader.lisp


;;
;;; Loader file for the Surf-Hippo Plot Hack files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")

(defparameter Sys-Version-Number "2.0")

(format t "Loading Surf-Hippo Plot Hack...~%")

;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Plot-Hack-PathName)
  (error "Load 'Plot-Hack-Loader' first to set Surf-Hippo-Plot-Hack-PathName before loading Plot Hack."))

(Defvar Surf-Hippo-Plot-Hack-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list

   "windows-hack"
   "menu-hack"
    "plot-hack"

    ))


(dolist (file Surf-Hippo-Plot-Hack-Files)
  (if compile-plot-hack-p
      (progn
	(compile-file (merge-pathnames file Surf-Hippo-Plot-Hack-Src))
	(load         (merge-pathnames file Surf-Hippo-Plot-Hack-Src)))
      (load (merge-pathnames file 
			     #+cmu "sys:"
			     #+(not cmu) Surf-Hippo-plot-hack-PathName
			     )
	    :verbose T)))


(setf (get :surf-hippo-modules :sys)  t)

(format t "...Done Plot Hack.~%")

