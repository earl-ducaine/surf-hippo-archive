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


;; GUI Source file: gui-loader.lisp


;;
;;; Loader file for the Surf-Hippo gui files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")

;(defparameter Sys-Version-Number "2.2")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Gui-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Gui-PathName before loading Gui."))

(Defvar Surf-Hippo-Gui-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   "macros"
   "variables"
   "math"
   "strings"
   "sequences"
   "colors"
   "linestyles"
   "windows-hack"
   "print-windows"
   "files"
   "menu-hack"
   "browser"
   "show-image"
   "plot-hack"
   "plot-hack-top"
   "virtual-things"
   "annotation-file"
   "tracer"
   ))


(compile-source-directory Surf-Hippo-gui-Src
			  Surf-Hippo-gui-pathname Surf-Hippo-gui-Files
			  :files-to-force-compile-all '("macros" "math")
			  :enable-compile compile-gui-p)


(setf (get :surf-hippo-modules :gui)  t)


