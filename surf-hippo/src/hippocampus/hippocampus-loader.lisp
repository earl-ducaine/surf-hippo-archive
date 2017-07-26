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


;; Loader file for the Surf-Hippo Hippocampus files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.



#|
==================================================================
Change log:
==================================================================
|#


(in-package "USER")


(defparameter Hippocampus-Version-Number "1.0")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Hippocampus-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Hippocampus-PathName before loading Hippocampus."))


;; ---- Load Hippocampus files

(Defvar Surf-Hippo-Hippocampus-Files	; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  '(
    "hippo"

    "working-hpc"			; WORKING model in Cerebral Cortex chapter.
    
    "hippos"
    "n120-max-red"

    
    ;; "c12861-ca1-max-red-tree"
    ;; "traub91"
    ;; "traub94"
    
    ))


(compile-source-directory Surf-Hippo-hippocampus-Src
			  Surf-Hippo-hippocampus-pathname Surf-Hippo-Hippocampus-Files
			  :enable-compile compile-hippocampus-p)


(setf (get :surf-hippo-modules :hippocampus)  t)


