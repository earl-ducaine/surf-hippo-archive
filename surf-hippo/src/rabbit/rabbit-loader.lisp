;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Loader file for the Surf-Hippo Rabbit files.

;;; This loader file was adapted from kr-loader.lisp, part of the
;;; Garnet project at CMU.


#|
==================================================================
Change log:
==================================================================
|#


(in-package "USER")


(defparameter Rabbit-Version-Number "1.0")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Rabbit-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Rabbit-PathName before loading Rabbit."))


;; ---- Load Rabbit files

(Defvar Surf-Hippo-Rabbit-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  '(
    "star-amacrine"
    "star-amacrine-functions"
    "network"
    ))


(compile-source-directory Surf-Hippo-rabbit-Src
			  Surf-Hippo-rabbit-pathname Surf-Hippo-Rabbit-Files
			  :enable-compile compile-rabbit-p)


(setf (get :surf-hippo-modules :rabbit)  t)


