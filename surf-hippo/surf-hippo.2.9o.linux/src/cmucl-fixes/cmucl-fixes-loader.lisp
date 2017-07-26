;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

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

;;
;;; Loader file for the Surf-Hippo cmucl-fixes files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Cmucl-Fixes-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Cmucl-Fixes-PathName before loading Cmucl-Fixes."))

(Defvar Surf-Hippo-Cmucl-Fixes-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list

   ;; a-mickish again, 2/22/94, for lisp crashing on a remote server
   "fd-stream-read-n-bytes"

   ;;; this is a fix to cmucl suggested by Andrew.Mickish@a.gp.cs.cmu.edu
   "fix-1-13-92"

   ;; From CMUCL Event Fix suggested by Andrew Mickish (see Garnet README)
   "serve-event"

   ;; for the mit cmucl environment
   "coerce"

   ;; just a terser output %time function.
   "time-w-new-output"
   
   ;; for solaris
   "run-time"

   "complex-fix"

   ; a simple optimization of the "with hash-value..." construct.
#-cmu18   "hash-looper"

   ; optimize one kind of truncate
   "numbers"

   "18a-fix-merge-types-aux"

#-new-random   "rand-mt19937-small-int"		; Fix to RANDOM from Pierpaolo Bernardi
   
   ))

(compile-source-directory Surf-Hippo-cmucl-Fixes-Src
			  Surf-Hippo-cmucl-Fixes-pathname Surf-Hippo-cmucl-Fixes-Files
			  :enable-compile compile-cmucl-fixes-p)

(setf (get :surf-hippo-modules :cmucl-fixes)  t)

