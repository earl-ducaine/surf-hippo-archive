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

;;
;;; Loader file for the Surf-Hippo parameter files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Parameters-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Parameters-PathName before loading Parameters."))

(Defvar Surf-Hippo-Parameters-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list

   "buffers"

   "pumps"
   "axons"
   "conc-ints"

   "syns"
   "light-syns"
   "bernander-etal-91-syns"


   
   "hodgkin-huxley"
   "NEURON-k-chs"
   "hippo-TR1161"			
   "traub91-chs"
   "traub94-chs"
   "warman94-chs"
   "jaffe94-chs"
   "migliore95-chs"
   "lytton-chs"
   "sah-french-etal-na"
   "barnes-hille-cone-chs"
   ;; some markov models
   "kuo-bean-na"
   "vandenberg-bezanilla"



   "k-chs"

   "working-hpc"			; Channels from the Working model described in Cerebral
					; Cortex chapter
   "working-hpc-absolute"

   "working-fs"
   ))


(compile-source-directory Surf-Hippo-parameters-Src
			  Surf-Hippo-parameters-pathname Surf-Hippo-Parameters-Files
			  :enable-compile compile-parameters-p)


(setf (get :surf-hippo-modules :parameters)  t)


