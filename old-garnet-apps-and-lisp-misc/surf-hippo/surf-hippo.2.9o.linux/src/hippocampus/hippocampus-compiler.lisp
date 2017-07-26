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


;; Compiler file for the Surf-Hippo Hippocampus files.

;;; This loader file was adapted from kr-compiler.lisp, part of the
;;; Garnet project at CMU.


#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER" :use '("LISP"))


(dolist (file Surf-Hippo-Hippocampus-Files)
	 (compile-file (merge-pathnames file Surf-Hippo-Hippocampus-Src))
	 #+(or allegro explorer lispworks lucid cmu)
	 (load         (merge-pathnames file Surf-Hippo-Hippocampus-Src)))



#+(or allegro explorer lispworks lucid cmu)
(setf (get :surf-hippo-modules :hippocampus) T)




