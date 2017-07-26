;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


(in-package "SURF-HIPPO")

;; NOT TESTED

;; Yamada, W. M. and Koch, C. and Adams, P. R. 1989
;; Multiple Channels and Calcium Dynamics
;; in Methods in Neuronal Modeling

;; For juxta-membrane compartment 
(buffer-type-def
 `(ca-yamada-89-membrane
   (species . ca)
   (total-conc . 30.0e-3)		; mM
   (k-forward . 1.0e2)			; mM^-1 ms^-1
   (k-backward . 0.1)))


;; For inner shell compartments
(buffer-type-def
 `(ca-yamada-89-inner
   (species . ca)
   (total-conc . 3.0e-3)		; mM
   (k-forward . 1.0e2)			; mM^-1 ms^-1
   (k-backward . 0.1)))

