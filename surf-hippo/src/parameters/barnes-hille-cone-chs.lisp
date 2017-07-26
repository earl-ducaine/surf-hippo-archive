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


;; Parameters for
#|
@ARTICLE{Bar-Hil-89,
	AUTHOR = {Barnes, S. and Hille, B.},
	TITLE = {Ionic channels of the inner segment of tiger salamander cone photoreceptors},
	JOURNAL = {Journal of General Physiology},
	YEAR = {1989},
	VOLUME = {94},
	PAGES = {719-743},
	MONTH = {October}
}
|#


(in-package "SURF-HIPPO")

(channel-type-def
 '(H-BARNES-HILLE-89
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.003)
   (e-rev . -17.0)
   (use-defined-rev . T)
   (ion-permeabilities . NIL)
   (q10 . 1.0)
   (reference-temp . 32.0)
   (v-particles . ((H-X-BARNES-HILLE-89 1)))))


(particle-type-def
 `(H-X-BARNES-HILLE-89
   (class . :HH-EXT)
   (linear-markov . (4 2))		; N = 4, M = 2
   (valence . -2.0)
   (gamma . 1.0)
   (base-rate . 1.0)
   (v-half . -98.0)
   (tau-0 . 180.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 32.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 32.0)
   (qten . 1.0)))

