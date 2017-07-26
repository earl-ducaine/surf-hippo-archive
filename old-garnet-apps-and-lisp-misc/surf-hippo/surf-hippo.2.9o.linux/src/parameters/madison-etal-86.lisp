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

#|
@ARTICLE{Mad-Mal-Nic-86,
	AUTHOR = {Madison, D. V. and Malenka, R. C. and Nicoll, R. A.},
	TITLE = {Phorbol esters block a voltage-sensitive chloride current in hippocampal pyramidal cells},
	JOURNAL = {Nature},
	YEAR = {1986},
	VOLUME = {321},
	PAGES = {695-697},
	MONTH = {June}
}
|#

(channel-type-def
 '(kclv-mad86
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.01)
   (e-rev . -70.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((Cl 1.0)))
   (q10 . 1.0)
   (reference-temp . 27.0)		; probably room temp
   (v-particles . ((KclY-mad86 1)))))

;; exponential inactivation with tau about 300ms at some unknown step, relaxing with tau about 500ms
;; on return to holding potential of -10
(particle-type-def
 '(Kcly-mad86
   (class . :HH-EXT)
   (valence . 4.4)
   (gamma . 0.3)
   (qten . 1.0)
   (base-rate . 0.05)
   (v-half . -51.0)
   (tau-0 . 0.1)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)))
